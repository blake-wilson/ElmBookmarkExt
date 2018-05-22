port module Main exposing (..)

import Http
import Html exposing (Attribute, program, section, header, div, ul, li, i, a, span, input, button, text)
import Html.Attributes exposing (href, class, type_, target, attribute, alt, title, checked, disabled)
import Html.Events exposing (onClick, on, targetChecked)
import Json.Decode as Json
import Json.Encode exposing (list, string)
import AnimationFrame
import Time
import Dict exposing (Dict)
import Svg exposing (svg, path)
import Svg.Attributes exposing (viewBox, fill, stroke, d)

backupEndpoint : String
backupEndpoint =
    "https://ly4uzc77fh.execute-api.us-west-2.amazonaws.com/beta/"

importPath : String
importPath =
    "https://s3-us-west-2.amazonaws.com/yellowpapersun-bookmarks/"

type alias Model =
    { bookmarks : Tree BookmarkNode
    , bookmarkIndex : Dict String TreePath -- Path of node ID to location in tree
    , rerender : Bool
    , selectedCount : Int
    , checkedNodes : Dict String Bool -- Set of selected node IDs
    }

(!!) : List a -> Int -> Maybe a
(!!) xs n  = List.head (List.drop n xs)

type alias TreePath = List Int

type Tree a
    = Empty
    | Node a (List (Tree a))

isEmpty : Tree a -> Bool
isEmpty tree =
    case tree of
        Empty ->
            True
        _ ->
            False

emptyChildren : Tree a -> Bool
emptyChildren tree =
    case tree of
        Empty ->
            True
        Node v [] ->
            True
        Node v lst ->
            List.all (isEmpty) lst

type alias BookmarkNode =
  { url : Maybe String
  , title : Maybe String
  , collapsed : Bool
  , id : String
  , backupLink : Maybe String
  , loading : Bool
  , checked : Bool
  }

setBackupLink : Maybe String -> BookmarkNode -> BookmarkNode
setBackupLink link node =
    { node | backupLink = link }

setChecked : Bool -> BookmarkNode -> BookmarkNode
setChecked checked node =
    { node | checked = checked }

setLoading : Bool -> BookmarkNode -> BookmarkNode
setLoading loading node =
    { node | loading = loading }

setCollapsed : Bool -> BookmarkNode -> BookmarkNode
setCollapsed collapsed node =
    { node | collapsed = collapsed }

type alias IDNode =
    { id : String }

map : (a -> b) -> Tree a -> Tree b
map f tree =
    case tree of
      Empty -> Empty
      Node v lst ->
          Node (f v) (List.map (\n -> (map f n) ) lst)

-- foldBranch : (List b -> b)
-- foldLeaf : a -> b

foldTree : (List b -> b) -> (a -> b) -> (Tree a) -> b-> b
foldTree foldBranch foldLeaf tree init =
    case tree of
        Empty ->
            init
        Node v lst ->
            case lst of
                [] ->
                    foldLeaf v
                _ ->
                    foldBranch ([foldLeaf v] ++ List.map (\t -> foldTree foldBranch foldLeaf t init) lst)

branchToDict : (List (Dict String TreePath)) -> Dict String TreePath
branchToDict lst =
    List.foldl (\acc d -> Dict.union d acc) Dict.empty lst

leafAccum : BookmarkNode -> Dict String TreePath
leafAccum n =
    Dict.singleton ("test" ++ n.id) [1,2,3]

branchToDict2 : (List (Dict String TreePath, Tree BookmarkNode)) -> ((Dict String TreePath), Tree BookmarkNode)
branchToDict2 lst =
    let nodes =
        Tuple.first (List.unzip lst)
        tree = Tuple.second (Maybe.withDefault (Dict.empty, Empty) <| List.head lst)
    in
    (branchToDict nodes, tree)

leafAccum2 : (BookmarkNode, Tree BookmarkNode) -> (Dict String TreePath, Tree BookmarkNode)
leafAccum2 (n, tree) =
    (Dict.singleton n.id (getNodePath tree n.id [] 0), tree)

-- leafToDict : (BookmarkNode -> Dict String TreePath)
-- leafToDict n =
--     Dict.singleton n.id (getNodePath n n.id [] 0)

getNodeAtPath : Tree BookmarkNode -> TreePath -> Tree BookmarkNode
getNodeAtPath tree path =
    case path of
        x::xs ->
            case tree of
                Empty ->
                    Empty
                Node v lst ->
                    case lst !! x of
                        Nothing ->
                            Empty
                        Just n ->
                            getNodeAtPath n xs
        [] ->
            tree


getNodePath : Tree BookmarkNode -> String -> TreePath -> Int -> TreePath
getNodePath tree id path index =
    case tree of
        Empty ->
            []
        Node v children ->
            if (v.id == id) then
                path
            else
                List.concat (List.indexedMap (\idx n -> getNodePath n id (path ++ [idx] ) idx) children)

-- updateNode replaces the node at the given tree path with the given node-updating function
updateNode : Tree BookmarkNode -> TreePath -> (BookmarkNode -> BookmarkNode) -> Tree BookmarkNode
updateNode tree path updateF =
    case tree of
        Empty ->
            Empty
        Node v children ->
            case path of
                [] ->
                    Node (updateF v) children
                x::[] ->
                    Node v (List.indexedMap (\idx cn ->
                        case cn of
                            Empty ->
                                Empty
                            Node value lst ->
                                if idx == x then (Node (updateF value) lst) else cn
                        ) children)
                x::xs ->
                    Node v (List.indexedMap (\idx t ->
                        if idx == x then
                            updateNode t xs updateF
                        else
                            t) children)

indexBookmarks : Tree BookmarkNode -> Dict String TreePath
indexBookmarks t =
    let mappedTree =
        map (\n -> (n, t)) t
        (result, _) = foldTree branchToDict2 leafAccum2 mappedTree (Dict.empty, t)
    in
        result
        -- fold (\n -> Dict.insert n.id (getNodePath t n.id [] 0) d ) Dict.empty t


type Msg =
    HandleBookmarks (Result String (Tree BookmarkNode))
    | HandleLinks (Result String (List (String, String)))
    | OpenTab String
    | Backup
    | BackupResult String (Result Http.Error String)
    | Tick Time.Time
    | ToggleExpand String
    | BoxChecked String

onBookmarksClicked : msg -> Attribute msg
onBookmarksClicked message =
    on "click" (Json.succeed message)

onFollowLink : msg -> Attribute msg
onFollowLink url =
    on "click" (Json.succeed url)

view model =
    let bookmarksSelected =
        not <| Dict.isEmpty (Dict.filter (\_ v -> v) model.checkedNodes)
    in
        div [] [
            div [] [
                header [ class "mdc-top-app-bar" ] [
                    div [class "mdc-top-app-bar__row" ] [
                        section [ class "mdc-top-app-bar__section mdc-top-app-bar__section--align-start" ] [
                            a [ href "#",  class "material-icons mdc-top-app-bar__navigation-icon" ] [text "menu" ]
                            , span [ class "mdc-top-app-bar__title" ] [text "Bookmarks" ]
                        ]
                        , if bookmarksSelected then
                            div [] [
                                -- show bookmark operations
                                section [ class "mdc-top-app-bar__section mdc-top-app-bar__section--align-end", attribute "role" "toolbar" ] [
                                    span [ class "material-icons mdc-top-app-bar__action-item"
                                    , attribute "aria-label" "Delete", title "Delete selected bookmark archives"
                                    ] [ text "delete" ]
                                ]
                                , section [ class "mdc-top-app-bar__section mdc-top-app-bar__section--align-end", attribute "role" "toolbar" ] [
                                    span [ class "material-icons mdc-top-app-bar__action-item"
                                    , onClick Backup
                                    , attribute "aria-label" "Backup", title "Delete selected bookmark archives"
                                    ] [ text "backup" ]
                                ]
                            ]
                        else
                            text ""
                    ]
                ]
            ]
            , div [ class "mdc-top-app-bar--fixed-adjust" ] [
                renderNode model model.bookmarks
            ]
    ]

-- getS3URL returns the s3 URL for the given s3 key
getS3URL : String -> String
getS3URL s3Key =
    importPath ++ s3Key

backupIcon : Html.Html Msg
backupIcon =
    i [ class "backup-icon material-icons mdc-button__icon" ] [ text "backup" ]

renderNode : Model -> Tree BookmarkNode -> Html.Html Msg
renderNode model node =
    case node of
    Empty ->
        div [] []
    Node v lst ->
        let icon =
            i [ class "toggle-btn material-icons mdc-icon-toggle"
                 , attribute "role" "button"
                 , attribute "data-toggle-on" """{"label": "Expand", "content": "arrow_right"}"""
                 , attribute "data-toggle-off" """{"label": "Collapse", "content": "arrow_drop_down"}"""
                 , attribute "node-id" v.id ] []
            title = Maybe.withDefault "No Title" v.title
            isParent = not <| emptyChildren node
            entryClass = if isParent then "parent-entry" else "leaf-entry"
            entry = case v.url of
                Just url ->
                    a [ href url, target "_blank" ]
                    [ text <| title ]
                _ ->
                    span [ class entryClass ] [ text title ]
        in
                    div [ class "tree-element" ]
                        [ if isParent
                            then
                                div [] [
                                    icon
                                    , i [ class "material-icons tree-folder" ] [text "folder"]
                                    , entry
                                ]
                        else
                            li [ class "mdc-list-item bookmark-item" ] [
                                span [ class "mdc-list-item__text" ] [
                                    entry
                                ]
                                , span [ class "mdc-list-item__meta" ] [
                                    if v.loading then
                                        div [ attribute "role" "progressbar", class "mdc-linear-progress mdc-linear-progress--indeterminate" ]
                                            [ div [ class "mdc-linear-progress__buffering-dots" ] []
                                            , div [ class "mdc-linear-progress__buffer" ] []
                                            , div [ class "mdc-linear-progress__bar mdc-linear-progress__primary-bar" ] [
                                                span [ class "mdc-linear-progress__bar-inner" ] []
                                            ]
                                            , div [ class "mdc-linear-progress__bar mdc-linear-progress__secondary-bar" ] [
                                                span [ class "mdc-linear-progress__bar-inner" ] []
                                            ]
                                            ]
                                    else
                                    --     text ""
                                    -- ,
                                    case v.backupLink of
                                        Nothing ->
                                            div [] [
                                                -- span [] [text "Not backed up"]
                                                div [ class "mdc-checkbox" ] [
                                                    input [ type_ "checkbox", class "mdc-checkbox__native-control"
                                                    , checked v.checked, onClick (BoxChecked v.id) ] []
                                                    , div [ class "mdc-checkbox__background" ] [
                                                            svg [ Svg.Attributes.class "mdc-checkbox__checkmark", viewBox "0 0 24 24"] [
                                                                    path [ Svg.Attributes.class "mdc-checkbox__checkmark-path", fill "none",
                                                                        stroke "white", d "M1.73,12.91 8.1,19.28 22.79,4.59"
                                                                    ] []
                                                            ]
                                                            , div [ class "mdc-checkbox__mixedmark" ] []
                                                            ]
                                                    ]
                                            ]
                                            -- button [ class "ripple-btn mdc-button__icon mdc-button"
                                            --         , disabled (if v.loading then True else False)
                                            --         , onClick <| Backup v.id (Maybe.withDefault "" v.url) ] [
                                            --     backupIcon
                                            -- ]
                                        Just s3Key ->
                                            div [] [
                                                a [href <| getS3URL s3Key, target "_blank"] [text "Open backup"]
                                                , div [ class "mdc-checkbox" ] [
                                                  input [ type_ "checkbox", class "mdc-checkbox__native-control"
                                                  , checked v.checked, onClick (BoxChecked v.id) ] []
                                                , div [ class "mdc-checkbox__background" ] [
                                                        svg [ Svg.Attributes.class "mdc-checkbox__checkmark", viewBox "0 0 24 24"] [
                                                                path [ Svg.Attributes.class "mdc-checkbox__checkmark-path", fill "none",
                                                                       stroke "white", d "M1.73,12.91 8.1,19.28 22.79,4.59"
                                                                ] []
                                                        ]
                                                        , div [ class "mdc-checkbox__mixedmark" ] []
                                                        ]
                                                  ]
                                            ]
                                ]
                            ]
                            , div [ attribute "style" <| if v.collapsed then """display: none""" else "display: block" ] [
                            (ul [ class "mdc-list mdc-list--dense" ] <| List.map (\c -> renderNode model c) lst)
                            ]
                        ]

backupAddress : String -> Http.Request String
backupAddress url =
    Http.request
        {
            method = "POST"
            , headers = []
            , url = backupEndpoint
            , body = Http.stringBody "application/json" ("""{"url":""" ++ "\"" ++ url ++ "\"}")
            , expect = Http.expectString
            , timeout = Nothing
            , withCredentials = False
        }

main : Program Never Model Msg
main = program
  { init = init,
    view = view,
    update = update,
    subscriptions = subscriptions
  }

-- subscriptions

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ handleBookmarks (decodeBookmarks >> HandleBookmarks)
              , handleLinks (decodeLink >> HandleLinks)
              , AnimationFrame.times Tick
              , toggleExpand ToggleExpand ]

-- withDefault for Decoder
withDefault : a -> Json.Decoder a -> Json.Decoder a
withDefault default decoder =
    Json.oneOf
        [ decoder
        , Json.succeed default
        ]

bookmark : Json.Decoder (Tree BookmarkNode)
bookmark =
    Json.map2 Node
        (Json.map7 BookmarkNode
                (Json.maybe (Json.field "url" Json.string))
                (Json.maybe (Json.field "title" Json.string))
                (Json.succeed False)
                (Json.field "id" Json.string)
                (Json.maybe (Json.field "backupLink" Json.string))
                (Json.succeed False)
                (Json.succeed False)
        )
        (withDefault [Empty] (Json.field "children" (Json.list (Json.lazy (\_ -> bookmark)))))


decodeBackup : Json.Decoder String
decodeBackup =
    Json.field "body" Json.string

decodeBookmarks : Json.Value -> Result String (Tree BookmarkNode)
decodeBookmarks =
    let _ = Debug.log "decoding" "children" in
        Json.decodeValue bookmark

decodeLink : Json.Value -> Result String (List (String, String))
decodeLink =
    Json.decodeValue linkDecoder

linkDecoder : Json.Decoder (List (String, String))
linkDecoder =
    Json.keyValuePairs Json.string

init : (Model, Cmd Msg)
init =
  (
    { bookmarks = Empty
    , rerender = False
    , bookmarkIndex = Dict.empty
    , selectedCount = 0
    , checkedNodes = Dict.empty
    }
  , Cmd.none
  )

port backup : (String, String) -> Cmd msg

port openTab : String -> Cmd msg

port reRender : String -> Cmd msg

port handleBookmarks : (Json.Value -> msg) -> Sub msg

port handleLinks : (Json.Value -> msg) -> Sub msg

port toggleExpand : (String -> msg) -> Sub msg


getNode : Tree BookmarkNode -> Dict String TreePath -> String -> Tree BookmarkNode
getNode tree pathDict id =
    let
        path = (Maybe.withDefault [] <| Dict.get id pathDict)
    in
        getNodeAtPath tree path


-- setProps : Tree BookmarkNode -> List String -> (BookmarkNode -> UpdateProps) -> Tree BookmarkNode
-- setProps model toUpdate updateF =
--     List.map (\id ->
--     let
--         path = (Maybe.withDefault [] <| Dict.get id model.bookmarkIndex)
--         node = case path of
--             [] -> Empty
--             _ -> (getNodeAtPath model.bookmarks path)
--                     updateProps =
--         case node of
--             Empty ->
--                 defaultProps
--             Node v lst ->
--                 let props = currentProps v in
--                     { props | updating = True }
--     in
--         case node of
--             Empty ->
--                 Cmd.none
--             Node v _ ->
--                 updateF v
--     ) ids
--     in
--         { model | bookmarks = }

-- TODO: refactor the map list iteration in favor of id-indexed Dict
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OpenTab url ->
        let _ = Debug.log "opening tab" url in
        (model, openTab url )
    HandleBookmarks (Ok bookmarks) ->
        ({model | bookmarks = bookmarks
         , bookmarkIndex = indexBookmarks bookmarks
         , rerender = True}, Cmd.none)
    HandleBookmarks (Err err) ->
        let _ = Debug.log "Uh oh" err in
            model ! []
    HandleLinks (Ok [(a,b)]) ->
        let
            link = (Just b)
            path = (Maybe.withDefault [] <| Dict.get a model.bookmarkIndex)
        in
            { model | bookmarks = updateNode model.bookmarks path (\n ->
                    n
                    |> setLoading False
                    |> setBackupLink link
                ) } ! []
    HandleLinks (Ok _) ->
        model ! [] -- should be unreachable
    HandleLinks (Err err) ->
        let _ = Debug.log "Error handling links" err in
            model ! []
    ToggleExpand id ->
        let
            path = (Maybe.withDefault [] <| Dict.get id model.bookmarkIndex)
        in
            { model | bookmarks = updateNode model.bookmarks path (\n -> setCollapsed (not n.collapsed) n) } ! []
    BoxChecked id ->
        let
            _ = Debug.log "checked boxes" model.checkedNodes
            path = (Maybe.withDefault [] <| Dict.get id model.bookmarkIndex)
            _ = Debug.log "Path is" path
            node = case path of
                [] -> Empty
                _ -> (getNodeAtPath model.bookmarks path)
            _ = Debug.log "Toggling Node " node
        in
            case node of
                Empty ->
                    model ! []
                Node v lst ->
                    let checkedNodes =
                        Dict.insert id (not v.checked) model.checkedNodes
                    in
                        {model | bookmarks = updateNode model.bookmarks path (\n -> setChecked (not n.checked) n)
                        , selectedCount = model.selectedCount + 1, checkedNodes = checkedNodes } ! []

    Backup ->
        let ids = Dict.filter (\_ v -> v) model.checkedNodes
        in
            ( { model | bookmarks = map (\n ->
                if Dict.member n.id ids then { n | loading = True } else n
                ) model.bookmarks }
            ,
            Cmd.batch (
                List.map (\id ->
                    case getNode model.bookmarks model.bookmarkIndex id of
                        Empty ->
                            Cmd.none
                        Node v _ ->
                            case v.url of
                                Just url ->
                                    let _ = Debug.log "backing up " url in
                                    Http.send (BackupResult v.id) <| backupAddress url
                                _ ->
                                    Cmd.none
                    ) (Dict.keys ids)
            )
        )
    BackupResult id (Result.Ok imgKey) ->
        ( model
        , Cmd.batch [
                backup (id, imgKey)
            ])
    BackupResult id (Err err) ->
        let _ = Debug.log "Error backing up bookmark" err in
            (model, Cmd.none)
    Tick t ->
        if model.rerender then
            ({ model | rerender = False }, reRender "")
        else
            model ! []
