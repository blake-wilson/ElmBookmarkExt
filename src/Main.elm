port module Main exposing (..)

import Http
import Html exposing (Attribute, program, div, ul, li, i, a, span, input, button, text)
import Html.Attributes exposing (href, class, type_, target, attribute)
import Html.Events exposing (onClick, on)
import Json.Decode as Json
import Json.Encode exposing (list, string)
import AnimationFrame
import Time

backupEndpoint : String
backupEndpoint = "https://ly4uzc77fh.execute-api.us-west-2.amazonaws.com/beta/"

importPath : String
importPath =
    "https://s3-us-west-2.amazonaws.com/yellowpapersun-bookmarks/"

type alias Model =
    { bookmarks : Tree BookmarkNode
    , rerender : Bool
    }

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
  }

map : (a -> b) -> Tree a -> Tree b
map f tree =
    case tree of
      Empty -> Empty
      Node v lst ->
          Node (f v) (List.map (\n -> (map f n) ) lst)

type Msg =
    HandleBookmarks (Result String (Tree BookmarkNode))
    | CollapseNode (String)
    | OpenTab String
    | Backup String
    | BackupResult (Result Http.Error String)
    | Tick Time.Time
    | ToggleExpand String

onBookmarksClicked : msg -> Attribute msg
onBookmarksClicked message =
    on "click" (Json.succeed message)

onFollowLink : msg -> Attribute msg
onFollowLink url =
    on "click" (Json.succeed url)

view model =
    div []
    [ renderNode model.bookmarks ]


backupIcon : Html.Html Msg
backupIcon =
    i [ class "material-icons mdc-button__icon" ] [ text "backup" ]

renderNode : Tree BookmarkNode -> Html.Html Msg
renderNode node =
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
                    a [href url, target "_blank", onFollowLink (OpenTab <| url) ]
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
                                    button [ class "mdc_button", onClick <| Backup (Maybe.withDefault "" v.url) ] [
                                        backupIcon
                                    ]
                                ]
                            ]
                            , div [ attribute "style" <| if v.collapsed then """display: none""" else "display: block" ] [
                            (ul [ class "mdc-list mdc-list--dense" ] <| List.map (\c -> renderNode c) lst)
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
        (Json.map4 BookmarkNode
                (Json.maybe (Json.field "url" Json.string))
                (Json.maybe (Json.field "title" Json.string))
                (Json.succeed False)
                (Json.field "id" Json.string)
        )
        (withDefault [Empty] (Json.field "children" (Json.list (Json.lazy (\_ -> bookmark)))))


decodeBackup : Json.Decoder String
decodeBackup =
    Json.field "body" Json.string

decodeBookmarks : Json.Value -> Result String (Tree BookmarkNode)
decodeBookmarks =
    let _ = Debug.log "decoding" "children" in
        Json.decodeValue bookmark

init : (Model, Cmd Msg)
init =
  (
    { bookmarks = Empty
    , rerender = False
    }
  , Cmd.none
  )

port backup : String -> Cmd msg

port openTab : String -> Cmd msg

port getBookmarks : String -> Cmd msg

port reRender : String -> Cmd msg

port handleBookmarks : (Json.Value -> msg) -> Sub msg

port toggleExpand : (String -> msg) -> Sub msg

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    OpenTab url ->
        let _ = Debug.log "opening tab" url in
        (model, openTab url )
    HandleBookmarks (Ok bookmarks) ->
        ({model | bookmarks = bookmarks, rerender = True}, Cmd.none)
    HandleBookmarks (Err err) ->
        let _ = Debug.log "Uh oh" err in
            (model, Cmd.none)
    ToggleExpand id ->
        let _ = (Debug.log "toggling" id) in
        ({ model | bookmarks = map (\n -> (if n.id == id then {n | collapsed = not n.collapsed} else n ) ) model.bookmarks }, Cmd.none)
    CollapseNode id ->
        ({ model | bookmarks = map (\n -> (if n.id == id then {n | collapsed = not n.collapsed} else n ) ) model.bookmarks }, Cmd.none)
    Backup url ->
        ( model, Http.send BackupResult <| backupAddress url )
    BackupResult (Result.Ok imgKey) ->
        (model, openTab <| importPath ++ imgKey)
    BackupResult (Err err) ->
        let _ = Debug.log "Error backing up bookmark" err in
            (model, Cmd.none)
    Tick t ->
        if model.rerender then
            ({ model | rerender = False }, reRender "")
        else
            model ! []
