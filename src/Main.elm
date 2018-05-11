port module Main exposing (..)

import Html exposing (Attribute, program, div, ul, i, a, span, input, button, text)
import Html.Attributes exposing (href, class, type_, attribute)
import Html.Events exposing (onClick, on)
import Json.Decode as Json
import AnimationFrame
import Time

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

renderNode : Tree BookmarkNode -> Html.Html Msg
renderNode node =
    case node of
    Empty ->
        div [] []
    Node v lst ->
        let icon =
            i [class "toggle-btn material-icons mdc-icon-toggle"
                 , attribute "role" "button"
                 , attribute "data-toggle-on" """{"label": "Expand", "content": "arrow_right"}"""
                 , attribute "data-toggle-off" """{"label": "Collapse", "content": "arrow_drop_down"}"""
                 , attribute "node-id" v.id ] []
            title = Maybe.withDefault "No Title" v.title
            isParent = not <| emptyChildren node
            entryClass = if isParent then "parent-entry" else "leaf-entry"
            entry = case v.url of
                Just url ->
                    a [href <| "#", onFollowLink (OpenTab <| url) ] [ text <| title ]
                _ ->
                    text <| title
        in
                div []
                    [ if isParent
                        then
                            div [] [
                                i [ class "material-icons toggle-btn" ] [text "folder"]
                                , icon
                            ]
                      else
                        div [] []
                    , span [ class entryClass ] [ entry ]
                    , div [ attribute "style" <| if v.collapsed then """display: none""" else "display: block" ] [
                          (ul [] <| List.map (\c -> renderNode c) lst)
                        ]
                    ]

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
        let _ = Debug.log "bookmarks" <| toString bookmarks in
            ({model | bookmarks = bookmarks, rerender = True}, Cmd.none)
    HandleBookmarks (Err err) ->
        let _ = Debug.log "Uh oh" err in
            (model, Cmd.none)
    ToggleExpand id ->
        let _ = (Debug.log "toggling" id) in
        ({ model | bookmarks = map (\n -> (if n.id == id then {n | collapsed = not n.collapsed} else n ) ) model.bookmarks }, Cmd.none)
    CollapseNode id ->
        ({ model | bookmarks = map (\n -> (if n.id == id then {n | collapsed = not n.collapsed} else n ) ) model.bookmarks }, Cmd.none)
    Tick t ->
        if model.rerender then
            ({ model | rerender = False }, reRender "")
        else
            model ! []
