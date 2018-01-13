module Picshare exposing (main)

import Html exposing (Html, div, h1, h2, h3, text, img, i, b, ul, li, input, button, form)
import Html.Attributes exposing (type_, class, src, placeholder, value)
import Html.Events exposing (onClick, onSubmit, onInput)
import Json.Decode exposing (Decoder, decodeString, bool, int, list, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, required)
import Http
import WebSocket
import Debug exposing (log)


-- --------------------------------------------------------------------
-- Config
-- --------------------------------------------------------------------

baseUrl : String
baseUrl = "https://front-end-elm.com/"

imgUrl : String -> String
imgUrl img_name = baseUrl ++ img_name

wsUrl : String
wsUrl = 
  "wss://programming-elm.com/"



-- --------------------------------------------------------------------
-- Types
-- --------------------------------------------------------------------

type Msg
  = ToggleLike Id
  | UpdateComment Id String
  | SaveComment Id 
  | LoadFeed (Result Http.Error Feed)
  | LoadStreamPhoto (Result String Photo)
  | FlushStreamQueue

type alias Id = Int

type alias Photo = 
  { id : Id
  , url : String
  , caption : String
  , liked : Bool
  , comments : List String
  , newComment : String
  }
    
type alias Feed =
  List Photo

type alias Model =
  { feed : Maybe Feed
  , error : Maybe Http.Error
  , streamQueue : Feed
  }

-- --------------------------------------------------------------------
-- JSON and API
-- --------------------------------------------------------------------

photoDecoder : Decoder Photo
photoDecoder = 
  decode Photo
  |> required "id" int
  |> required "url" string
  |> required "caption" string
  |> required "liked" bool
  |> required "comments" (list string)
  |> hardcoded ""

fetchFeed : Cmd Msg
fetchFeed =
  Http.get (baseUrl ++ "feed") (list photoDecoder)
  |> Http.send LoadFeed


-- --------------------------------------------------------------------
-- Message Handling
-- --------------------------------------------------------------------

-- The following functions take a Photo as argument.
-- The mapping from Maybe Photo to Photo is done by
-- the functions at the end of this block

saveComment : Photo -> Photo
saveComment photo =
      if photo.newComment == "" then
        { photo | newComment = "" }
      else
        { photo | comments = photo.comments ++ [photo.newComment], newComment = "" }

toggleLike : Photo -> Photo
toggleLike photo =
  { photo | liked = not photo.liked }

updateComment : String -> Photo -> Photo
updateComment comment photo =
  { photo | newComment = comment }

-- ----------------------------
-- MAPPING Maybe Photo to Photo
-- ----------------------------

updatePhotoById : (Photo -> Photo) -> Id -> Feed -> Feed
updatePhotoById updatePhoto id feed =
  List.map
      (\photo ->
        if photo.id == id then
          updatePhoto photo
        else
          photo
      )
      feed

updateFeed : (Photo -> Photo) -> Id -> Maybe Feed -> Maybe Feed
updateFeed updatePhoto id maybeFeed =
  Maybe.map (updatePhotoById updatePhoto id) maybeFeed

-- updateFeed calls updatePhoto when there is a Photo or
-- returns a Maybe Photo if not.

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

    ToggleLike id ->
      ( { model 
            | feed = updateFeed toggleLike id model.feed
        }
      , Cmd.none 
      )

    UpdateComment id comment ->
      ( { model 
            | feed = updateFeed (updateComment comment) id model.feed
        }
      , Cmd.none
      )

    SaveComment id ->
      ( { model
          | feed = updateFeed saveComment id model.feed
        }
      , Cmd.none
      )
      
    LoadFeed (Ok feed) ->
      ( { model | feed = Just feed }
      , Cmd.none
      )

    LoadFeed (Err error) ->
      ( { model | error = Just error }, Cmd.none )

    LoadStreamPhoto (Ok photo) ->
      ( { model | streamQueue = photo :: model.streamQueue }
      , Cmd.none
      )

    LoadStreamPhoto (Err _) ->
      ( model, Cmd.none)

    FlushStreamQueue ->
      ( model, Cmd.none )

-- --------------------------------------------------------------------
-- View
-- --------------------------------------------------------------------

viewLoveButton : Photo -> Html Msg
viewLoveButton photo =
  let
      buttonClass =
        if photo.liked then
          "fa-heart"
        else
          "fa-heart-o"
  in
     i [ class "fa fa-2x" 
        , class buttonClass
        , class "like-button"
        , onClick (ToggleLike photo.id)
       ]
       []
    
viewStreamNotification : Feed -> Html Msg
viewStreamNotification queue =
  case queue of
    [] ->
      text ""

    _ ->
      let
          content = 
            "View new photos: " ++ (toString (List.length queue))
      in
          div
            [ class "stream-notification"
            , onClick FlushStreamQueue
            ]
            [ text content ]

viewComment : String -> Html msg
viewComment comment =
  li [ class "comment" ] 
     [
       b [] [text "Comment: "]
     , text comment
     ]

viewComments : Photo -> Html Msg
viewComments photo =
  ul  [ class "comments" ]
      (List.map viewComment photo.comments)


commentsForm : Photo -> Html Msg
commentsForm photo = 
  form [ class "comments-form", onSubmit (SaveComment photo.id) ]
      [
        input 
        [ type_ "text"
        , placeholder "Add a comment" 
        , value photo.newComment
        , onInput (UpdateComment photo.id)
        ] []
      , button [ class "button" ] [ text "Commit" ]
      ]

commentsHeader : Photo -> Html msg
commentsHeader model =
  if List.length(model.comments) > 0 then
    h3 [class "comments"] [text "Comments"]
  else
    text ""

viewDetailedPhoto : Photo -> Html Msg
viewDetailedPhoto photo =
  div [ class "detailed-photo" ]
      [
        div [ class "like-button" ] [(viewLoveButton photo)]
      , img [ src photo.url ] []
      , div [ class "photo-info" ] 
            [ h2  [ class "caption" ] [ text photo.caption ] 
            , div []
                  [ commentsHeader photo
                  , viewComments photo
                  , commentsForm photo
                  ]
            ]
      ]


-- --------------------------------------------------------------------
-- Subscriptions / Callbacks
-- --------------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.feed of
    Just _ ->
      WebSocket.listen wsUrl (LoadStreamPhoto << (decodeString photoDecoder))

    Nothing ->
      Sub.none


-- --------------------------------------------------------------------
-- Application
-- --------------------------------------------------------------------

initialModel : Model
initialModel = 
  { feed = Nothing
  , error = Nothing
  , streamQueue = []
  }

errorMessage : Http.Error -> String
errorMessage error =
  case error of
    Http.BadPayload _ _ ->
      """Sorry, we couldn't process your feed at this time.
      We're working on it!"""
    _ ->
      """Sorry, we couldn't load your feed at this time.
      Please try again later."""

viewContent : Model -> Html Msg
viewContent model =
  case model.error of
    Just error ->
      div [ class "feed-error" ]
          [ text (errorMessage error) ]

    Nothing ->
      div []
          [ viewStreamNotification model.streamQueue
          , viewFeed model.feed
          ]

viewFeed : Maybe Feed -> Html Msg
viewFeed maybeFeed =
  case maybeFeed of
    Just feed ->
      div [ class "photo-grid" ] (List.map viewDetailedPhoto feed)
    Nothing ->
      div [ class "loading-feed" ]
          [ text "Loading Feed ..." ]

view : Model -> Html Msg
view model = 
  div [ class "header" ] 
      [ h1 [] 
        [text "Picshare"]
      , div [ class "content-flow" ]
            [ viewContent model ]
      ]

init : (Model, Cmd Msg)
init =
  (initialModel, fetchFeed)

main : Program Never Model Msg
main =
  Html.program
     { init = init
     , view = view
     , update = update
     , subscriptions = subscriptions
     }

