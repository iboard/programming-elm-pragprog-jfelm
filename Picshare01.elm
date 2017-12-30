module Picshare exposing (main)

import Html exposing (Html, div, h1, h2, h3, text, img, i, b, ul, li, input, button, form)
import Html.Attributes exposing (class, src, placeholder, value)
import Html.Events exposing (onClick, onSubmit, onInput)
import Debug exposing (log)
import Json.Decode exposing (Decoder, decodeString, bool, int, list, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, required)
import Http


-- --------------------------------------------------------------------
-- Config
-- --------------------------------------------------------------------

baseUrl : String
baseUrl = "https://front-end-elm.com/"

imgUrl : String -> String
imgUrl img_name = baseUrl ++ img_name

-- --------------------------------------------------------------------
-- Types
-- --------------------------------------------------------------------

type Msg
  = ToggleLike
  | SaveComment 
  | UpdateComment String
  | LoadFeed (Result Http.Error Feed)

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

updateFeed : (Photo -> Photo) -> Maybe Photo -> Maybe Photo
updateFeed updatePhoto maybePhoto =
  Maybe.map updatePhoto maybePhoto

-- updateFeed calls updatePhoto when there is a Photo or
-- returns a Maybe Photo if not.

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of

   -- ToggleLike ->
   --   ( { model 
   --         | photo = updateFeed toggleLike model.photo
   --     }
   --   , Cmd.none 
   --   )

   -- UpdateComment comment ->
   --   ( { model 
   --         | photo = updateFeed (updateComment comment) model.photo
   --     }
   --   , Cmd.none
   --   )

   -- SaveComment ->
   --   ( { model
   --       | photo = updateFeed saveComment model.photo
   --     }
   --   , Cmd.none
   --   )
      
    LoadFeed (Ok feed) ->
      ( { model | feed = Just feed }
      , Cmd.none
      )

    LoadFeed (Err _) ->
      ( model, Cmd.none )

    _ -> (model, Cmd.none)

-- --------------------------------------------------------------------
-- View
-- --------------------------------------------------------------------

viewLoveButton : Photo -> Html Msg
viewLoveButton model =
  let
      buttonClass =
        if model.liked then
          "fa-heart"
        else
          "fa-heart-o"
  in
     i [ class "fa fa-2x" 
        , class buttonClass
        , class "like-button"
        -- , onClick ToggleLike
       ]
       []
    

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
commentsForm model = 
  form [ class "comments-form", onSubmit SaveComment ]
      [
        input [ 
         -- onInput UpdateComment, 
         placeholder "Add a comment", value model.newComment ] []
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
        img [ src photo.url ] []
      , div [ class "photo-info" ] 
            [ (viewLoveButton photo) ]
      , div [ class "like-button" ] []
      , h2  [ class "caption" ] [ text photo.caption ] 
      , commentsHeader photo
      , viewComments photo
      , commentsForm photo
      ]


-- --------------------------------------------------------------------
-- Subscriptions / Callbacks
-- --------------------------------------------------------------------

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

-- --------------------------------------------------------------------
-- Application
-- --------------------------------------------------------------------

initialModel : Model
initialModel = 
  { feed = Nothing
  }

viewFeed : Maybe Feed -> Html Msg
viewFeed maybeFeed =
  case maybeFeed of
    Just feed ->
      div [] (List.map viewDetailedPhoto feed)
    Nothing ->
      div [ class "loading-feed" ]
          [ text "Loading Feed ..." ]

view : Model -> Html Msg
view model = 
  div [ class "header" ] 
      [ h1 [] 
        [text "Picshare"]
      , div [ class "content-flow" ]
            [ viewFeed model.feed ]
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

