module Picshare exposing (main)

import Html exposing (Html, div, h1, h2, h3, text, img, i, b, ul, li, input, button, form)
import Html.Attributes exposing (class, src, placeholder, value)
import Html.Events exposing (onClick, onSubmit, onInput)
import Debug exposing (log)
import Json.Decode exposing (Decoder, decodeString, bool, int, list, string)
import Json.Decode.Pipeline exposing (decode, hardcoded, required)
import Http

baseUrl : String
baseUrl = "https://front-end-elm.surge.sh/"

imgUrl : String -> String
imgUrl img_name = baseUrl ++ img_name

type Msg
  = ToggleLike
  | SaveComment 
  | UpdateComment String
  | LoadFeed (Result Http.Error Photo)

type alias Id = Int

type alias Photo = 
  { id : Id
  , url : String
  , caption : String
  , liked : Bool
  , comments : List String
  , newComment : String
  }
    
type alias Model = Photo


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
  Http.get (baseUrl ++ "feed/1") photoDecoder
  |> Http.send LoadFeed

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let 
      userInput = model.newComment |> String.trim
  in
  case msg of
    ToggleLike ->
      ( { model | liked = not model.liked }
      , Cmd.none
      )

    SaveComment ->
      if userInput == "" then
        ( { model | newComment = "" }
        , Cmd.none
        )
      else
        ( { model | comments = model.comments ++ [userInput], newComment = "" }
        , Cmd.none
        )
      
    UpdateComment comment ->
      ( { model | newComment = comment }, Cmd.none )

    LoadFeed _ ->
      ( model, Cmd.none )

viewLoveButton : Model -> Html Msg
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
        , onClick ToggleLike
       ]
       []
    

viewComment : String -> Html msg
viewComment comment =
  li [ class "comment" ] 
     [
       b [] [text "Comment: "]
     , text comment
     ]

commentsForm : Model -> Html Msg
commentsForm model = 
  form [ class "comments-form", onSubmit SaveComment ]
      [
        input [ onInput UpdateComment, placeholder "Add a comment", value model.newComment ] []
      , button [ class "button" ] [ text "Commit" ]
      ]

commentsHeader : Model -> Html msg
commentsHeader model =
  if List.length(model.comments) > 0 then
    h3 [class "comments"] [text "Comments"]
  else
    text ""

viewDetailedPhoto : Model -> Html Msg
viewDetailedPhoto model =
  div [ class "detailed-photo" ]
      [
        img [ src model.url ] []
      , div [ class "photo-info" ] 
            [ (viewLoveButton model) ]
      , div [ class "like-button" ] []
      , h2  [ class "caption" ] [ text model.caption ] 
      , commentsHeader model
      , ul  [ class "comments" ]
            (List.map viewComment model.comments)
      , commentsForm model
      ]


initialModel : Model
initialModel = {
    id = 1
  , url = imgUrl "1.jpg"
  , liked = False
  , caption = "Sunrise"
  , comments = []
  , newComment = ""
  }

view : Model -> Html Msg
view model = 
  div [ class "header" ] 
      [ h1 [] 
        [text "Picshare"]
      , div [ class "content-flow" ]
            [ viewDetailedPhoto model ]
      ]

init : (Model, Cmd Msg)
init =
  (initialModel, fetchFeed)

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

main : Program Never Model Msg
main =
  Html.program
     { init = init
     , view = view
     , update = update
     , subscriptions = subscriptions
     }

      --   viewDetailedPhoto (imgUrl "HackingBeautiful-774x179.png") "Hacking Beautifyl Code",
      --   viewDetailedPhoto (imgUrl "andi_thinking.jpg")   "Thinking",
      --   viewDetailedPhoto (imgUrl "yarb_Pulp-O-Mizer_Cover_Image.jpg")   "YARB"
