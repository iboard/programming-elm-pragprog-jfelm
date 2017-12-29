module Picshare exposing (main)

import Html exposing (Html, div, h1, h2, h3, text, img, i, b, ul, li, input, button, form)
import Html.Attributes exposing (class, src, placeholder, value)
import Html.Events exposing (onClick, onSubmit, onInput)
import Debug exposing (log)

type Msg
  = ToggleLike
  | SaveComment 
  | UpdateComment String

type alias Model = 
  { url : String
  , caption : String
  , liked : Bool
  , comments : (List String)
  , newComment : String
  }
    

update : Msg -> Model -> Model
update msg model =
  let 
      userInput = model.newComment |> String.trim
  in
  case msg of
    ToggleLike ->
      { model | liked = not model.liked }

    SaveComment ->
      if userInput == "" then
        { model | newComment = "" }
      else
        { model | comments = model.comments ++ [userInput],
                  newComment = "" 
        }
      
    UpdateComment comment ->
      { model | newComment = comment }

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

baseUrl : String
baseUrl = "https://s3.eu-central-1.amazonaws.com/iboard.core/static/"

imgUrl : String -> String
imgUrl img_name = baseUrl ++ img_name


initialModel : Model
initialModel = Model (imgUrl "HackingBeautiful-774x179.png") "Hacking Beautiful Code" False [] ""

view : Model -> Html Msg
view model = 
  div [ class "header" ] 
      [ h1 [] 
        [text "Picshare"]
      , div [ class "content-flow" ]
            [ viewDetailedPhoto model ]
      ]

main : Program Never Model Msg
main =
  Html.beginnerProgram
     { model = initialModel
     , view = view
     , update = update
     }

      --   viewDetailedPhoto (imgUrl "HackingBeautiful-774x179.png") "Hacking Beautifyl Code",
      --   viewDetailedPhoto (imgUrl "andi_thinking.jpg")   "Thinking",
      --   viewDetailedPhoto (imgUrl "yarb_Pulp-O-Mizer_Cover_Image.jpg")   "YARB"
