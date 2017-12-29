module Picshare exposing (main)

import Html exposing (Html, div, h1, h2, text, img, i)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)

type Msg
  = ToggleLike

type alias Model = 
  { url : String
  , caption : String
  , liked : Bool
  }
    

update : Msg -> Model -> Model
update msg model =
  case msg of
    ToggleLike ->
      { model | liked = not model.liked }

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
    

viewDetailedPhoto : Model -> Html Msg
viewDetailedPhoto model =
  div [ class "detailed-photo" ]
      [
        img [ src model.url ] []
      , div [ class "photo-info" ] 
            [ (viewLoveButton model) ]
      , div [ class "like-button" ] []
      , h2  [ class "caption" ] [ text model.caption ] 
      ]

baseUrl : String
baseUrl = "https://s3.eu-central-1.amazonaws.com/iboard.core/static/"

imgUrl : String -> String
imgUrl img_name = baseUrl ++ img_name


initialModel : Model
initialModel = Model (imgUrl "HackingBeautiful-774x179.png") "Hacking Beautiful Code" False

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
