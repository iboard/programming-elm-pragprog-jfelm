module Picshare exposing (main)

import Html exposing (Html, div, h1, h2, text, img, i)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)

type Msg
  = Like
  | Unlike


type alias Model = 
  { url : String
  , caption : String
  , liked : Bool
  }
    

update :
  Msg
  -> Model
  -> Model
update msg model =
  case msg of
    Like ->
      { model | liked = True }
    Unlike ->
      { model | liked = False }

viewDetailedPhoto : Model -> Html Msg
viewDetailedPhoto model =
  let 
      buttonClass =
        if model.liked then
          "fa-heart"
        else
          "fa-heart-o"
      msg =
        if model.liked then
          Unlike
        else
          Like
  in
      div [ class "detailed-photo" ]
          [
            img [ src model.url ] []
          , div [ class "photo-info" ] 
                [ div [ class "like-button" ] 
                      [ i [ class "fa fa-2x" 
                        , class buttonClass
                        , onClick msg
                        ]
                        []
                      ]
                , h2 [ class "caption" ] [ text model.caption ] 
                ]
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
