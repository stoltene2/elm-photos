module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (Html, button, div, h1, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List


type alias Photo =
    { url : String }


type alias Model =
    { photos : List Photo
    , selected : String
    , chosenSize : ThumbnailSize
    }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe


type ThumbnailSize
    = Small
    | Medium
    | Large


initialModel : Model
initialModel =
    { photos =
        [ { url = "1.jpeg" }
        , { url = "2.jpeg" }
        , { url = "3.jpeg" }
        ]
    , selected = "1.jpeg"
    , chosenSize = Medium
    }


photoArray : Array Photo
photoArray =
    Array.fromList initialModel.photos


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


getPhotoUrl : Int -> String
getPhotoUrl n =
    case Array.get n photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""


view : Model -> Html Msg
view model =
    div [ class "conent" ]
        [ h1 [] [ text "Photo Groove" ]
        , div [ id "choose-size" ] (List.map viewSizeChooser [ Small, Medium, Large ])
        , button
            [ onClick ClickedSurpriseMe ]
            [ text "Surprise me" ]
        , div
            [ id "thumbnails"
            , class (sizeToString model.chosenSize)
            , style "float" "left"
            ]
            (List.map (viewThumbnail model.selected) model.photos)
        , img
            [ class "large"
            , style "float" "right"
            , src (urlPrefix ++ "large/" ++ model.selected)
            ]
            []
        ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selected t =
    img
        [ src (urlPrefix ++ t.url)
        , classList [ ( "selected", t.url == selected ) ]
        , onClick (ClickedPhoto t.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input
            [ type_ "radio"
            , name "size"
            , onClick (ClickedSize size)
            ]
            []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Small ->
            "small"

        Medium ->
            "medium"

        Large ->
            "large"


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedPhoto s ->
            { model | selected = s }

        ClickedSurpriseMe ->
            { model | selected = "2.jpeg" }

        ClickedSize size ->
            { model | chosenSize = size }


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
