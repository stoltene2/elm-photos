module PhotoGroove exposing (main)

import Browser
import Html exposing (Html, button, div, h1, img, input, label, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import List
import Random


type alias Photo =
    { url : String }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { chosenSize : ThumbnailSize
    , status : Status
    }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo


type ThumbnailSize
    = Small
    | Medium
    | Large


initialModel : Model
initialModel =
    { chosenSize = Medium
    , status =
        Loaded
            [ { url = "1.jpeg" }
            , { url = "2.jpeg" }
            , { url = "3.jpeg" }
            ]
            "1.jpeg"
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selected ->
                viewLoaded photos selected model.chosenSize

            Loading ->
                []

            Errored err ->
                [ text <| "Error: " ++ err ]


viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selected chosenSize =
    [ h1 [] [ text "Photo Groove" ]
    , div [ id "choose-size" ] (List.map viewSizeChooser [ Small, Medium, Large ])
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise me" ]
    , div
        [ id "thumbnails"
        , class (sizeToString chosenSize)
        , style "float" "left"
        ]
        (List.map (viewThumbnail selected) photos)
    , img
        [ class "large"
        , style "float" "right"
        , src (urlPrefix ++ "large/" ++ selected)
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


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        _ ->
            status


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | status = selectUrl url model.status }
            , Cmd.none
            )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (p :: ps) _ ->
                    ( model
                    , Random.generate GotRandomPhoto <| Random.uniform p ps
                    )

                _ ->
                    ( model, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }
            , Cmd.none
            )

        GotRandomPhoto photo ->
            ( { model | status = selectUrl photo.url model.status }
            , Cmd.none
            )


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
