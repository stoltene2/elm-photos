port module PhotoGroove exposing (Msg(..), Photo, Status(..), decodePhoto, initialModel, main, urlPrefix, view)

import Browser
import Html exposing (Html, button, canvas, div, h1, img, input, label, node, text)
import Html.Attributes as Attributes exposing (..)
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as Decode exposing (Decoder)
import Json.Encode
import List
import Random


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { chosenSize : ThumbnailSize
    , status : Status
    , hue : Int
    , ripple : Int
    , noise : Int
    , activity : String
    }


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | SlideHue Int
    | SlideRipple Int
    | SlideNoise Int
    | OnActivity String


type ThumbnailSize
    = Small
    | Medium
    | Large


initialModel : Model
initialModel =
    { chosenSize = Medium
    , status = Loading
    , hue = 5
    , ripple = 5
    , noise = 5
    , activity = ""
    }


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


port setFilters : FilterOptions -> Cmd msg



--                 ^ One param          ^ type variable


port setActivity : (String -> msg) -> Sub msg


decodePhoto : Decoder Photo
decodePhoto =
    Decode.map3 Photo
        (Decode.field "url" Decode.string)
        (Decode.field "size" Decode.int)
        (fieldDefault "(untitled)" <| Decode.maybe (Decode.field "title" Decode.string))


fieldDefault : val -> Decoder (Maybe val) -> Decoder val
fieldDefault default =
    Decode.map <|
        \val ->
            case val of
                Nothing ->
                    default

                Just v ->
                    v


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selected ->
                viewLoaded photos selected model

            Loading ->
                []

            Errored err ->
                [ text <| "Error: " ++ err ]


rangeSlider : List (Html.Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attributes.max "11"
            , Attributes.property "val" (Json.Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selected model =
    [ h1 [] [ text "Photo Groove" ]
    , div [ id "choose-size" ] (List.map viewSizeChooser [ Small, Medium, Large ])
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise me" ]
    , div [ class "activity" ] [ text model.activity ]
    , div [ class "filters" ]
        [ viewFilter SlideHue "Hue" model.hue
        , viewFilter SlideRipple "Ripple" model.ripple
        , viewFilter SlideNoise "Noise" model.noise
        ]
    , div
        [ id "thumbnails"
        , class (sizeToString model.chosenSize)
        , style "float" "left"
        ]
        (List.map (viewThumbnail selected) photos)
    , canvas [ id "main-canvas", class "large" ] []
    ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selected t =
    img
        [ src (urlPrefix ++ t.url)
        , classList [ ( "selected", t.url == selected ) ]
        , onClick (ClickedPhoto t.url)
        , title (t.title ++ " [" ++ String.fromInt t.size ++ " KB]")
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
            "med"

        Large ->
            "large"


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        _ ->
            status


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded _ selectedUrl ->
            let
                url =
                    urlPrefix ++ "large/" ++ selectedUrl

                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]
            in
            ( model, setFilters { url = url, filters = filters } )

        _ ->
            ( model, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSurpriseMe ->
            case model.status of
                Loaded (p :: ps) _ ->
                    Random.uniform p ps
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                _ ->
                    ( model, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }
            , Cmd.none
            )

        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        GotPhotos (Ok photos) ->
            case photos of
                first :: _ ->
                    applyFilters { model | status = Loaded photos first.url }

                [] ->
                    ( { model | status = Errored "0 photos found" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "oh boy" }, Cmd.none )

        SlideHue hue ->
            applyFilters { model | hue = hue }

        SlideRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlideNoise noise ->
            applyFilters { model | noise = noise }

        OnActivity activity ->
            ( { model | activity = activity }, Cmd.none )


onSlide : (Int -> msg) -> Html.Attribute msg
onSlide toMsg =
    Decode.at [ "detail", "userSlidTo" ] Decode.int
        |> Decode.map toMsg
        |> on "slide"


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (Decode.list decodePhoto)
        }


main : Program String Model Msg
main =
    Browser.element
        { init = \activity -> ( { initialModel | activity = "Pasta v" ++ activity }, initialCmd )
        , view = view
        , update = update
        , subscriptions = \_ -> setActivity OnActivity
        }
