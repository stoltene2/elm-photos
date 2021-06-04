module PhotoFolders exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h2, h3, img, span, text)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)
import PhotoGroove exposing (urlPrefix)


type alias Photo =
    { title : String
    , size : Int
    , relatedUrls : List String
    , url : String
    }


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    }


type Msg
    = GotInitialModel (Result Http.Error Model)
    | ClickedPhoto String


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    }


view : Model -> Html Msg
view model =
    let
        photoByUrl : String -> Maybe Photo
        photoByUrl url =
            Dict.get url model.photos

        selectedPhoto : Html Msg
        selectedPhoto =
            case Maybe.andThen photoByUrl model.selectedPhotoUrl of
                Just photo ->
                    viewSelectedPhoto photo

                Nothing ->
                    text ""
    in
    div [ class "content" ]
        [ div [ class "selected-photo" ] [ selectedPhoto ] ]


viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo =
    div
        [ class "selected-photo" ]
        [ h2 [] [ text photo.title ]
        , img [ src (urlPrefix ++ "photos/" ++ photo.url ++ "/full") ] []
        , span [] [ text (String.fromInt photo.size ++ "KB") ]
        , h3 [] [ text "Related" ]
        , div [ class "related-photos" ]
            (List.map viewRelatedPhoto photo.relatedUrls)
        ]


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url =
    img
        [ class "related-photo"
        , onClick (ClickedPhoto url)
        , src (urlPrefix ++ "photos/" ++ url ++ "/thumb")
        ]
        []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInitialModel (Err _) ->
            ( initialModel, Cmd.none )

        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


decodeModel : Decoder Model
decodeModel =
    Decode.succeed
        { selectedPhotoUrl = Just "trevi"
        , photos =
            Dict.fromList
                [ ( "trevi"
                  , { title = "Trevi"
                    , relatedUrls = [ "coli", "fresco" ]
                    , size = 34
                    , url = "trevi"
                    }
                  )
                , ( "fresco"
                  , { title = "Fresco"
                    , relatedUrls = [ "trevi" ]
                    , size = 46
                    , url = "fresco"
                    }
                  )
                , ( "coli"
                  , { title = "Coliseum"
                    , relatedUrls = [ "trevi", "fresco" ]
                    , size = 36
                    , url = "coli"
                    }
                  )
                ]
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "http://elm-in-action.com/folders/list"
        , expect = Http.expectJson GotInitialModel decodeModel
        }
    )


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
