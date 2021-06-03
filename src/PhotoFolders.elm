module PhotoFolders exposing (main)

import Browser
import Html exposing (Html)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Model =
    { selectedPhotoUrl : Maybe String
    }


type Msg
    = GotInitialModel (Result Http.Error Model)
    | ClickedPhoto String


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing }


view : Model -> Html Msg
view _ =
    Html.div [] [ Html.text "Hello" ]


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
    Decode.succeed initialModel


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
