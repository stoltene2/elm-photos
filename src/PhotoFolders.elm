module PhotoFolders exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, h2, h3, img, label, span, text)
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


type Msg
    = GotInitialModel (Result Http.Error Model)
    | ClickedPhoto String


type Folder
    = Folder
        { name : String
        , photoUrls : List String
        , subfolders : List Folder
        }


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    , root : Folder
    }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing
    , photos = Dict.empty
    , root = Folder { name = "Loading...", photoUrls = [], subfolders = [] }
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
        [ div [ class "folders" ]
            [ h1 [] [ text "Folders" ]
            , viewFolder model.root
            ]
        , div [ class "selected-photo" ] [ selectedPhoto ]
        ]


viewFolder : Folder -> Html Msg
viewFolder (Folder folder) =
    let
        subfolders =
            List.map viewFolder folder.subfolders
    in
    div
        [ class "folder" ]
        [ label [] [ text folder.name ]
        , div [ class "subfolder" ] subfolders
        ]


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


modelDecoder : Decoder Model
modelDecoder =
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
        , root =
            Folder
                { name = "Photos"
                , photoUrls = []
                , subfolders =
                    [ Folder
                        { name = "2016"
                        , photoUrls = [ "trevi", "coli" ]
                        , subfolders =
                            [ Folder
                                { name = "outdoors"
                                , photoUrls = []
                                , subfolders = []
                                }
                            , Folder
                                { name = "indoors"
                                , photoUrls = [ "fresco" ]
                                , subfolders = []
                                }
                            ]
                        }
                    , Folder
                        { name = "2017"
                        , photoUrls = []
                        , subfolders =
                            [ Folder
                                { name = "outdoors"
                                , photoUrls = []
                                , subfolders = []
                                }
                            , Folder
                                { name = "indoors"
                                , photoUrls = []
                                , subfolders = []
                                }
                            ]
                        }
                    ]
                }
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "http://elm-in-action.com/folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
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
