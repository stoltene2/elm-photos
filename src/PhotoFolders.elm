module PhotoFolders exposing (..)

import Browser
import Dict exposing (Dict)
import Html exposing (Html, div, h1, h2, h3, img, label, span, text)
import Html.Attributes exposing (class, name, src)
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


type alias JsonPhoto =
    { title : String
    , size : Int
    , relatedUrls : List String
    }


type Msg
    = GotInitialModel (Result Http.Error Model)
    | ClickedPhoto String
    | ClickedFolder FolderPath


type Folder
    = Folder
        { name : String
        , photoUrls : List String
        , subfolders : List Folder
        , expanded : Bool
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
    , root = Folder { name = "Loading...", photoUrls = [], subfolders = [], expanded = False }
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
            , viewFolder End model.root
            ]
        , div [ class "selected-photo" ] [ selectedPhoto ]
        ]


viewPhoto : String -> Html Msg
viewPhoto url =
    div [ class "photo", onClick (ClickedPhoto url) ]
        [ text url ]


viewFolder : FolderPath -> Folder -> Html Msg
viewFolder path (Folder folder) =
    let
        viewSubfolder : Int -> Folder -> Html Msg
        viewSubfolder index subfolder =
            viewFolder (appendIndex index path) subfolder

        folderLabel =
            label [ onClick (ClickedFolder path) ] [ text folder.name ]
    in
    if folder.expanded then
        let
            contents =
                List.append
                    (List.indexedMap viewSubfolder folder.subfolders)
                    (List.map viewPhoto folder.photoUrls)
        in
        div [ class "folder expanded" ]
            [ folderLabel
            , div [ class "contents" ] contents
            ]

    else
        div [ class "folder collapsed" ] [ folderLabel ]


appendIndex : Int -> FolderPath -> FolderPath
appendIndex index path =
    case path of
        End ->
            SubFolder index End

        SubFolder idx rest ->
            SubFolder idx (appendIndex index rest)



{- *
   appendIndex End root

   SubFolder 0 End
   SubFolder 1 End


   *
-}


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


type FolderPath
    = End
    | SubFolder Int FolderPath


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotInitialModel (Err _) ->
            ( initialModel, Cmd.none )

        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        ClickedFolder path ->
            ( { model | root = toggleExpanded path model.root }, Cmd.none )


toggleExpanded : FolderPath -> Folder -> Folder
toggleExpanded path (Folder folder) =
    case path of
        End ->
            Folder { folder | expanded = not folder.expanded }

        SubFolder targetIndex remainingPath ->
            let
                subfolders : List Folder
                subfolders =
                    List.indexedMap transform folder.subfolders

                transform : Int -> Folder -> Folder
                transform i currentSubfolder =
                    if i == targetIndex then
                        toggleExpanded remainingPath currentSubfolder

                    else
                        currentSubfolder
            in
            Folder { folder | subfolders = subfolders }


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- Intermediate representation


jsonPhotoDecoder : Decoder JsonPhoto
jsonPhotoDecoder =
    Decode.succeed JsonPhoto
        |> required "title" string
        |> required "size" int
        |> required "related_photos" (list string)


photoDecoder : Decoder (Dict String Photo)
photoDecoder =
    Decode.keyValuePairs jsonPhotoDecoder
        |> Decode.map fromPairs


finishPhoto : ( String, JsonPhoto ) -> ( String, Photo )
finishPhoto ( url, json ) =
    ( url
    , { url = url
      , size = json.size
      , title = json.title
      , relatedUrls = json.relatedUrls
      }
    )


fromPairs : List ( String, JsonPhoto ) -> Dict String Photo
fromPairs pairs =
    pairs
        |> List.map finishPhoto
        |> Dict.fromList


folderDecoder : Decoder Folder
folderDecoder =
    Decode.succeed folderFromJson
        |> required "name" string
        |> required "photos" photoDecoder
        |> required "subfolders" (Decode.lazy (\() -> list folderDecoder))


folderFromJson : String -> Dict String Photo -> List Folder -> Folder
folderFromJson name photos subfolders =
    Folder
        { name = name
        , photoUrls = Dict.keys photos
        , expanded = True
        , subfolders = subfolders
        }


modelPhotosDecoder : Decoder (Dict String Photo)
modelPhotosDecoder =
    Decode.succeed modelPhotosFromJson
        |> required "photos" photoDecoder
        |> required "subfolders" (Decode.lazy (\() -> list modelPhotosDecoder))


modelPhotosFromJson : Dict String Photo -> List (Dict String Photo) -> Dict String Photo
modelPhotosFromJson photos subfolderPhotos =
    List.foldl Dict.union photos subfolderPhotos


modelDecoder : Decoder Model
modelDecoder =
    Decode.map2 combineDecodedModel modelPhotosDecoder folderDecoder


combineDecodedModel : Dict String Photo -> Folder -> Model
combineDecodedModel photos root =
    { photos = photos
    , root = root
    , selectedPhotoUrl = Nothing
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
