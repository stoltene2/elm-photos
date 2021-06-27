module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Nav
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Lazy exposing (lazy)
import Maybe as Maybe
import Url exposing (Url)
import Url.Parser as Parser exposing ((</>), Parser, s, string)


type alias Model =
    { page : Page
    , key : Nav.Key
    }


type Page
    = SelectedPhoto String
    | Gallery
    | Folders
    | NotFound


view : Model -> Document Msg
view model =
    let
        content =
            text "Here is where the content goes"
    in
    { title = "Photo Groove, SPA style"
    , body =
        [ lazy viewHeader model.page
        , content
        , viewFooter
        ]
    }


viewHeader : Page -> Html Msg
viewHeader page =
    let
        logo =
            h1 [] [ text "Photogroove" ]

        links =
            ul []
                [ navLink Gallery { caption = "Gallery", url = "/gallery" }
                , navLink Folders { caption = "Folders", url = "/" }
                ]

        navLink : Page -> { caption : String, url : String } -> Html Msg
        navLink targetPage { caption, url } =
            li [ classList [ ( "active", isActive { link = targetPage, page = page } ) ] ]
                [ a [ href url ] [ text caption ] ]
    in
    nav [] [ logo, links ]


isActive : { link : Page, page : Page } -> Bool
isActive { link, page } =
    case ( link, page ) of
        ( Gallery, Gallery ) ->
            True

        ( Gallery, _ ) ->
            False

        ( Folders, Folders ) ->
            True

        ( Folders, SelectedPhoto _ ) ->
            True

        ( Folders, _ ) ->
            False

        ( SelectedPhoto _, _ ) ->
            False

        ( NotFound, _ ) ->
            False


viewFooter : Html Msg
viewFooter =
    footer []
        [ text "This is the footer"
        , text "and another piece of the footer"
        ]


type Msg
    = ClickedLink Browser.UrlRequest
    | ChangeUrl Url


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink urlRequest ->
            case urlRequest of
                Browser.External href ->
                    ( model, Nav.load href )

                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

        ChangeUrl url ->
            ( { model | page = urlToPage url }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { page = urlToPage url, key = key }, Cmd.none )


urlToPage : Url -> Page
urlToPage url =
    Parser.parse parser url
        |> Maybe.withDefault NotFound


parser : Parser (Page -> a) a
parser =
    Parser.oneOf
        [ Parser.map Folders Parser.top
        , Parser.map SelectedPhoto (s "photos" </> string)
        , Parser.map Gallery (s "gallery")
        ]


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        , onUrlRequest = ClickedLink
        , onUrlChange = ChangeUrl
        }
