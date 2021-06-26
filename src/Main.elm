module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)
import Html.Events exposing (onMouseOver)
import Html.Lazy exposing (lazy)


type alias Model =
    { page : Page }


type Page
    = Gallery
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
            li [ classList [ ( "active", page == targetPage ) ] ]
                [ a [ href url ] [ text caption ] ]
    in
    nav [] [ logo, links ]


viewFooter : Html Msg
viewFooter =
    footer [] [ text "This is the footer" ]


type Msg
    = NothingYet


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( { page = Gallery }, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        , view = view
        }
