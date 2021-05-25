module PhotoGrooveTest exposing (..)

import Expect exposing (Expectation, equal)
import Fuzz exposing (Fuzzer, int, string)
import Html.Attributes as Attr
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (Msg(..), Photo, Status(..), decodePhoto, initialModel, urlPrefix, view)
import Test exposing (..)
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag)


suite : Test
suite =
    fuzz2 string int "decodePhoto title defaults to (untitled) when not provided" <|
        \url size ->
            [ ( "url", Encode.string url )
            , ( "size", Encode.int size )
            ]
                |> Encode.object
                |> decodeValue decodePhoto
                |> Result.map .title
                |> equal (Ok "(untitled)")


noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails render when there are no photos to render" <|
        \_ ->
            initialModel
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Query.findAll [ tag "img" ]
                |> Query.count (Expect.equal 0)


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll [ tag "img", attribute (Attr.src (urlPrefix ++ url)) ]
        |> Query.count (Expect.atLeast 1)


urlFuzzer : Fuzzer (List String)
urlFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map urlsFromCount


urlsFromCount : Int -> List String
urlsFromCount upper =
    List.range 1 upper
        |> List.map (\n -> String.fromInt n ++ ".jpg")


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }


thumbnailsAllRender : Test
thumbnailsAllRender =
    fuzz urlFuzzer "All thumbnails render" <|
        \urls ->
            let
                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel | status = Loaded (List.map photoFromUrl urls) "" }
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks


nonEmptyUrlList : String -> List String -> List String -> ( String, List String )
nonEmptyUrlList name front back =
    let
        url =
            name ++ ".png"
    in
    ( url, front ++ url :: back )


clickThumbnail : Test
clickThumbnail =
    fuzz3 urlFuzzer string urlFuzzer "clicking a thumbnail selects it" <|
        \urlsBefore urlToSelect urlsAfter ->
            let
                ( srcToClick, photos ) =
                    nonEmptyUrlList urlToSelect urlsBefore urlsAfter
                        |> Tuple.mapSecond (List.map photoFromUrl)
            in
            { initialModel | status = Loaded photos "" }
                |> view
                |> Query.fromHtml
                |> Query.find [ tag "img", attribute (Attr.src (urlPrefix ++ srcToClick)) ]
                |> Event.simulate Event.click
                |> Event.expect (ClickedPhoto srcToClick)
