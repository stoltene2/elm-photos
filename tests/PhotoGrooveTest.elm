module PhotoGrooveTest exposing (..)

import Expect exposing (Expectation, equal)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes as Attr
import Json.Decode exposing (decodeValue)
import Json.Encode as Encode
import PhotoGroove exposing (Photo, Status(..), decodePhoto, initialModel, urlPrefix)
import Test exposing (..)
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


thumbnailsAllRender : Test
thumbnailsAllRender =
    fuzz (Fuzz.intRange 1 5) "All thumbnails render" <|
        \urlCount ->
            let
                urls : List String
                urls =
                    List.range 1 urlCount
                        |> List.map (\n -> String.fromInt n ++ ".jpg")

                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel | status = Loaded (List.map photoFromUrl urls) "" }
                |> PhotoGroove.view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }
