module Main exposing (main)

import Browser
import Csv.Decode as CsvDecode
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, div, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Task


type alias Model =
    { music : List Music
    }


type Msg
    = CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | CsvUploaded (Result Http.Error (List Music))


main : Program (List Music) Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


init : List Music -> ( Model, Cmd Msg )
init music =
    ( { music = music }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CsvRequested ->
            ( model
            , Select.file [ "text/csv" ] CsvSelected
            )

        CsvSelected file ->
            ( model
            , Task.perform CsvLoaded (File.toString file)
            )

        CsvLoaded content ->
            ( model
            , content
                |> CsvDecode.decodeCsv CsvDecode.FieldNamesFromFirstRow musicCsvDecoder
                |> (\result ->
                        case result of
                            Result.Ok csv ->
                                submitMusic csv

                            Result.Err error ->
                                Cmd.none
                   )
            )

        CsvUploaded result ->
            case result of
                Ok music ->
                    ( { model | music = music }, Cmd.none )

                Err error ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.music of
        [] ->
            button [ onClick CsvRequested ] [ text "Load CSV" ]

        content ->
            div []
                (List.map
                    (\row ->
                        p [ style "white-space" "pre" ] [ text row.composer ]
                    )
                    content
                )



-- Music CSV


type alias MusicCsv =
    { title : String
    , composer : String
    , publisher : String
    , parts : String
    , qty : Int
    }


musicCsvDecoder : CsvDecode.Decoder MusicCsv
musicCsvDecoder =
    CsvDecode.into MusicCsv
        |> CsvDecode.pipeline (CsvDecode.field "TITLE" CsvDecode.string)
        |> CsvDecode.pipeline (CsvDecode.field "COMPOSER/ARRANGER" CsvDecode.string)
        |> CsvDecode.pipeline (CsvDecode.field "PUBLISHER" CsvDecode.string)
        |> CsvDecode.pipeline (CsvDecode.field "PARTS" CsvDecode.string)
        |> CsvDecode.pipeline
            (CsvDecode.field "QTY" CsvDecode.string
                |> CsvDecode.andThen
                    (\qty ->
                        case String.toInt qty of
                            Nothing ->
                                CsvDecode.succeed 0

                            Just val ->
                                CsvDecode.succeed val
                    )
            )


musicCsvEncoder : MusicCsv -> Encode.Value
musicCsvEncoder music =
    Encode.object
        [ ( "title", Encode.string music.title )
        , ( "composer", Encode.string music.composer )
        , ( "publisher", Encode.string music.publisher )
        , ( "parts", Encode.string music.parts )
        , ( "qty", Encode.int music.qty )
        ]


submitMusic : List MusicCsv -> Cmd Msg
submitMusic music =
    Http.post
        { url = "/api/music"
        , body = Http.jsonBody (Encode.list musicCsvEncoder music)
        , expect = Http.expectJson CsvUploaded (Decode.list musicDecoder)
        }



-- Music


type alias Music =
    { id : Int
    , title : String
    , composer : String
    , publisher : String
    , parts : String
    , qty : Int
    }


musicDecoder : Decode.Decoder Music
musicDecoder =
    Decode.succeed Music
        |> Decode.required "id" Decode.int
        |> Decode.required "title" Decode.string
        |> Decode.required "composer" Decode.string
        |> Decode.required "publisher" Decode.string
        |> Decode.required "parts" Decode.string
        |> Decode.required "qty" Decode.int
