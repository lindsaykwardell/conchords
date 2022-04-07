module Main exposing (main)

import Browser
import Csv.Decode as CsvDecode
import File exposing (File)
import File.Select as Select
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline as Decode
import Json.Encode as Encode
import Task
import Ui


type alias Model =
    { music : List Music
    , searchValue : String
    , order : Maybe ( String, Ui.Direction )
    }


type Msg
    = CsvRequested
    | CsvSelected File
    | CsvLoaded String
    | CsvUploaded (Result Http.Error (List Music))
    | UpdateSearch String
    | UpdateOrder ( String, Ui.Direction )
    | NoOp


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
    ( { music = music
      , searchValue = ""
      , order = Nothing
      }
    , Cmd.none
    )


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

        UpdateSearch value ->
            ( { model | searchValue = value }
            , Cmd.none
            )

        UpdateOrder order ->
            ( { model | order = Just order }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    case model.music of
        [] ->
            button [ onClick CsvRequested ] [ text "Load CSV" ]

        content ->
            div []
                [ musicTable
                    { music = content
                    , search = model.searchValue
                    , onSearch = UpdateSearch
                    , order = model.order
                    , onOrder = UpdateOrder
                    }
                ]



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


musicTable :
    { music : List Music
    , search : String
    , onSearch : String -> Msg
    , order : Maybe ( String, Ui.Direction )
    , onOrder : ( String, Ui.Direction ) -> Msg
    }
    -> Html Msg
musicTable { music, search, onSearch, order, onOrder } =
    Ui.table
        { headers =
            [ "Title"
            , "Composer"
            , "Publisher"
            , "Parts"
            , "Qty"
            ]
        , data = music
        , render =
            \piece ->
                [ td [] [ text piece.title ]
                , td [] [ text piece.composer ]
                , td [] [ text piece.publisher ]
                , td [] [ text piece.parts ]
                , td [] [ text (String.fromInt piece.qty) ]
                ]
        , controls =
            [ Ui.Search search
                (\piece ->
                    let
                        query =
                            String.toLower search

                        title =
                            String.toLower piece.title

                        composer =
                            String.toLower piece.composer

                        publisher =
                            String.toLower piece.publisher

                        parts =
                            String.toLower piece.parts

                        qty =
                            String.fromInt piece.qty
                    in
                    String.contains query title
                        || String.contains query composer
                        || String.contains query publisher
                        || String.contains query parts
                        || String.contains query qty
                )
                onSearch
            , case order of
                Nothing ->
                    Ui.OrderBy "" (\_ -> "") Ui.Ascending onOrder

                Just ( field, direction ) ->
                    Ui.OrderBy field
                        (\piece ->
                            case field of
                                "Title" ->
                                    piece.title

                                "Composer" ->
                                    piece.composer

                                "Publisher" ->
                                    piece.publisher

                                "Parts" ->
                                    piece.parts

                                _ ->
                                    ""
                        )
                        direction
                        onOrder
            ]
        }
        []
