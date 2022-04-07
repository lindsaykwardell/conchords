module Ui exposing (..)

import Html as Html exposing (..)
import Html.Attributes as Attr exposing (..)
import Html.Events as Events exposing (onSubmit)
import Json.Decode as Decode
import List.Extra as List


type alias InputConfig msg =
    { label : String
    , type_ : InputType
    , value : String
    , onInput : Maybe (String -> msg)
    }


type InputType
    = Text
    | Number
    | Email
    | Password


inputType : InputType -> Attribute msg
inputType type_ =
    case type_ of
        Text ->
            Attr.type_ "text"

        Number ->
            Attr.type_ "number"

        Email ->
            Attr.type_ "email"

        Password ->
            Attr.type_ "password"


input : InputConfig msg -> List (Attribute msg) -> List (Attribute msg) -> Html msg
input config outerAttr innerAttr =
    withLabel
        { label = config.label }
        (class "focus-within:border-blue-500" :: outerAttr)
        (Html.input
            ([ class "w-full text-sm p-1 bg-transparent outline-none text-black text-base"
             , inputType config.type_
             , value config.value
             ]
                ++ (case config.onInput of
                        Nothing ->
                            []

                        Just onInput ->
                            [ Events.onInput onInput ]
                   )
                ++ innerAttr
            )
            []
        )


type alias TextAreaConfig msg =
    { label : String
    , value : String
    , onInput : Maybe (String -> msg)
    }


textarea : TextAreaConfig msg -> List (Attribute msg) -> List (Attribute msg) -> Html msg
textarea config outerAttr innerAttr =
    withLabel
        { label = config.label }
        (class "focus-within:border-blue-500 h-full" :: outerAttr)
        (Html.textarea
            ([ class "w-full text-sm p-1 bg-transparent outline-none h-full resize-none text-black text-base"
             ]
                ++ (case config.onInput of
                        Nothing ->
                            []

                        Just onInput ->
                            [ Events.onInput onInput ]
                   )
                ++ innerAttr
            )
            [ text config.value ]
        )


type alias RadioGroupConfig msg =
    { value : String
    , label : String
    , name : String
    , options : List (RadioOption msg)
    , direction : FlexDirection
    }


type FlexDirection
    = Row
    | Column


flexDirection : FlexDirection -> Attribute msg
flexDirection direction =
    case direction of
        Row ->
            class "flex flex-col md:flex-row"

        Column ->
            class "flex flex-col"


type alias RadioOption msg =
    { label : String
    , value : String
    , onChange : Maybe (Bool -> String -> msg)
    }


radioGroup : RadioGroupConfig msg -> List (Attribute msg) -> Html msg
radioGroup config attrs =
    withLabel { label = config.label }
        attrs
        (Html.div [ class "flex justify-around p-1", flexDirection config.direction ]
            (config.options
                |> List.map
                    (\option ->
                        Html.label
                            [ class "flex items-center text-sm rounded focus-within:text-black border border-transparent px-1 focus-within:border-blue-500 hover:text-black"
                            , class
                                (if option.value == config.value then
                                    "text-black"

                                 else
                                    "text-gray-300"
                                )
                            ]
                            [ Html.input
                                ([ type_ "radio"
                                 , name config.name
                                 , value option.value
                                 , class "outline-none"
                                 , checked (option.value == config.value)
                                 ]
                                    ++ (case option.onChange of
                                            Nothing ->
                                                []

                                            Just onChange ->
                                                [ Events.onCheck (\checked -> onChange checked option.value) ]
                                       )
                                )
                                []
                            , Html.span [ class "ml-2 text-base" ] [ Html.text option.label ]
                            ]
                    )
            )
        )


type alias CheckboxGroupConfig msg =
    { label : String
    , name : String
    , options : List (CheckboxOption msg)
    , direction : FlexDirection
    }


type alias CheckboxOption msg =
    { label : String
    , value : Bool
    , onChange : Maybe (Bool -> String -> msg)
    }


checkboxGroup : CheckboxGroupConfig msg -> List (Attribute msg) -> Html msg
checkboxGroup config attrs =
    withLabel { label = config.label }
        attrs
        (Html.div [ class "flex justify-around p-1" ]
            (config.options
                |> List.map
                    (\option ->
                        Html.label
                            []
                            [ checkbox option [ class "text-black" ]
                            ]
                    )
            )
        )


checkbox : CheckboxOption msg -> List (Attribute msg) -> Html msg
checkbox config attrs =
    Html.div (class "flex items-center" :: attrs)
        [ Html.input
            ([ type_ "checkbox"
             , class "outline-none"
             , checked config.value
             ]
                ++ (case config.onChange of
                        Nothing ->
                            []

                        Just onChange ->
                            [ Events.onCheck (\checked -> onChange checked config.label) ]
                   )
            )
            []
        , Html.span [ class "ml-2 text-base" ] [ Html.text config.label ]
        ]


type alias LabelConfig =
    { label : String }


withLabel : LabelConfig -> List (Attribute msg) -> Html msg -> Html msg
withLabel config attrs content =
    fieldset ([ class "border rounded text-gray-500 text-sm" ] ++ attrs)
        [ legend [ class "px-1 font-oswald leading-none" ]
            [ text config.label ]
        , content
        ]


type alias SelectConfig msg =
    { label : String
    , options : List String
    , value : String
    , onInput : Maybe (String -> msg)
    }


inputSelect : SelectConfig msg -> List (Attribute msg) -> Html msg
inputSelect config attrs =
    div []
        [ input { label = config.label, type_ = Text, value = config.value, onInput = config.onInput } attrs [ Attr.list config.label ]
        , Html.datalist [ id config.label ]
            (config.options
                |> List.map
                    (\option ->
                        Html.option [ value option ] [ text option ]
                    )
            )
        ]


select : SelectConfig msg -> List (Attribute msg) -> Html msg
select config attrs =
    withLabel { label = config.label }
        (class "focus-within:border-blue-500" :: attrs)
        (Html.select
            ([ value config.value
             , class "w-full text-sm px-1 py-[.375rem] bg-transparent outline-none text-black text-base"
             ]
                ++ (case config.onInput of
                        Nothing ->
                            []

                        Just onInput ->
                            [ Events.onInput onInput ]
                   )
            )
            (Html.option [ value "", Attr.disabled True ] [ text "Select..." ]
                :: (config.options
                        |> List.map
                            (\opt ->
                                Html.option [ value opt ] [ text opt ]
                            )
                   )
            )
        )


grid : String -> List (Html msg) -> Html msg
grid cols content =
    div
        [ style "display" "grid"
        , style "grid-template-columns" cols
        , style "gap" "0.5rem"
        ]
        content


gridWithCols : Int -> List (Html msg) -> Html msg
gridWithCols cols content =
    grid (List.repeat cols "1fr" |> String.join " ") content


cell : { cols : Int, rows : Int } -> List (Attribute msg) -> List (Html msg) -> Html msg
cell { cols, rows } attrs content =
    let
        colClass =
            "col-span-" ++ String.fromInt cols

        rowClass =
            "row-span-" ++ String.fromInt rows
    in
    div (class (colClass ++ " " ++ rowClass) :: attrs) content


type alias TableConfig a msg =
    { headers : List String
    , data : List a
    , render : a -> List (Html msg)
    , controls : List (Control a msg)
    }


type Control a msg
    = EmptyControl
    | Print (a -> msg)
    | Copy (a -> msg)
    | Delete (a -> msg)
    | Search String (a -> Bool) (String -> msg)
    | OrderBy String (a -> String) Direction (( String, Direction ) -> msg)
    | Select (a -> msg)


type Direction
    = Ascending
    | Descending


table : TableConfig a msg -> List (Attribute msg) -> Html msg
table config attr =
    let
        isSearchEnabled =
            List.find
                (\control ->
                    case control of
                        Search _ _ _ ->
                            True

                        _ ->
                            False
                )
                config.controls

        isOrderEnabled =
            List.find
                (\control ->
                    case control of
                        OrderBy _ _ _ _ ->
                            True

                        _ ->
                            False
                )
                config.controls

        isSelectEnabled =
            List.find
                (\control ->
                    case control of
                        Select _ ->
                            True

                        _ ->
                            False
                )
                config.controls

        actionControls =
            List.filter
                (\control ->
                    case control of
                        Print _ ->
                            True

                        Copy _ ->
                            True

                        Delete _ ->
                            True

                        _ ->
                            False
                )
                config.controls
    in
    div [ class "w-full" ]
        [ case isSearchEnabled of
            Just (Search searchValue _ onSearch) ->
                Html.div
                    [ class "flex justify-end p-1" ]
                    [ input
                        { label = "Search"
                        , type_ = Text
                        , value = searchValue
                        , onInput = Just (\value -> onSearch value)
                        }
                        []
                        []
                    ]

            _ ->
                text ""
        , Html.table (class "w-full" :: attr)
            [ thead [ class "border-b font-oswald text-gray-500 text-sm sticky top-0 bg-white" ]
                [ tr []
                    (let
                        ( selectedHeader, selectedDirection ) =
                            case isOrderEnabled of
                                Just (OrderBy key _ direction _) ->
                                    ( key, direction )

                                _ ->
                                    ( "", Ascending )
                     in
                     List.map
                        (\header ->
                            let
                                selectHeader =
                                    case isOrderEnabled of
                                        Just (OrderBy _ _ direction onOrder) ->
                                            Just
                                                (onOrder
                                                    (case ( selectedHeader == header, direction ) of
                                                        ( True, Ascending ) ->
                                                            ( header, Descending )

                                                        ( True, Descending ) ->
                                                            ( "", Ascending )

                                                        ( False, _ ) ->
                                                            ( header, Ascending )
                                                    )
                                                )

                                        _ ->
                                            Nothing
                            in
                            th
                                (case selectHeader of
                                    Nothing ->
                                        []

                                    Just onSelect ->
                                        [ Events.onClick onSelect, class "hover:cursor-pointer" ]
                                )
                                [ text header
                                , text
                                    (if header == selectedHeader then
                                        case selectedDirection of
                                            Ascending ->
                                                "⬆"

                                            Descending ->
                                                "⬇"

                                     else
                                        ""
                                    )
                                ]
                        )
                        (config.headers
                            ++ (if List.length actionControls > 0 then
                                    [ "Actions" ]

                                else
                                    []
                               )
                        )
                    )
                ]
            , tbody
                [ class
                    (if isSelectEnabled /= Nothing then
                        "hover:cursor-pointer"

                     else
                        ""
                    )
                ]
                (config.data
                    |> (\data ->
                            case isOrderEnabled of
                                Just (OrderBy _ getKey direction _) ->
                                    List.sortBy getKey data
                                        |> (\sortedData ->
                                                case direction of
                                                    Ascending ->
                                                        sortedData

                                                    Descending ->
                                                        List.reverse sortedData
                                           )

                                _ ->
                                    data
                       )
                    |> List.filterMap
                        (\row ->
                            let
                                render =
                                    tr
                                        (class "bg-white odd:bg-gray-50 hover:bg-blue-100"
                                            :: (case isSelectEnabled of
                                                    Just (Select onSelect) ->
                                                        [ Events.onClick (onSelect row) ]

                                                    _ ->
                                                        []
                                               )
                                        )
                                        (config.render row
                                            ++ (if List.length actionControls > 0 then
                                                    [ td [ class "flex justify-around gap-1" ]
                                                        (actionControls
                                                            |> List.map
                                                                (\action ->
                                                                    case action of
                                                                        Print onPrint ->
                                                                            button
                                                                                [ class "border rounded border-green-500 w-8 h-8 hover:bg-green-300 hover:border-green-800 transition duration-75"
                                                                                , Events.stopPropagationOn "click" (Decode.succeed ( onPrint row, True ))
                                                                                ]
                                                                                [ text "🖨" ]

                                                                        Copy onCopy ->
                                                                            button
                                                                                [ class "border rounded border-blue-500 w-8 h-8 hover:bg-blue-300 hover:border-blue-800 transition duration-75"
                                                                                , Events.stopPropagationOn "click" (Decode.succeed ( onCopy row, True ))
                                                                                ]
                                                                                [ text "📋" ]

                                                                        Delete onDelete ->
                                                                            button
                                                                                [ class "border rounded border-red-500 w-8 h-8 hover:bg-red-300 hover:border-red-800 transition duration-75"
                                                                                , Events.stopPropagationOn "click" (Decode.succeed ( onDelete row, True ))
                                                                                ]
                                                                                [ text "🗑" ]

                                                                        _ ->
                                                                            text ""
                                                                )
                                                        )
                                                    ]

                                                else
                                                    [ text "" ]
                                               )
                                        )
                            in
                            case isSearchEnabled of
                                Just (Search _ searchFilter _) ->
                                    if searchFilter row then
                                        Just render

                                    else
                                        Nothing

                                _ ->
                                    Just render
                        )
                )
            ]
        ]


breadcrumb : List (Html msg) -> Html msg
breadcrumb breadcrumbs =
    let
        constructCrumb crumbs trail =
            case crumbs of
                [] ->
                    trail

                [ crumb ] ->
                    [ Html.span [ class "text-2xl text-gray-700" ] [ crumb ] ]

                crumb :: rest ->
                    [ crumb
                    , Html.span [ class "select-none" ] [ text " > " ]
                    ]
                        ++ constructCrumb rest (trail ++ [ crumb ])
    in
    Html.div [ class "text-lg font-oswald text-gray-500 pb-4 flex gap-1 items-center" ]
        (constructCrumb breadcrumbs [])


homeCrumb : msg -> Html msg
homeCrumb msg =
    Html.button [ Events.onClick msg ] [ Html.text "Home" ]


layout : { breadcrumbs : List (Html msg), search : Maybe (Html msg), content : List (Html msg), error : Maybe String, onCloseError : msg } -> Html msg
layout config =
    div []
        [ if List.length config.breadcrumbs > 0 || config.search /= Nothing then
            div [ class "" ]
                [ div [ class "p-3 pb-0 flex justify-between items-center bg-stone-200 shadow-md sticky top-0" ]
                    [ breadcrumb config.breadcrumbs
                    , div [ class "w-1/3" ] [ Maybe.withDefault (text "") config.search ]
                    ]
                , div [ class "p-3" ] config.content
                ]

          else
            div [] config.content
        , div
            [ class <|
                "fixed w-screen h-16 bg-red-800 text-white text-bold flex items-center px-6 font-bold gap-6 "
                    ++ (if config.error == Nothing then
                            "bottom-[-10vh]"

                        else
                            "bottom-0"
                       )
            ]
            [ div [ class "flex-shrink" ]
                [ button [ class "p-4", Events.onClick config.onCloseError ] [ text "X" ]
                ]
            , div [ class "flex-grow" ] [ text (Maybe.withDefault "" config.error) ]
            ]
        ]


card : List (Attribute msg) -> List (Html msg) -> Html msg
card attrs content =
    div (class "shadow-md rounded p-1 m-2 border" :: attrs) content


clickOutside : List (Attribute msg) -> List (Html msg) -> Html msg
clickOutside attr content =
    Html.node "click-outside" attr content


onClickOutside : msg -> Attribute msg
onClickOutside msg =
    Events.on "clickOutside" (Decode.succeed msg)


type DialogSections msg
    = Header (Html msg)
    | Body (Html msg)
    | Footer (Html msg)


dialog :
    { content : List (DialogSections msg)
    , onSubmit : msg
    , onClose : msg
    }
    -> List (Attribute msg)
    -> Html msg
dialog { content, onSubmit, onClose } attrs =
    let
        headers =
            List.filterMap
                (\section ->
                    case section of
                        Header header ->
                            Just header

                        _ ->
                            Nothing
                )
                content

        body =
            List.filterMap
                (\section ->
                    case section of
                        Body b ->
                            Just b

                        _ ->
                            Nothing
                )
                content

        footers =
            List.filterMap
                (\section ->
                    case section of
                        Footer footer ->
                            Just footer

                        _ ->
                            Nothing
                )
                content
    in
    Html.node "dialog"
        (class "rounded shadow-lg relative" :: attrs)
        [ Html.form
            [ Attr.attribute "method" "dialog"
            , Events.onSubmit onSubmit
            , class "flex flex-col gap-6"
            ]
            [ Html.button
                [ class "absolute top-0 right-0 text-2xl w-8 h-8 p-3 flex justify-center items-center hover:bg-gray-200 rounded"
                , Attr.value "cancel"
                , Attr.type_ "button"
                , Events.onClick onClose
                ]
                [ text "X" ]
            , Html.div [] headers
            , Html.div [] body
            , Html.div [ class "flex gap-4 justify-end" ] footers
            ]
        ]


saveContentDialog :
    { title : String
    , id : String
    , body : Html msg
    , onClose : msg
    , onSubmit : msg
    }
    -> Html msg
saveContentDialog { title, id, body, onClose, onSubmit } =
    dialog
        { content =
            [ Header <|
                Html.h1 [ Attr.class "text-3xl font-oswald" ] [ Html.text title ]
            , Body <| body
            , Footer <|
                Html.input
                    [ Attr.type_ "submit"
                    , Attr.class "bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded cursor-pointer transition duration-75"
                    ]
                    [ Html.text "Save" ]
            , Footer <|
                Html.button
                    [ Attr.class "bg-transparent border text-blue-800 border-blue-500 hover:bg-blue-700 hover:text-white font-bold py-2 px-4 rounded cursor-pointer transition duration-75"
                    , Events.preventDefaultOn
                        "click"
                        (Decode.succeed ( onClose, True ))
                    ]
                    [ Html.text "Cancel" ]
            ]
        , onSubmit = onSubmit
        , onClose = onClose
        }
        [ Attr.id id, Attr.class "w-[800px]" ]


type Tab tab
    = Tab tab


tabOption : tab -> Tab tab
tabOption tab =
    Tab tab


tabs :
    { activeTab : Tab tab
    , tabList : List ( Tab tab, String, Html msg )
    , onTabChange : Tab tab -> msg
    }
    -> Html msg
tabs { activeTab, tabList, onTabChange } =
    Html.div [ Attr.class "flex flex-col" ]
        [ Html.div [ Attr.class "flex gap-2" ]
            (tabList
                |> List.map
                    (\( tabId, tabLabel, _ ) ->
                        Html.div []
                            [ Html.button
                                [ class <|
                                    "bg-transparent border-b-2 border-gray-500 hover:bg-gray-200 text-gray-800 font-oswald py-2 px-4 cursor-pointer hover:border-gray-500 transition duration-75 "
                                        ++ (if tabId == activeTab then
                                                "border-b-4"

                                            else
                                                " border-gray-300"
                                           )
                                , Events.onClick (onTabChange tabId)
                                ]
                                [ Html.text tabLabel ]
                            ]
                    )
            )
        , Html.div []
            (tabList
                |> List.map
                    (\( tabId, _, tabContent ) ->
                        if tabId == activeTab then
                            tabContent

                        else
                            Html.div [] []
                    )
            )
        ]
