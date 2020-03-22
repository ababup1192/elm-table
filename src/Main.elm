module Main exposing (main)

import Browser
import Html
    exposing
        ( Html
        , button
        , div
        , i
        , main_
        , p
        , span
        , strong
        , table
        , tbody
        , td
        , text
        , th
        , thead
        , tr
        )
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    Int


init : () -> ( Model, Cmd Msg )
init _ =
    ( 0, Cmd.none )



-- UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( model + 1, Cmd.none )

        Decrement ->
            ( model - 1, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    main_ [ class "ly_cont" ]
        [ table []
            [ thead []
                [ tr []
                    [ th []
                        [ i [ class "fas fa-sort" ] []
                        , span [] [ text "First Name" ]
                        ]
                    , th []
                        [ i [ class "fas fa-sort" ] []
                        , span [] [ text "Last Name" ]
                        ]
                    , th
                        []
                        [ i [ class "fas fa-sort" ] []
                        , span [] [ text "Email" ]
                        ]
                    , th []
                        [ i [ class "fas fa-sort" ] []
                        , span [] [ text "Age" ]
                        ]
                    , th []
                        [ i [ class "fas fa-sort" ] []
                        , span [] [ text "Country" ]
                        ]
                    , th []
                        [ i [ class "fas fa-sort" ] []
                        , span [] [ text "Category" ]
                        ]
                    , th []
                        [ i [ class "fas fa-sort" ] []
                        , span [] [ text "Last Update" ]
                        ]
                    ]
                ]
            , tbody []
                [ tr []
                    [ td [] [ text "Addison" ]
                    , td [] [ text "Pikett" ]
                    , td [] [ text "apikett1r@over-blog.com" ]
                    , td [] [ text "59" ]
                    , td [] [ text "New Zealang" ]
                    , td [] [ text "A1" ]
                    , td [] [ text "2017-08-28" ]
                    ]
                , tr []
                    [ td [] [ text "Addison" ]
                    , td [] [ text "Pikett" ]
                    , td [] [ text "apikett1r@over-blog.com" ]
                    , td [] [ text "59" ]
                    , td [] [ text "New Zealang" ]
                    , td [] [ text "A1" ]
                    , td [] [ text "2017-08-28" ]
                    ]
                , tr []
                    [ td [] [ text "Addison" ]
                    , td [] [ text "Pikett" ]
                    , td [] [ text "apikett1r@over-blog.com" ]
                    , td [] [ text "59" ]
                    , td [] [ text "New Zealang" ]
                    , td [] [ text "A1" ]
                    , td [] [ text "2017-08-28" ]
                    ]
                , tr []
                    [ td [] [ text "Addison" ]
                    , td [] [ text "Pikett" ]
                    , td [] [ text "apikett1r@over-blog.com" ]
                    , td [] [ text "59" ]
                    , td [] [ text "New Zealang" ]
                    , td [] [ text "A1" ]
                    , td [] [ text "2017-08-28" ]
                    ]
                , tr []
                    [ td [] [ text "Addison" ]
                    , td [] [ text "Pikett" ]
                    , td [] [ text "apikett1r@over-blog.com" ]
                    , td [] [ text "59" ]
                    , td [] [ text "New Zealang" ]
                    , td [] [ text "A1" ]
                    , td [] [ text "2017-08-28" ]
                    ]
                ]
            ]
        , div [ class "bl-pagenation" ]
            [ button [] [ i [ class "fas fa-caret-left" ] [] ]
            , span [] [ text "Page ", strong [] [ text "2" ], text " of 20" ]
            , button [] [ i [ class "fas fa-caret-right" ] [] ]
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
