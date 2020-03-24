module Main exposing (main)

import Browser
import Html
    exposing
        ( Html
        , button
        , div
        , i
        , main_
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
import Json.Decode as JD



-- MAIN


main : Program JD.Value Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type Column
    = FirstName
    | LastName
    | Email
    | Age
    | Country
    | Category
    | LastUpdate


type SortTarget
    = SortTarget Column SortDirection


type SortDirection
    = Asc
    | Desc


type alias Model =
    { personList : List Person
    , sortTarget : SortTarget
    , numOfPage : Int
    }


init : JD.Value -> ( Model, Cmd Msg )
init tableData =
    let
        personListResult =
            JD.decodeValue
                decoderPersonList
                tableData
    in
    ( { personList =
            case personListResult of
                Ok pList ->
                    pList

                Err _ ->
                    []
      , sortTarget = SortTarget FirstName Asc
      , numOfPage = 1
      }
    , Cmd.none
    )


type alias Person =
    { firstName : String
    , lastName : String
    , email : String
    , age : Int
    , country : String
    , category : String
    , lastUpdate : String
    }


decoderPersonList : JD.Decoder (List Person)
decoderPersonList =
    JD.list decodePerson


decodePerson : JD.Decoder Person
decodePerson =
    JD.map7 Person
        (JD.field "first_name" JD.string)
        (JD.field "last_name" JD.string)
        (JD.field "email" JD.string)
        (JD.field "age" JD.int)
        (JD.field "country" JD.string)
        (JD.field "category" JD.string)
        (JD.field "last_update" JD.string)



-- UPDATE


type Msg
    = SortFirstName
    | SortLastName
    | SortEmail
    | SortAge
    | SortCountry
    | SortCategory
    | SortLastUpdate
    | PrevPage
    | NextPage


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        { sortTarget, numOfPage } =
            model

        toggleDirection direction =
            case direction of
                Asc ->
                    Desc

                Desc ->
                    Asc

        sortColumn column =
            case sortTarget of
                SortTarget clmn direction ->
                    if column == clmn then
                        SortTarget column <| toggleDirection direction

                    else
                        SortTarget column Asc
    in
    case msg of
        SortFirstName ->
            ( { model
                | sortTarget = sortColumn FirstName
                , numOfPage = 1
              }
            , Cmd.none
            )

        SortLastName ->
            ( { model
                | sortTarget = sortColumn LastName
                , numOfPage = 1
              }
            , Cmd.none
            )

        SortEmail ->
            ( { model
                | sortTarget = sortColumn Email
                , numOfPage = 1
              }
            , Cmd.none
            )

        SortAge ->
            ( { model
                | sortTarget = sortColumn Age
                , numOfPage = 1
              }
            , Cmd.none
            )

        SortCountry ->
            ( { model
                | sortTarget = sortColumn Country
                , numOfPage = 1
              }
            , Cmd.none
            )

        SortCategory ->
            ( { model
                | sortTarget = sortColumn Category
                , numOfPage = 1
              }
            , Cmd.none
            )

        SortLastUpdate ->
            ( { model
                | sortTarget = sortColumn LastUpdate
                , numOfPage = 1
              }
            , Cmd.none
            )

        PrevPage ->
            ( { model
                | numOfPage = numOfPage - 1
              }
            , Cmd.none
            )

        NextPage ->
            ( { model
                | numOfPage = numOfPage + 1
              }
            , Cmd.none
            )


sortDirectionCompare : SortDirection -> comparable -> comparable -> Order
sortDirectionCompare direction a b =
    case direction of
        Asc ->
            compare a b

        Desc ->
            case compare a b of
                LT ->
                    GT

                EQ ->
                    EQ

                GT ->
                    LT



-- VIEW


view : Model -> Html Msg
view { numOfPage, personList, sortTarget } =
    let
        sortTargetToClass column =
            case sortTarget of
                SortTarget clmn direction ->
                    "fas "
                        ++ (if column == clmn then
                                directionToClass direction

                            else
                                "fa-sort"
                           )

        directionToClass direction =
            "active "
                ++ (case direction of
                        Asc ->
                            "fa-caret-up"

                        Desc ->
                            "fa-caret-down"
                   )

        limit =
            5

        maxPage =
            List.length personList // limit
    in
    main_ [ class "ly_cont" ]
        [ table []
            [ thead []
                [ tr []
                    [ th [ onClick SortFirstName ]
                        [ i
                            [ class <| sortTargetToClass FirstName
                            ]
                            []
                        , span [] [ text "First Name" ]
                        ]
                    , th [ onClick SortLastName ]
                        [ i
                            [ class <| sortTargetToClass LastName
                            ]
                            []
                        , span [] [ text "Last Name" ]
                        ]
                    , th
                        [ onClick SortEmail ]
                        [ i
                            [ class <| sortTargetToClass Email
                            ]
                            []
                        , span [] [ text "Email" ]
                        ]
                    , th [ onClick SortAge ]
                        [ i
                            [ class <| sortTargetToClass Age
                            ]
                            []
                        , span [] [ text "Age" ]
                        ]
                    , th [ onClick SortCountry ]
                        [ i
                            [ class <| sortTargetToClass Country
                            ]
                            []
                        , span [] [ text "Country" ]
                        ]
                    , th [ onClick SortCategory ]
                        [ i [ class <| sortTargetToClass Category ] []
                        , span [] [ text "Category" ]
                        ]
                    , th [ onClick SortLastUpdate ]
                        [ i [ class <| sortTargetToClass LastUpdate ] []
                        , span [] [ text "Last Update" ]
                        ]
                    ]
                ]
            , tbody [] <|
                (personList
                    |> List.sortWith
                        (\a b ->
                            case sortTarget of
                                SortTarget FirstName dir ->
                                    sortDirectionCompare dir a.firstName b.firstName

                                SortTarget LastName dir ->
                                    sortDirectionCompare dir a.lastName b.lastName

                                SortTarget Email dir ->
                                    sortDirectionCompare dir a.email b.email

                                SortTarget Age dir ->
                                    sortDirectionCompare dir a.age b.age

                                SortTarget Country dir ->
                                    sortDirectionCompare dir a.country b.country

                                SortTarget Category dir ->
                                    sortDirectionCompare dir a.category b.category

                                SortTarget LastUpdate dir ->
                                    sortDirectionCompare dir a.lastUpdate b.lastUpdate
                        )
                    -- Pagination
                    |> List.drop ((numOfPage - 1) * limit)
                    |> List.take limit
                    -- Table
                    |> List.map
                        (\{ firstName, lastName, email, age, country, category, lastUpdate } ->
                            tr []
                                [ td [] [ text firstName ]
                                , td [] [ text lastName ]
                                , td [] [ text email ]
                                , td [] [ text <| String.fromInt age ]
                                , td [] [ text country ]
                                , td [] [ text category ]
                                , td [] [ text lastUpdate ]
                                ]
                        )
                )
            ]
        , div [ class "bl-pagenation" ]
            [ if numOfPage - 1 > 0 then
                button [ onClick PrevPage ] [ i [ class "fas fa-caret-left" ] [] ]

              else
                text ""
            , span [] [ text "Page ", strong [] [ text <| String.fromInt numOfPage ], text <| " of " ++ String.fromInt maxPage ]
            , if numOfPage < maxPage then
                button [ onClick NextPage ] [ i [ class "fas fa-caret-right" ] [] ]

              else
                text ""
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none
