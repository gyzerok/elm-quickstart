module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


main =
    Html.beginnerProgram { model = init, update = update, view = view }



-- model


type alias Model =
    { submitted : List Entry

    -- form
    , firstName : String
    , lastName : String
    , age : Int
    }


type alias Entry =
    { firstName : String
    , lastName : String
    , age : Int
    }


init : Model
init =
    { firstName = ""
    , lastName = ""
    , age = 0
    , submitted = []
    }



-- update


type Action
    = FirstNameChanged String
    | LastNameChanged String
    | AgeChanged String
    | ClearClicked
    | FormSubmitted


update : Action -> Model -> Model
update action model =
    case action of
        FirstNameChanged val ->
            { model | firstName = val }

        LastNameChanged val ->
            { model | lastName = val }

        AgeChanged val ->
            { model
                | age =
                    val
                        |> String.toInt
                        |> Result.withDefault model.age
            }

        ClearClicked ->
            { init | submitted = model.submitted }

        FormSubmitted ->
            { init
                | submitted =
                    model.submitted
                        ++ [ { firstName = model.firstName
                             , lastName = model.lastName
                             , age = model.age
                             }
                           ]
            }



-- view


view : Model -> Html Action
view model =
    section [ sectionStyle ]
        [ div [ id "foo-form" ] [ formView model ]
        , div [ id "all-forms" ] [ formList model.submitted ]
        ]


formView : Model -> Html Action
formView model =
    Html.form [ formStyle, onSubmit FormSubmitted ]
        [ span [ style [ ( "margin-right", "2px" ) ] ]
            [ input
                [ type_ "text"
                , required True
                , value model.firstName
                , placeholder "FirstName"
                , onInput FirstNameChanged
                ]
                []
            ]
        , span [ style [ ( "margin-right", "2px" ) ] ]
            [ input
                [ type_ "text"
                , required True
                , value model.lastName
                , placeholder "LastName"
                , onInput LastNameChanged
                ]
                []
            ]
        , span []
            [ input
                [ type_ "number"
                , required True
                , Html.Attributes.min "1"
                , Html.Attributes.max "100"
                , value (toString model.age)
                , placeholder "Age"
                , onInput AgeChanged
                ]
                []
            ]
        , div [ id "foo-form-actions", style [ ( "margin-left", "10px" ) ] ]
            [ div []
                [ button [ type_ "submit" ] [ text "Submit" ]
                , button [ type_ "reset", onClick ClearClicked ] [ text "Clear" ]
                ]
            ]
        ]


formList : List Entry -> Html Action
formList entries =
    div [ style [ ( "float", "left" ), ( "max-width", "100%" ) ] ]
        [ h3 [] [ text "All entries" ]
        , ul []
            (entries
                |> List.map
                    (\entry ->
                        li []
                            [ b [] [ text entry.firstName ]
                            , span [] [ text " " ]
                            , b [] [ text entry.lastName ]
                            , span [] [ text " " ]
                            , b [] [ text (toString entry.age) ]
                            ]
                    )
            )
        ]



-- styles


sectionStyle : Attribute msg
sectionStyle =
    style
        [ ( "visibility", "visible" )
        , ( "display", "inline-block" )
        ]


formStyle : Attribute msg
formStyle =
    style
        [ ( "display", "inline-flex" )
        , ( "padding", "2px" )
        , ( "width", "25%" )
        ]
