module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)


main =
    Html.beginnerProgram { model = model, update = update, view = view }



-- model
-- list of all entries to be displayed


type alias Entries =
    { forms : List Entry
    }



-- currently active entry


type alias Entry =
    { firstName : String
    , lastName : String
    , age : Int
    }


type alias Model =
    { current : Entry
    , submitted : Entries
    }



-- full model


model : Model
model =
    { current = { age = 0, firstName = "", lastName = "" }, submitted = { forms = [] } }


setFirstName : String -> Entry -> Entry
setFirstName val entry =
    { entry | firstName = val }


setLastName : String -> Entry -> Entry
setLastName val entry =
    { entry | lastName = val }


setAge : Int -> Entry -> Entry
setAge val entry =
    { entry | age = val }


clearForm : Entry -> Entry
clearForm entry =
    { entry | firstName = "", lastName = "", age = 0 }



-- update


type Action
    = NoOp
    | UpdateFirstName String
    | UpdateLastName String
    | UpdateAge String
    | Clear
    | Submit


update : Action -> Model -> Model
update action model =
    case action of
        NoOp ->
            model

        UpdateFirstName val ->
            let
                current =
                    model.current

                new =
                    current |> setFirstName val
            in
            { model
                | current = new
            }

        UpdateLastName val ->
            let
                current =
                    model.current

                new =
                    current |> setLastName val
            in
            { model
                | current = new
            }

        UpdateAge val ->
            let
                current =
                    model.current

                age =
                    String.toInt val |> Result.toMaybe |> Maybe.withDefault 0

                new =
                    current |> setAge age
            in
            { model
                | current = new
            }

        Clear ->
            let
                current =
                    model.current

                new =
                    current |> clearForm
            in
            { model
                | current = new
            }

        Submit ->
            model



-- view


sectionStyle : Attribute msg
sectionStyle =
    style
        [ ( "visibility", "visible" )
        , ( "display", "inline-block" )
        ]


formStyle : Attribute msg
formStyle =
    style
        [ ( "display", "inline-grid" )
        ]


view : Model -> Html Action
view model =
    section [ sectionStyle ]
        [ div [ id "foo-form" ] [ lazy formView model ]
        , div [ id "foo-form-actions" ] [ lazy formActions model ]
        , div [ id "all-forms" ] [ lazy formList model.submitted ]
        ]


formView : Model -> Html Action
formView model =
    Html.form [ formStyle ]
        [ span []
            [ input [ type_ "text", value model.current.firstName, placeholder "FirstName", onInput UpdateFirstName ] []
            ]
        , span []
            [ input [ type_ "text", value model.current.lastName, placeholder "LastName", onInput UpdateLastName ] []
            ]
        , span []
            [ input [ type_ "number", value (toString model.current.age), placeholder "Age", onInput UpdateAge ] []
            ]
        ]


formActions : model -> Html Action
formActions model =
    div []
        [ button [ onClick Submit ] [ text "Submit" ]
        , button [ onClick Clear ] [ text "Clear" ]
        ]


formList : Entries -> Html Action
formList entries =
    ul [] (List.map formListItem entries.forms)


formListItem : Entry -> Html Action
formListItem entry =
    li []
        [ span [] [ text entry.firstName ]
        , span [] [ text entry.lastName ]
        , span [] [ text (toString entry.age) ]
        ]
