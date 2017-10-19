module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Lazy exposing (lazy)


main =
    Html.beginnerProgram { model = model, update = update, view = view }



-- model


type alias Entry =
    { firstName : String
    , lastName : String
    , age : Int
    }


type alias Model =
    { current : Entry
    , submitted : List Entry
    }



-- full model


model : Model
model =
    { current = { age = 0, firstName = "", lastName = "" }, submitted = [] }


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


saveForm : Model -> Model
saveForm model =
    { model
        | submitted = model.submitted ++ [ model.current ]
        , current = model.current |> clearForm
    }


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
            saveForm model



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
        [
            ( "display", "inline-flex" ),
            ("padding", "2px"),
            ("width", "25%")
        ]


view : Model -> Html Action
view model =
    section [ sectionStyle ]
        [ div [ id "foo-form" ] [ lazy formView model ]
        , div [ id "all-forms" ] [ lazy formList model.submitted ]
        ]


formView : Model -> Html Action
formView model =
    Html.form [ formStyle, onSubmit Submit]
        [
            span [ style [("margin-right", "2px")] ] [
                input [
                    type_ "text",
                    required True,
                    value model.current.firstName,
                    placeholder "FirstName",
                    onInput UpdateFirstName
                ] []
            ],
            span [ style [("margin-right", "2px")] ] [
                input [
                    type_ "text",
                    required True,
                    value model.current.lastName,
                    placeholder "LastName",
                    onInput UpdateLastName ] []
            ] ,
            span [] [
                input [
                    type_ "number",
                    required True,
                    Html.Attributes.min "1",
                    Html.Attributes.max "100",
                    value (toString model.current.age),
                    placeholder "Age",
                    onInput UpdateAge ] []
            ],
            div [ id "foo-form-actions", style [("margin-left", "10px")] ] [ lazy formActions model ]
        ]


formActions : model -> Html Action
formActions model =
    div [] [
            button [ type_ "submit" ] [ text "Submit" ],
            button [ type_ "reset", onClick Clear ] [ text "Clear" ]
        ]


formList : List Entry -> Html Action
formList entries =
    div [ style[ ("float", "left"), ("max-width", "100%") ] ]
        [ h3 [] [ text "All entries" ]
        , ul [] (List.map formListItem entries)
        ]


formListItem : Entry -> Html Action
formListItem entry =
    li []
        [ b [] [ text entry.firstName ]
        , span [] [ text " " ]
        , b [] [ text entry.lastName ]
        , span [] [ text " " ]
        , b [] [ text (toString entry.age) ]
        ]
