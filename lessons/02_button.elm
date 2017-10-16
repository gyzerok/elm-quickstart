import Html exposing (Html, div, section, input, strong, text, button)
import Html.Events exposing (onClick)

main =
  Html.beginnerProgram{ model=model, update=update, view=view }

-- model
type alias Model = {
  action: String
}
model : Model
model =
  Model ""

type Action = Foo | Bar

-- update
update : Action -> Model -> Model
update msg model =
    case msg of
        Foo -> { model | action = "Foo" }
        Bar -> { model | action = "Bar" }


-- view
view : Model -> Html Action
view model =
    section [] [
      div [] [
        button [ onClick Foo ] [ text "Foo"],
        button [ onClick Bar ] [ text "Bar"]
      ],
      div [] [
        strong [] [ text(model.action) ]
      ]
    ]
