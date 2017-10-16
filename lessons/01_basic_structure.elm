import Html exposing (Html, div, input, strong, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

main =
  Html.beginnerProgram{ model=model, update=update, view=view }

-- model
type alias Model = {
  text: String
}
model : Model
model =
  Model ""

-- update
type Msg =
  Update String

update : Msg -> Model -> Model
update msg model =
  case msg of
      Update newText -> { model | text = newText }


-- view

view : Model -> Html Msg
view model =
  div []
    [
      input [ placeholder "Put data here", onInput Update ] [ text ("Initial") ],
      div [] [
        strong [] [ text (String.reverse model.text) ]
      ]
    ]
