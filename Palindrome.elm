import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

main = Html.beginnerProgram {model = "",
                             view = viewFunction,
                             update = updater}

type Msg = TextChanged String

viewFunction : String -> Html Msg
viewFunction s = div [] [input [onInput TextChanged] [],
                         text (s ++ (String.reverse s))]

updater msg s = case msg of
                    TextChanged s2 -> s2
