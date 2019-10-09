import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

-- Coordinates in a Mercator projection
type alias Coord = { x: Float, y: Float }
type alias Model = { centre: Coord, zoom: Int }


init : Model
init = Model (Coord 0.0 0.3) 16



-- UPDATE


type Msg
  = ZoomIn
  | ZoomOut


update : Msg -> Model -> Model
update msg model =
  case msg of
    ZoomIn ->
      Model model.centre (model.zoom + 1)

    ZoomOut ->
      Model model.centre (model.zoom - 1)



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick ZoomOut ] [ text "-" ]
    , div [] [ text (String.fromInt model.zoom ) ]
    , button [ onClick ZoomIn ] [ text "+" ]
    ]
