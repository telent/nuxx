import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MODEL

-- Coordinates in a Mercator projection
type alias Coord = { x: Float, y: Float }

-- zoom level
type alias Zoom = Int

type alias Lat = Float
type alias Lng = Float

sec x = 1 / (cos x)

toCoord : Lat -> Lng -> Coord
toCoord lat lng =
    let
        lat_rad = lat * pi / 180
        x = (lng + 180) / 360
        y = (1 - (logBase e ((tan lat_rad) + (sec lat_rad))) / pi) / 2
    in
        Coord x y


init : Model
init = Model (toCoord 51.5 0.0) 16



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
