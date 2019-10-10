import Browser
import Html exposing (Html, button, div, text, img)
import Html.Attributes exposing (src)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MATHS

-- Coordinates in a Mercator projection
type alias Coord = { x: Float, y: Float }

-- zoom level
type alias Zoom = Int

type alias Model = { centre: Coord, zoom: Zoom }

type alias TileNumber = (Int, Int)

type alias Lat = Float
type alias Lng = Float

-- project latling to co-ordinates based on pseudocode at
-- https://wiki.openstreetmap.org/wiki/Slippy_map_tilenames#Zoom_levels

sec x = 1 / (cos x)

toCoord : Lat -> Lng -> Coord
toCoord lat lng =
    let
        lat_rad = lat * pi / 180
        x = (lng + 180) / 360
        y = (1 - (logBase e ((tan lat_rad) + (sec lat_rad))) / pi) / 2
    in
        Coord x y

tileCovering : Coord -> Zoom -> TileNumber
tileCovering c z = (truncate (toFloat (2 ^ z) * c.x),
                    truncate (toFloat (2 ^ z) * c.y))

tileUrl : TileNumber -> Zoom -> String
tileUrl (x,y) z =
    String.concat ["https://a.tile.openstreetmap.org",
                       "/", (String.fromInt z),
                       "/", String.fromInt x,
                       "/", String.fromInt y,
                       ".png" ]

-- MODEL

init : Model
init = Model (toCoord 51.5 0.0) 16


-- UPDATE

-- this could be two functions: turn pixels into Coordinates,
-- and sum Coordinates
translatePixels : Coord -> Zoom -> Int -> Int -> Coord
translatePixels old z x y =
    let x_float = toFloat x / toFloat ( 2 ^ (z + 8))
        y_float = toFloat y / toFloat ( 2 ^ (z + 8))
    in Coord (old.x + x_float) (old.y + y_float)

type Msg
  = ZoomIn
  | ZoomOut
  | Scroll Int Int

update : Msg -> Model -> Model
update msg model =
  case msg of
    ZoomIn ->
      Model model.centre (model.zoom + 1)

    ZoomOut ->
      Model model.centre (model.zoom - 1)

    Scroll x y ->
      Model (translatePixels model.centre model.zoom x y) model.zoom


-- VIEW


view : Model -> Html Msg
view model =
    let coord = model.centre
        tile = tileCovering coord model.zoom
        d = Debug.log "hey" (coord, tile)
    in div []
        [ button [ onClick ZoomOut ] [ text "-" ]
        , img [ src (tileUrl tile model.zoom) ] []
        , div [] [ text (String.fromInt model.zoom ) ]
        , button [ onClick ZoomIn ] [ text "+" ]
        , button [ onClick (Scroll 0 -10) ] [ text "^" ]
        , button [ onClick (Scroll 10 0) ] [ text ">" ]
        , button [ onClick (Scroll 0 10) ] [ text "V" ]
        , button [ onClick (Scroll -10 0) ] [ text "<" ]
        ]
