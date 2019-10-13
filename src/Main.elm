import Browser
import Html exposing (Html, button, div, span, text, img)
import Html.Attributes exposing (src, style)
import Html.Events exposing (onClick)



-- MAIN


main =
  Browser.sandbox { init = init, update = update, view = view }



-- MATHS

-- Coordinates in a Mercator projection
type alias Coord = { x: Float, y: Float }

-- zoom level
type alias Zoom = Int

type alias TileNumber = { x: Int, y: Int }

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

pixelsToCoord z x y =
    let x_float = toFloat x / toFloat ( 2 ^ (z + 8))
        y_float = toFloat y / toFloat ( 2 ^ (z + 8))
    in Coord x_float y_float

reflect : Coord -> Coord
reflect c = Coord -c.x -c.y

-- translate : a -> a -> a
translate base offset =
    { base | x = (base.x + offset.x), y = (base.y + offset.y) }

translatePixels : Coord -> Zoom -> Int -> Int -> Coord
translatePixels old z x y = translate old (pixelsToCoord z x y)


tileCovering : Coord -> Zoom -> TileNumber
tileCovering c z =
    TileNumber (truncate (toFloat (2 ^ z) * c.x)) (truncate (toFloat (2 ^ z) * c.y))

pixelFromCoord : Coord -> Zoom -> (Int, Int)
pixelFromCoord c z =
    let {x,y} = tileCovering c (z + 8)
    in (x,y)

boundingTiles : Coord -> Zoom -> Int -> Int -> (TileNumber, TileNumber)
boundingTiles centre z width height =
    -- find the tiles needed to cover the area (`width` x `height`)
    -- about the point at `centre`
    let delta = pixelsToCoord z (width // 2) (height // 2)
        minCoord = translate centre (reflect delta)
        maxCoord = translate centre delta
    in ((tileCovering minCoord z),
        (translate (tileCovering maxCoord z) (TileNumber 1 1)))

-- MODEL

type alias Model = { centre: Coord, zoom: Zoom }
init : Model
init = Model (toCoord 51.5 0.0) 16


-- UPDATE

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

tileUrl : TileNumber -> Zoom -> String
tileUrl {x,y} z =
    String.concat ["https://a.tile.openstreetmap.org",
                       "/", String.fromInt z,
                       "/", String.fromInt x,
                       "/", String.fromInt y,
                       ".png" ]

tileImg zoom tilenumber = img [ src (tileUrl tilenumber zoom) ] []

px x = String.fromInt x ++ "px"

canvas centre zoom width height =
    let (mintile, maxtile) = boundingTiles centre zoom width height
        -- offset is pixel difference between centre (which *should*
        -- be the middle of the image) and actual middle of the canvas
        (pixelCentreX,pixelCentreY) = pixelFromCoord centre zoom
        leftedge = mintile.x * 256
        topedge = mintile.y * 256
        offsetX = pixelCentreX - (width // 2) - leftedge
        offsetY = pixelCentreY - (height // 2) - topedge
        pixWidth = (1 + maxtile.x - mintile.x) * 256
        pixHeight = (1 + maxtile.y - mintile.y) * 256
        xs = List.range mintile.x maxtile.x
        ys = List.range mintile.y maxtile.y
    in  div [style "position" "absolute"
            ,style "width" (px pixWidth)
            ,style "height" (px pixHeight)
            ,style "left" (px -offsetX)
            ,style "top" (px -offsetY)
            ,style "lineHeight" (px 0)]
        (List.map
             (\ y -> div []
                     (List.map (\ x -> tileImg zoom (TileNumber x y)) xs))
             ys)

portalWidth = 600
portalHeight = 600


view : Model -> Html Msg
view model =
    let coord = model.centre
        tiles = canvas coord model.zoom portalWidth portalHeight
    in div []
        [ (div [ style "width" (px portalWidth)
               , style "height" (px portalHeight)
               , style "display" "inline-block"
               , style "position" "relative"
               , style "overflow" "hidden"]
               [tiles])
        , div [] [ text (String.fromInt model.zoom ) ]
        , button [ onClick ZoomOut ] [ text "-" ]
        , button [ onClick ZoomIn ] [ text "+" ]
        , button [ onClick (Scroll 0 -10) ] [ text "^" ]
        , button [ onClick (Scroll 0 10) ] [ text "V" ]
        , button [ onClick (Scroll -10 0) ] [ text "<" ]
        , button [ onClick (Scroll 10 0) ] [ text ">" ]
        ]
