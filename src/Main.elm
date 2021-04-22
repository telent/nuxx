module Main exposing (view)

import Browser
import Html exposing (Html, button, div, span, text, img, pre)
import Html.Attributes as H exposing (src, style, width, height)
import Html.Events exposing (onClick)
import Html.Events.Extra.Pointer as Pointer
import Maybe exposing (Maybe)
import Http
import Json.Decode as D
import Svg exposing (Svg, svg, rect, circle, g)
import Svg.Attributes as S exposing
    ( viewBox
    , x, y
    , r, rx, ry
    , cx, cy
    , fill
    , stroke, strokeWidth, strokeOpacity)



-- MAIN


main =
  Browser.element { init = init
                  , update = update
                  , subscriptions = subscriptions
                  , view = view }



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

pixelsToCoord z (x,y) =
    let x_float = toFloat x / toFloat ( 2 ^ (z + 8))
        y_float = toFloat y / toFloat ( 2 ^ (z + 8))
    in Coord x_float y_float

reflect : Coord -> Coord
reflect c = Coord -c.x -c.y

-- translate : a -> a -> a
translate base offset =
    { base | x = (base.x + offset.x), y = (base.y + offset.y) }

translatePixels : Coord -> Zoom -> (Int, Int) -> Coord
translatePixels old z (x, y) = translate old (pixelsToCoord z (x, y))


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
    let delta = pixelsToCoord z ((width // 2), (height // 2))
        minCoord = translate centre (reflect delta)
        maxCoord = translate centre delta
    in ((tileCovering minCoord z),
        (translate (tileCovering maxCoord z) (TileNumber 1 1)))

-- MODEL

type Drag
    = None
    | Dragging  (Int, Int)  (Int, Int)

dragTo : Drag ->  (Int, Int) -> Drag
dragTo d dest =
    case d of
        None -> None
        Dragging from to -> Dragging from dest

dragDelta d =
    case d of
        None -> (0,0)
        Dragging (fx,fy) (tx,ty) -> (fx-tx, fy-ty)

type alias Model =
    { centre: Coord
    , zoom: Zoom
    , drag: Drag
    , track: List Trackpoint }

init : () -> (Model, Cmd Msg)
init _ = (Model (toCoord 51 0) 16 None [], fetch)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

fetch = Http.get
        { url = "//localhost:8001/activities"
        , expect = Http.expectJson Loaded trackDecoder
        }

type alias Trackpoint =
    { ts : String
    , ele : Int
    }

pointDecoder : D.Decoder Trackpoint
pointDecoder = D.map2 Trackpoint
               (D.field "ts" D.string)
               (D.field "ele" D.int)

trackDecoder : D.Decoder (List Trackpoint)
trackDecoder = D.list pointDecoder

-- UPDATE

type Msg
  = ZoomIn
  | ZoomOut
  | Scroll Int Int
  | PointerDown (Int, Int)
  | PointerMove (Int, Int)
  | PointerUp (Int, Int)
  | Loaded (Result  Http.Error (List Trackpoint))


update : Msg -> Model -> (Model, Cmd Msg)
update msg model = ((update_ msg model), Cmd.none)

update_ msg model =
  case msg of
    ZoomIn ->
      { model | zoom = model.zoom + 1 }

    ZoomOut ->
      { model | zoom = model.zoom - 1 }

    Scroll x y ->
      { model | centre = translatePixels model.centre model.zoom (x,y) }

    PointerDown (x,y) ->
      { model | drag = Dragging (x,y) (x,y) }

    PointerMove (x,y) ->
      { model | drag = dragTo model.drag (x,y) }

    PointerUp (x,y) ->
      { model | drag = None,
                centre = translatePixels model.centre model.zoom (dragDelta model.drag) }

    Loaded result ->
        case result of
            Ok track -> { model | track = track }
            Err _ -> model

-- VIEW

tileUrl : TileNumber -> Zoom -> String
tileUrl {x,y} z =
    String.concat ["https://a.tile.openstreetmap.org",
                       "/", String.fromInt z,
                       "/", String.fromInt x,
                       "/", String.fromInt y,
                       ".png" ]

tileImg zoom tilenumber = img [ width 256,
                                height 256,
                                src (tileUrl tilenumber zoom) ] []

trackView : List Trackpoint -> Svg Msg
trackView points =
    svg
    [H.style "width" "100%"
    ,H.style "height" "100%"
    ,H.style "position" "absolute"
    ]
    [ g
      [fill "none"
      ,stroke "blue"
      ,strokeWidth "7"
      ,strokeOpacity "0.5"]
      [ rect
        [ x "10"
        , y "10"
        , S.width "100"
        , S.height "100"
        , rx "15"
        , ry "15"
        ]
        []
      , circle
        [ cx "50"
        , cy "50"
        , r "50"
        ]
        []
    ]]

px x = String.fromInt x ++ "px"

tiles xs ys zoom =
    List.map
        (\ y -> div []
             (List.map (\ x -> tileImg zoom (TileNumber x y)) xs))
        ys


canvas centre zoom width height track =
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
        epos e = Tuple.mapBoth floor floor e.pointer.clientPos
    in  div [style "position" "absolute"
            ,style "width" (px pixWidth)
            ,style "height" (px pixHeight)
            ,style "left" (px -offsetX)
            ,style "top" (px -offsetY)
            ,style "lineHeight" (px 0)
            ,Pointer.onUp (\e -> PointerUp (epos e))
            ,Pointer.onMove (\e -> PointerMove (epos e))
            ,Pointer.onDown (\e -> PointerDown (epos e)) ]
        (trackView track :: tiles xs ys zoom)

portalWidth = 600
portalHeight = 600

view : Model -> Html Msg
view model =
    let coord = translate model.centre (pixelsToCoord model.zoom (dragDelta model.drag))
        canvasV = canvas coord model.zoom portalWidth portalHeight model.track
    in div []
        [ (div [ style "width" (px portalWidth)
               , style "height" (px portalHeight)
               , style "display" "inline-block"
               , style "position" "relative"
               , style "overflow" "hidden"]
               [canvasV])
        , div [] [ text (String.fromInt model.zoom ) ]
        , button [ onClick ZoomOut ] [ text "-" ]
        , button [ onClick ZoomIn ] [ text "+" ]
        , button [ onClick (Scroll 0 -10) ] [ text "^" ]
        , button [ onClick (Scroll 0 10) ] [ text "V" ]
        , button [ onClick (Scroll -10 0) ] [ text "<" ]
        , button [ onClick (Scroll 10 0) ] [ text ">" ]
--        , div [] [ text (Debug.toString (List.length model.track)) ]
        ]
