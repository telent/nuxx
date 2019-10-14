# My First Elm program

A "Slippy Maps" app (see also, Google Maps, leaflet.js) that puts
openstreetmap maps on the page and lets you scroll around by dragging.

## Try it

   elm reactor
   # browse to src/Main.elm

## Build it for "Production"

This uses the uglify-js NPM module to minify the Elm code

   npm install
   make
   # open index.html in a browser
   