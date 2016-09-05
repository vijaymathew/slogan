A simple package for graphics programming.

````
gui_init()
let w = gui_open_window("hello, world", 100, 100, 650, 650)
let i = gui_open_bmp(w, "./images/hello.bmp")

function quit()
{ gui_close_bmp(i)
  gui_close_window(w)
  gui_quit() }

let running = true;

function handler(event)
  if (event == 0) running = false
  else true

let loop()
{ gui_event(handler)
  if (running) loop()
  else quit() }
```

This package has a dependency on SDL2.
Install the dependencies:

### Ubuntu:
sudo apt-get install libsdl2-dev libsdl2-image-dev \
libsdl2-mixer-dev libsdl2-ttf-dev libjpeg-dev libpng12-dev

### Mac OS X:
brew install sdl2 sdl2_gfx sdl2_image sdl2_mixer sdl2_ttf
