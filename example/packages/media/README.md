A simple package for graphics programming.

````
let m = media
m.init()
let w = m.open_window("hello, world", 100, 100, 650, 650)
let i = m.open_bmp(w, "./images/hello.bmp")

function quit()
{ m.close_bmp(i)
  m.close_window(w)
  m.quit() }

let running = true;

function handler(event)
  if (event == 0) running = false
  else true

let loop()
{ m.event(handler)
  if (running) loop()
  else quit() }
```

Remember to install the SDL2 dependencies before building the package:

### Ubuntu

```
sudo apt-get install libsdl2-dev libsdl2-image-dev \
libsdl2-mixer-dev libsdl2-ttf-dev libjpeg-dev libpng12-dev
```

### Mac OS X

```
brew install sdl2 sdl2_gfx sdl2_image sdl2_mixer sdl2_ttf
```