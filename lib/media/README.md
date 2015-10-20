The `media` library provides access to video, audio and other multimedia hardware facilities.

Building the library
====================

Export the environment variable `SLOGAN_ROOT` to point to the Slogan parent directory.
`media` has a dependency on SDL. Make sure the SDL headers and development library is installed.
Build the shared library by running the `build` script in this folder.

Load and initialize the crypto-library
======================================

````
load "$SLOGAN_ROOT/lib/media/core";

import media;
initialize();

// Do multimedia stuff here

release();
````

Examples
========

Create and show a simple window
-------------------------------

````
define w = create_window("hello", -1, -1, 500, 500);
define r = create_renderer(w, -1);
set_render_draw_color(r, 0, 0, 0, 255);
render_clear(r);
render_present(r);
````

Display an image in the window
------------------------------

Put this code between `render_clear` and `render_present`:

````
define img = load_image(r, "image.bmp");
render_copy(r, img, 0, 0);
````

Destroy the window and renderer after use
-----------------------------------------

````
destroy_window(w);
destroy_renderer(r);
````

