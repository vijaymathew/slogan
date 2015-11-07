The `media` library provides access to video, audio and other multimedia hardware facilities.

Building the library
====================

Export the environment variable `SLOGAN_ROOT` to point to the Slogan parent directory.
`media` has a dependency on SDL. Make sure the SDL headers and development library is installed.
Build the shared library by running the `build` script in this folder.

Load and initialize the core-library
======================================

````
load "$SLOGAN_ROOT/lib/media/core";

import media_core;
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
define r = create_renderer(w, -1, %[software]);
set_render_color(r, 0, 0, 0, 255);
render_clear(r);
// draw some shapes here
render_present(r);
````

Display an image in the window
------------------------------

Put this code between `render_clear` and `render_present`:

````
define img = load_image(r, "image.bmp");
render_copy(r, img, 0, 0);
````

Draw some simple shapes on the Window's surface
------------------------------------------------

````
define s = get_window_surface(w);
line(s, !red, 10 : 10, 100 : 200);
ellipse(s, !green, [20, 30, 150, 250]);
circle(s, !blue, 45, 50, 60);
rect(s, [255, 255, 0, 255], [20, 30, 100, 100], 10);
update_window_surface(w);
````

Render some text
----------------

````
text_init();
define font = text_open_font("/Library/Fonts/Verdana.ttf", 16);
define surface = text_on_solid_surface(font, "hello, world", [255, 0, 0, 255]);
define texture = texture_from_surface(r, surface);
destroy_surface(surface);
render_copy(r, texture, 100, 50);
````

Destroy the window and renderer after use
-----------------------------------------

````
destroy_window(w);
destroy_renderer(r);
````

Render graphics using the higher-level functions
------------------------------------------------

````
load "$SLOGAN_ROOT/lib/media/media";
load "$SLOGAN_ROOT/lib/media/graphics";

import media_graphics;

define w = window(500, 500);
define c = window_canvas(w);
point(c, 20, 20);
point(c, 30, 30);
point(c, 40, 40);
point(c, 50, 50);
point(c, 60, 60);
color(c, [255, 0, 0, 255]);
line(c, 10, 10, 10, 100);
line(c, 10, 10, 100, 10);
line(c, 100, 10, 100, 100);
line(c, 10, 100, 100, 100);
circle(c, 100, 100, 50);
ellipse(c, 200, 250, 100, 120);
redraw(c);
````