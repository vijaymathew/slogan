A simple package for graphics programming.

````
gui_init()
let w = gui_open_window("hello, world", 100, 100, 650, 650)
let i = gui_open_bmp(w, "./images/hello.bmp")

gui_close_bmp(i)
gui_close_window(w)
gui_quit()
```