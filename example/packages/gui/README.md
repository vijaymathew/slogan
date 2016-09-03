A simple package for graphics programming.

````
gui_init()
let w = gui_open_window("hello, world", 100, 100, 650, 650)
let i = gui_open_bmp(w, "./images/hello.bmp")

function event_handler(event)
{ showln("event ", event, " received.")
  when (event == 0) gui_quit() }

gui_event_loop(#[event_handler])

gui_close_bmp(i)
gui_close_window(w)
gui_quit()
```