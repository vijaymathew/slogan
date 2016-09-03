#include <SDL.h>
#include <stdio.h>
#include "slogan.h"

typedef struct window_ {
  SDL_Window *win;
  SDL_Renderer *ren;
} window;

static void report_error(const char *prefix)
{
  printf("%s: %s\n", prefix, SDL_GetError());
}

int gui_init()
{
  if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
    report_error("SDL_Init");
    return 0;
  }
  return 1;
}

void gui_quit()
{
  SDL_Quit();
}

window *gui_open_window(___slogan_obj s_title, ___slogan_obj s_x, ___slogan_obj s_y,
			___slogan_obj s_w, ___slogan_obj s_h)
{
  window *gui_w = NULL;
  SDL_Renderer *ren;
  SDL_Window *win = SDL_CreateWindow(title, x, y, w, h, SDL_WINDOW_SHOWN);

  if (win == NULL) {
    report_error("SDL_CreateWindow");
    SDL_Quit();
    return NULL;
  }

  ren = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED | SDL_RENDERER_PRESENTVSYNC);
  if (ren == NULL) {
    SDL_DestroyWindow(win);
    report_error("SDL_CreateRenderer");
    SDL_Quit();
    return NULL;
  }
 
  gui_w = (window *) malloc(sizeof(window));
  if (gui_w == NULL) {
    printf("Failed to allocate window.\n");
    return NULL;
  }
  gui_w->win = win;
  gui_w->ren = ren;
  return gui_w;
}

void gui_render(window *w)
{
  SDL_Renderer *ren = w->ren;
  SDL_RenderClear(ren);
  SDL_RenderPresent(ren);
}

int gui_event_loop(___slogan_obj *args)
{
  int error = 0;
  ___slogan_obj result;
  ___slogan_obj f = args[0];
  SDL_Event event;
  int etype = 0;

  while (1) {
    while (SDL_PollEvent(&event)) {
      switch (event.type) {
      case SDL_QUIT:
	etype = 0;
	break;
      default:
	etype = event.type;
      }
      ___ON_THROW(result = ___call_fn(f, ___pair(___fix(etype), ___NUL)), error = 1);
      if (error == 1)
	return -1;
      if (result == 0)
	return 0;
    }
  }
  return 0;
}

void gui_close_window(window *w)
{
  SDL_DestroyRenderer(w->ren);
  SDL_DestroyWindow(w->win);
  free(w);
}

SDL_Texture *gui_open_bmp(window *w, const char *file_name)
{
  SDL_Texture *tex;
  SDL_Surface *bmp = SDL_LoadBMP(file_name);
  SDL_Renderer *ren = w->ren;
  
  if (bmp == NULL) {
    report_error("SDL_LoadBMP");
    return NULL;
  }

  tex = SDL_CreateTextureFromSurface(ren, bmp);
  SDL_FreeSurface(bmp);
  if (tex == NULL) {
    report_error("SDL_CreateTextureFromSurface");
    return NULL;
  }

  SDL_RenderClear(ren);
  SDL_RenderCopy(ren, tex, NULL, NULL);
  //Update the screen
  SDL_RenderPresent(ren);
  return tex;
}

void gui_close_bmp(SDL_Texture *tex)
{
  SDL_DestroyTexture(tex);
}
