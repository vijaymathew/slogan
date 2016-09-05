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

int media_init()
{
  if (SDL_Init(SDL_INIT_EVERYTHING) != 0) {
    report_error("SDL_Init");
    return 0;
  }
  return 1;
}

void media_quit()
{
  SDL_Quit();
}

window *media_open_window(___slogan_obj *args)
{
  window *media_w = NULL;
  SDL_Renderer *ren;
  SDL_Window *win;
  char *title;
  int x, y, w, h;

  ___slogan_obj_to_charstring(args[0], &title);
  ___slogan_obj_to_int(args[1], &x);
  ___slogan_obj_to_int(args[2], &y);
  ___slogan_obj_to_int(args[3], &w);
  ___slogan_obj_to_int(args[4], &h);

  win = SDL_CreateWindow(title, x, y, w, h, SDL_WINDOW_SHOWN);
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
 
  media_w = (window *) malloc(sizeof(window));
  if (media_w == NULL) {
    printf("Failed to allocate window.\n");
    return NULL;
  }
  media_w->win = win;
  media_w->ren = ren;
  return media_w;
}

void media_render(window *w)
{
  SDL_Renderer *ren = w->ren;
  SDL_RenderClear(ren);
  SDL_RenderPresent(ren);
}

___slogan_obj media_event(___slogan_obj *args)
{
  int error = 0;
  ___slogan_obj result;
  ___slogan_obj f = args[0];
  SDL_Event event;
  int etype = 0;

  if (SDL_PollEvent(&event)) {
    switch (event.type) {
    case SDL_QUIT:
      etype = 0;
      break;
    default:
      etype = event.type;
    }
    ___ON_THROW(result = ___call_fn(f, ___pair(___fix(etype), ___NUL)), error = 1);
    if (error == 1)
      return ___FAL;
    return result;
  }
  return ___FAL;
}

void media_close_window(window *w)
{
  SDL_DestroyRenderer(w->ren);
  SDL_DestroyWindow(w->win);
  free(w);
}

SDL_Texture *media_open_bmp(window *w, ___slogan_obj *args)
{
  SDL_Texture *tex;
  SDL_Surface *bmp;
  SDL_Renderer *ren = w->ren;
  char *file_name;

  ___slogan_obj_to_charstring(args[0], &file_name);

  bmp = SDL_LoadBMP(file_name);
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

void media_close_bmp(SDL_Texture *tex)
{
  SDL_DestroyTexture(tex);
}
