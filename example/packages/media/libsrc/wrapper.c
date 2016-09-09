#include <SDL.h>
#include <SDL2_gfxPrimitives.h>
#include <stdio.h>
#include "slogan.h"

typedef struct canvas_ {
  SDL_Renderer *ren;
  Uint32 fg_color;
} canvas;

typedef struct window_ {
  SDL_Window *win;
  canvas c;
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
  canvas c;
  SDL_Renderer *ren;
  SDL_Window *win;
  char *title;
  int x, y, w, h;

  ___slogan_obj_to_charstring(args[0], &title);
  ___slogan_obj_to_int(args[1], &x);
  ___slogan_obj_to_int(args[2], &y);
  ___slogan_obj_to_int(args[3], &w);
  ___slogan_obj_to_int(args[4], &h);

  win = SDL_CreateWindow(title,
                         x < 0 ? SDL_WINDOWPOS_CENTERED : x,
                         y < 0 ? SDL_WINDOWPOS_CENTERED : y,
                         w, h, SDL_WINDOW_SHOWN);
  if (win == NULL) {
    report_error("SDL_CreateWindow");
    SDL_Quit();
    return NULL;
  }

  ren = SDL_CreateRenderer(win, -1, SDL_RENDERER_ACCELERATED);
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
  c.ren = ren;
  c.fg_color = 0;
  media_w->win = win;
  media_w->c = c;
  return media_w;
}

int media_canvas_clear(canvas *c)
{
  int ret = SDL_RenderClear(c->ren);
  if (ret) report_error("SDL_RenderClear");
  return ret;
}

void media_canvas_render(canvas *c)
{
  SDL_RenderPresent(c->ren);
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
  SDL_DestroyRenderer(w->c.ren);
  SDL_DestroyWindow(w->win);
  free(w);
}

canvas *media_window_canvas(window *w)
{
  return &w->c;
}

int media_canvas_bg(canvas *c, ___slogan_obj *args)
{
  int r, g, b, a, ret;
  SDL_Renderer *ren;

  ___slogan_obj_to_int(args[0], &r);
  ___slogan_obj_to_int(args[1], &g);
  ___slogan_obj_to_int(args[2], &b);
  ___slogan_obj_to_int(args[3], &a);

  ren = c->ren;
  ret = SDL_SetRenderDrawColor(ren, r, g, b, a);
  if (ret) report_error("SDL_SetRenderDrawColor");
  ret = SDL_RenderClear(ren);
  if (ret) report_error("SDL_RenderClear");
  return ret;
}

void media_canvas_fg(canvas *c, ___slogan_obj *args)
{
  int ret;
  Uint8 rgba[4];
  
  ___slogan_obj_to_int(args[0], (int *)&rgba[0]);
  ___slogan_obj_to_int(args[1], (int *)&rgba[1]);
  ___slogan_obj_to_int(args[2], (int *)&rgba[2]);
  ___slogan_obj_to_int(args[3], (int *)&rgba[3]);
  c->fg_color = *((Uint32 *)rgba);
}

int media_canvas_draw_line(canvas *c, ___slogan_obj *args)
{
  int x1, y1, x2, y2, ret;

  ___slogan_obj_to_int(args[0], &x1);
  ___slogan_obj_to_int(args[1], &y1);
  ___slogan_obj_to_int(args[2], &x2);
  ___slogan_obj_to_int(args[3], &y2);
  
  ret = lineColor(c->ren, x1, y1, x2, y2, c->fg_color);
  if (ret) report_error("lineColor");
  return ret;
}

int media_canvas_draw_point(canvas *c, ___slogan_obj *args)
{
  int x, y, ret;

  ___slogan_obj_to_int(args[0], &x);
  ___slogan_obj_to_int(args[1], &y);
  
  ret = pixelColor(c->ren, x, y, c->fg_color);
  if (ret) report_error("pixelColor");
  return ret;
}

int media_canvas_draw_rect(canvas *c, ___slogan_obj *args)
{
  int x1, y1, x2, y2, ret;

  ___slogan_obj_to_int(args[0], &x1);
  ___slogan_obj_to_int(args[1], &y1);
  ___slogan_obj_to_int(args[2], &x2);
  ___slogan_obj_to_int(args[3], &y2);
  
  ret = rectangleColor(c->ren, x1, y1, x2, y2, c->fg_color);
  if (ret) report_error("rectangleColor");
  return ret;
}

int media_canvas_draw_rrect(canvas *c, ___slogan_obj *args)
{
  int x1, y1, x2, y2, rad, ret;

  ___slogan_obj_to_int(args[0], &x1);
  ___slogan_obj_to_int(args[1], &y1);
  ___slogan_obj_to_int(args[2], &x2);
  ___slogan_obj_to_int(args[3], &y2);
  ___slogan_obj_to_int(args[4], &rad);
  
  ret = roundedRectangleColor(c->ren, x1, y1, x2, y2, rad, c->fg_color);
  if (ret) report_error("roundedRectangleColor");
  return ret;
}

int media_canvas_draw_triangle(canvas *c, ___slogan_obj *args)
{
  int x1, y1, x2, y2, x3, y3, ret;

  ___slogan_obj_to_int(args[0], &x1);
  ___slogan_obj_to_int(args[1], &y1);
  ___slogan_obj_to_int(args[2], &x2);
  ___slogan_obj_to_int(args[3], &y2);
  ___slogan_obj_to_int(args[4], &x3);
  ___slogan_obj_to_int(args[5], &y3);
  
  ret = trigonColor(c->ren, x1, y1, x2, y2, x3, y3, c->fg_color);
  if (ret) report_error("trigonColor");
  return ret;
}

int media_canvas_draw_filled_triangle(canvas *c, ___slogan_obj *args)
{
  int x1, y1, x2, y2, x3, y3, r, g, b, a, ret;

  ___slogan_obj_to_int(args[0], &x1);
  ___slogan_obj_to_int(args[1], &y1);
  ___slogan_obj_to_int(args[2], &x2);
  ___slogan_obj_to_int(args[3], &y2);
  ___slogan_obj_to_int(args[4], &x3);
  ___slogan_obj_to_int(args[5], &y3);
  ___slogan_obj_to_int(args[6], &r);
  ___slogan_obj_to_int(args[7], &g);
  ___slogan_obj_to_int(args[8], &b);
  ___slogan_obj_to_int(args[9], &a);
  
  ret = filledTrigonRGBA(c->ren, x1, y1, x2, y2, x3, y3, r, g, b, a);
  if (ret) report_error("filledTrigonRGBA");
  return ret;
}

SDL_Texture *media_open_bmp(window *w, ___slogan_obj *args)
{
  SDL_Texture *tex;
  SDL_Surface *bmp;
  SDL_Renderer *ren = w->c.ren;
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
