#include <SDL.h>
#include <SDL_image.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include "slogan.h"

static int MAX_INIT_FLAGS = 7;
static int INIT_FLAGS[] = { SDL_INIT_HAPTIC,
                            SDL_INIT_AUDIO,
                            SDL_INIT_VIDEO,
                            SDL_INIT_TIMER,
                            SDL_INIT_JOYSTICK,
                            SDL_INIT_EVERYTHING,
                            SDL_INIT_NOPARACHUTE };

___slogan_obj _media_init(___slogan_obj so_flags)
{
  int flags = 0;

  while (so_flags != ___NUL)
    {
      ___slogan_obj obj = ___CAR(so_flags);
      int f;
      ___slogan_obj_to_int(obj, &f);
      assert(f >= 0 && f < MAX_INIT_FLAGS);
      flags |= INIT_FLAGS[f];
      so_flags = ___CDR(so_flags);
    }
  if (SDL_Init(flags) >= 0)
    return ___TRU;
  else
    return ___FAL;
}

void _media_release()
{
  SDL_Quit();
}

___slogan_obj _media_get_error()
{
  char *s = (char *)SDL_GetError();
  ___slogan_obj ret;
  if (s != NULL)
    {
      ___charstring_to_slogan_obj(s, &ret);
      ___release_slogan_obj(ret);
    }
  else
    ret = ___FAL;
  return ret;
}

static int MAX_CREATE_WINDOW_FLAGS = 12;
static int CREATE_WINDOW_FLAGS[] = { SDL_WINDOW_FULLSCREEN,
                                     SDL_WINDOW_OPENGL,
                                     SDL_WINDOW_SHOWN,
                                     SDL_WINDOW_HIDDEN,
                                     SDL_WINDOW_BORDERLESS,
                                     SDL_WINDOW_RESIZABLE,
                                     SDL_WINDOW_MINIMIZED,
                                     SDL_WINDOW_MAXIMIZED,
                                     SDL_WINDOW_INPUT_GRABBED,
                                     SDL_WINDOW_INPUT_FOCUS,
                                     SDL_WINDOW_MOUSE_FOCUS,
                                     SDL_WINDOW_FOREIGN };
  
void *_media_create_window(___slogan_obj so_title,
                           ___slogan_obj so_xpos,
                           ___slogan_obj so_ypos,
                           ___slogan_obj so_width,
                           ___slogan_obj so_height,
                           ___slogan_obj so_flags)
{
  char *title;
  int xpos, ypos, width, height, flags;

  flags = 0;
  ___slogan_obj_to_charstring(so_title, &title);
  ___slogan_obj_to_int(so_xpos, &xpos);
  ___slogan_obj_to_int(so_ypos, &ypos);
  ___slogan_obj_to_int(so_width, &width);
  ___slogan_obj_to_int(so_height, &height);
  while (so_flags != ___NUL)
    {
      ___slogan_obj obj = ___CAR(so_flags);
      int f;
      ___slogan_obj_to_int(obj, &f);
      assert(f >= 0 && f < MAX_CREATE_WINDOW_FLAGS);
      flags |= CREATE_WINDOW_FLAGS[f];
      so_flags = ___CDR(so_flags);
    }

  return SDL_CreateWindow(title,
                          xpos == -1 ? SDL_WINDOWPOS_CENTERED : xpos,
                          ypos == -1 ? SDL_WINDOWPOS_CENTERED : ypos,
                          width, height, flags);
}

void _media_destroy_window(void *w)
{
  SDL_DestroyWindow(w);
}

void _media_show_window(void *w)
{
  SDL_ShowWindow(w);
}

void _media_hide_window(void *w)
{
  SDL_HideWindow(w);
}

void *_media_get_window_surface(void *w)
{
  return SDL_GetWindowSurface((SDL_Window *)w);
}

___slogan_obj _media_update_window_surface(void *w)
{
  return SDL_UpdateWindowSurface(w) == 0 ? ___TRU : ___FAL;
}

static int MAX_RENDERER_FLAGS = 4;
static int RENDERER_FLAGS[] = { SDL_RENDERER_SOFTWARE,
                                SDL_RENDERER_ACCELERATED,
                                SDL_RENDERER_PRESENTVSYNC,
                                SDL_RENDERER_TARGETTEXTURE };

void *_media_create_renderer(void *window,
                             ___slogan_obj so_driver,
                             ___slogan_obj so_flags)
{
  int driver, flags;
  flags = 0;
  
  ___slogan_obj_to_int(so_driver, &driver);

  while (so_flags != ___NUL)
    {
      ___slogan_obj obj = ___CAR(so_flags);
      int f;
      ___slogan_obj_to_int(obj, &f);
      assert(f >= 0 && f < MAX_RENDERER_FLAGS);
      flags |= RENDERER_FLAGS[f];
      so_flags = ___CDR(so_flags);
    }

  return SDL_CreateRenderer(window, driver, flags);
}

void _media_destroy_renderer(void *r)
{
  SDL_DestroyRenderer(r);
}

___slogan_obj _media_set_render_color(void *renderer,
                                      ___slogan_obj so_r,
                                      ___slogan_obj so_g,
                                      ___slogan_obj so_b,
                                      ___slogan_obj so_a)
{
  int r, g, b, a;

  ___slogan_obj_to_int(so_r, &r);
  ___slogan_obj_to_int(so_g, &g);
  ___slogan_obj_to_int(so_b, &b);
  ___slogan_obj_to_int(so_a, &a);
  
  if (SDL_SetRenderDrawColor(renderer, r, g, b, a) == 0)
    return ___TRU;
  else
    return ___FAL;
}

___slogan_obj _media_render_clear(void *renderer)
{
  if (SDL_RenderClear(renderer) == 0)
    return ___TRU;
  else
    return ___FAL;
}

void _media_render_present(void *renderer)
{
  SDL_RenderPresent(renderer);
}

void _media_delay(___slogan_obj so_ms)
{
  int ms;

  ___slogan_obj_to_int(so_ms, &ms);
  SDL_Delay(ms);
}

___slogan_obj _media_handle_event()
{
  SDL_Event event;
  if (SQL_PollEvent(&event))
    return ___FIX(event.type);
  else
    return ___FAL;
}

void *_media_load_image(void *renderer,
                        ___slogan_obj so_path)
{
  char *path;
  SDL_Surface *surface;
  
  ___slogan_obj_to_charstring(so_path, &path);
  surface = IMG_Load(path);
  if (surface != NULL)
    {
      SDL_Texture *texture = SDL_CreateTextureFromSurface(renderer, surface);
      SDL_FreeSurface(surface);
      return texture;
    }
  
  return NULL;
}

void *_media_texture_from_surface(void *renderer_ptr,
                                  ___slogan_obj surface_obj)
{
  SDL_Surface *surface;

  ___slogan_obj_to_void_pointer(surface_obj, (void **)&surface);
  return SDL_CreateTextureFromSurface((SDL_Renderer *)renderer_ptr, surface);
}

void _media_free_surface(void *surface)
{
  SDL_FreeSurface(surface);
}

void _media_free_texture(void *texture)
{
  SDL_DestroyTexture(texture);
}

static int MAX_FLIP = 3;
static int FLIP[] = { SDL_FLIP_NONE,
                      SDL_FLIP_HORIZONTAL,
                      SDL_FLIP_VERTICAL };

___slogan_obj _media_render_copy(___slogan_obj so_ptrs,
                                 ___slogan_obj so_x,
                                 ___slogan_obj so_y,
                                 ___slogan_obj so_w,
                                 ___slogan_obj so_h,
                                 ___slogan_obj so_angle,
                                 ___slogan_obj so_center,
                                 ___slogan_obj so_flip)
{
  int x, y, w, h, flip;
  double angle;
  SDL_Point center;
  SDL_Renderer *renderer;
  SDL_Texture *texture;
  SDL_Rect src;
  SDL_Rect dest;

  ___slogan_obj_to_int(so_x, &x);
  ___slogan_obj_to_int(so_y, &y);
  ___slogan_obj_to_int(so_w, &w);
  ___slogan_obj_to_int(so_h, &h);
  ___slogan_obj_to_int(so_flip, &flip);
  assert(flip >= 0 && flip <= MAX_FLIP);
  flip = FLIP[flip];
  ___slogan_obj_to_void_pointer(___CAR(so_ptrs), (void **)&renderer);
  ___slogan_obj_to_void_pointer(___CAR(___CDR(so_ptrs)), (void **)&texture);
  ___slogan_obj_to_double(so_angle, &angle);
  if (so_center != ___NUL)
    {
      ___slogan_obj_to_int(___CAR(so_center), &center.x);
      ___slogan_obj_to_int(___CAR(___CDR(so_center)), &center.y);
    }

  if (w <= 0 || h <= 0)
    if (SDL_QueryTexture(texture, NULL, NULL, &w, &h) < 0)
      return ___FAL;
  
  dest.x = src.x = x;
  dest.y = src.y = y;
  dest.w = src.w = w;
  dest.h = src.h = h;
  if (SDL_RenderCopyEx(renderer, texture, &src, &dest,
                       angle, so_center == ___NUL ? NULL : &center,
                       flip) == 0)
    return ___TRU;
  else
    return ___FAL;
}

___slogan_obj _media_render_line(void *renderer,
                                 ___slogan_obj so_x1,
                                 ___slogan_obj so_y1,
                                 ___slogan_obj so_x2,
                                 ___slogan_obj so_y2)
{

  int x1, y1, x2, y2;

  ___slogan_obj_to_int(so_x1, &x1);
  ___slogan_obj_to_int(so_y1, &y1);
  ___slogan_obj_to_int(so_x2, &x2);
  ___slogan_obj_to_int(so_y2, &y2);
  
  if (SDL_RenderDrawLine(renderer, x1, y1, x2, y2) == 0)
    return ___TRU;
  else
    return ___FAL;
}

___slogan_obj _media_render_lines(void *renderer,
                                  ___slogan_obj so_points,
                                  ___slogan_obj so_count)
{
  int count;
  int i;
  SDL_Point *points;

  ___slogan_obj_to_int(so_count, &count);
  points = (SDL_Point *)malloc(sizeof(SDL_Point) * count);
  for (i = 0; i < count; ++i)
    {
      SDL_Point point;
      if (so_points == ___NUL) break;
      ___slogan_obj_to_int(___CAR(___CAR(so_points)), &point.x);
      ___slogan_obj_to_int(___CDR(___CAR(so_points)), &point.y);
      points[i] = point;
      so_points = ___CDR(so_points);
    }
  i = SDL_RenderDrawLines(renderer, points, count);
  free(points);
  return (i == 0 ? ___TRU : ___FAL);
}

___slogan_obj _media_render_point(void *renderer,
                                  ___slogan_obj so_x,
                                  ___slogan_obj so_y)
{
  int x, y;

  ___slogan_obj_to_int(so_x, &x);
  ___slogan_obj_to_int(so_y, &y);

  if (SDL_RenderDrawPoint(renderer, x, y) == 0)
    return ___TRU;
  return ___FAL;
}

___slogan_obj _media_render_rect(void *renderer,
                                 ___slogan_obj so_x,
                                 ___slogan_obj so_y,
                                 ___slogan_obj so_w,
                                 ___slogan_obj so_h,
                                 ___slogan_obj fill)
{
  SDL_Rect rect;
  int r;
  
  ___slogan_obj_to_int(so_x, &rect.x);
  ___slogan_obj_to_int(so_y, &rect.y);
  ___slogan_obj_to_int(so_w, &rect.w);
  ___slogan_obj_to_int(so_h, &rect.h);

  if (fill == ___TRU)
    r = SDL_RenderFillRect(renderer, &rect);
  else
    r = SDL_RenderDrawRect(renderer, &rect);
  return (r == 0 ? ___TRU : ___FAL);
}

