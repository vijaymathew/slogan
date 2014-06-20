/* 
   SDL wrapper for Slogan.

   Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved. 
 */

#include <stdio.h>
#include <SDL2/SDL.h>
#include "slogan.h"

int airglow_init () 
{
  return SDL_Init (SDL_INIT_EVERYTHING);
}

void airglow_destroy ()
{
  SDL_Quit ();
}

int AIRGLOW_WINDOW_SHOWN () { return SDL_WINDOW_SHOWN; }
int AIRGLOW_WINDOW_FULLSCREEN () { return SDL_WINDOW_FULLSCREEN; }
int AIRGLOW_WINDOW_FULLSCREEN_DESKTOP () { return SDL_WINDOW_FULLSCREEN_DESKTOP; }
int AIRGLOW_WINDOW_OPENGL () { return SDL_WINDOW_OPENGL; }
int AIRGLOW_WINDOW_HIDDEN () { return SDL_WINDOW_HIDDEN; }
int AIRGLOW_WINDOW_BORDERLESS () { return SDL_WINDOW_BORDERLESS; }
int AIRGLOW_WINDOW_RESIZABLE () { return SDL_WINDOW_RESIZABLE; }
int AIRGLOW_WINDOW_MINIMIZED () { return SDL_WINDOW_MINIMIZED; }
int AIRGLOW_WINDOW_MAXIMIZED () { return SDL_WINDOW_MAXIMIZED; }
int AIRGLOW_WINDOW_INPUT_GRABBED () { return SDL_WINDOW_INPUT_GRABBED; }
int AIRGLOW_WINDOW_ALLOW_HIGHDPI () { return SDL_WINDOW_ALLOW_HIGHDPI; }

void *airglow_create_window (___slogan_obj title, ___slogan_obj width, 
                             ___slogan_obj height, ___slogan_obj flags)
{
  char *str;
  ___slogan_obj_to_charstring (title, &str);
  return SDL_CreateWindow(str, SDL_WINDOWPOS_CENTERED, SDL_WINDOWPOS_CENTERED,
                          ___int (width), ___int (height), 
                          (Uint32) ___int (flags));
}

void *airglow_create_renderer (void *window, ___slogan_obj index, ___slogan_obj flags)
{
  return SDL_CreateRenderer (window, ___int (index), (Uint32) ___int (flags));
}

int airglow_set_render_draw_color (void *renderer, 
                                   ___slogan_obj r, ___slogan_obj g,
                                   ___slogan_obj b, ___slogan_obj a)
{
  return SDL_SetRenderDrawColor (renderer, ___int (r), ___int (g), 
                                 ___int (b), ___int (a));
}

int airglow_render_clear (void *renderer)
{
  return SDL_RenderClear (renderer);
}

void airglow_render_present (void *renderer)
{
  SDL_RenderPresent (renderer);
}

void airglow_delay (___slogan_obj ms)
{
  SDL_Delay ((Uint32) ___int (ms));
}

