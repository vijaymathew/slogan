#include <SDL.h>
#include <SDL_ttf.h>
#include "slogan.h"

___slogan_obj _media_text_init()
{
  return ___FIX(TTF_Init()) == 0 ? ___TRU : ___FAL;
}

void _media_text_quit()
{
  TTF_Quit();
}

___slogan_obj _media_text_get_error()
{
  char *s = (char *)TTF_GetError();
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

void *_media_text_open_font(___slogan_obj font_name_obj,
                            ___slogan_obj ptsize_obj)
{
  char *font_name;
  int ptsize;

  ___slogan_obj_to_charstring(font_name_obj, &font_name);
  ___slogan_obj_to_int(ptsize_obj, &ptsize);

  return TTF_OpenFont(font_name, ptsize);
}

void _media_text_close_font(void *f)
{
  TTF_CloseFont((TTF_Font *)f);
}

static void sobj_to_sdl_color(___slogan_obj rgba_obj,
                              SDL_Color *rgba)
{
  ___slogan_obj_to_int(___CAR(rgba_obj), (int *)&rgba->r);
  rgba_obj = ___CDR(rgba_obj);
  ___slogan_obj_to_int(___CAR(rgba_obj), (int *)&rgba->g);
  rgba_obj = ___CDR(rgba_obj);
  ___slogan_obj_to_int(___CAR(rgba_obj), (int *)&rgba->b);
  rgba_obj = ___CDR(rgba_obj);
  ___slogan_obj_to_int(___CAR(rgba_obj), (int *)&rgba->a);
}
  
void *_media_text_render_solid(void *f,
                               ___slogan_obj text_obj,
                               ___slogan_obj rgba_obj)
{
  char *text;
  SDL_Color rgba;

  ___slogan_obj_to_charstring(text_obj, &text);
  sobj_to_sdl_color(rgba_obj, &rgba);

  return TTF_RenderText_Solid(f, text, rgba);
}
