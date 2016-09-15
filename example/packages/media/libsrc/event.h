#ifndef __MEDIA_EVENT_H_
#define __MEDIA_EVENT_H_

#include <SDL.h>
#include "slogan.h"

enum media_event {
  MEDIA_EVENT_NONE = -1,
  MEDIA_EVENT_QUIT = 0,
  MEDIA_EVENT_MOUSE_MOTION = 1,
  MEDIA_EVENT_MOUSE_BUTTON_DOWN = 2,
  MEDIA_EVENT_MOUSE_BUTTON_UP = 3
};

___slogan_obj mouse_motion_event(SDL_Event *event);
___slogan_obj mouse_button_event(SDL_Event *event);

#endif
