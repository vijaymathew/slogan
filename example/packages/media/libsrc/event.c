#include "event.h"

___slogan_obj mouse_motion_event(SDL_Event *event)
{
  ___slogan_obj pos = ___pair(___fix(event->motion.x), ___fix(event->motion.y));
  ___slogan_obj rel_pos = ___pair(___fix(event->motion.xrel), ___fix(event->motion.yrel));
  return ___pair(pos, ___pair(rel_pos, ___NUL));
}

___slogan_obj mouse_button_event(SDL_Event *event)
{
  int button = 0, state = 0;
  
  switch (event->button.button) {
  case SDL_BUTTON_LEFT: button = 0; break;
  case SDL_BUTTON_MIDDLE: button = 1; break;
  case SDL_BUTTON_RIGHT: button = 2; break;
  case SDL_BUTTON_X1: button = 3; break;    
  case SDL_BUTTON_X2: button = 4; break;
  }
  switch (event->button.state) {
  case SDL_PRESSED: state = 0; break;
  case SDL_RELEASED: state = 1; break;
  }
  return ___pair(___fix(event->button.x),
                 ___pair(___fix(event->button.y),
                         ___pair(___fix(button),
                                 ___pair(___fix(state), ___NUL))));
}
