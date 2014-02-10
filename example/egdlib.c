// gcc -c -Wall -Werror -I../platform/gsc/include -I../src/include -fpic egdlib.c 
// gcc -shared -o libegdlib.so egdlib.o

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "slogan.h"
 
void c_hello (void)
{
  printf ("hello from C!\n");
}

void c_add_ten (___SLOGAN_OBJ i)
{
  printf ("%d\n", ___INT (i) + 10);
}

int c_add_two_ints (___SLOGAN_OBJ i, ___SLOGAN_OBJ j)
{
  return ___INT (i) + ___INT (j);
}

float c_add_two_floats (___SLOGAN_OBJ i, ___SLOGAN_OBJ j)
{
  float a, b;
  ___SLOGAN_OBJ_to_FLOAT (i, &a, 0);
  ___SLOGAN_OBJ_to_FLOAT (j, &b, 0);
  return a + b;
}

void c_print_msg (___SLOGAN_OBJ msg)
{
  char *str;
  ___SLOGAN_OBJ_to_CHARSTRING (msg, &str, 0);
  printf ("%s\n", str);
}

char *c_get_greeting (___SLOGAN_OBJ name)
{
  static char buf[100];
  char *str;
  strcpy (buf, "hello ");
  ___SLOGAN_OBJ_to_CHARSTRING (name, &str, 0);
  strcat (buf, str);
  return buf;
}

___SLOGAN_OBJ c_get_greeting_as_string (___SLOGAN_OBJ name)
{
  char *greetings = c_get_greeting (name);
  ___SLOGAN_OBJ str;
  ___CHARSTRING_to_SLOGAN_OBJ (___PSTATE, greetings, &str, 0);
  ___release_slogan_obj (str);
  return str;
}

void c_print_list_of_strings (___SLOGAN_OBJ lst)
{
  while (lst != ___NUL)
    {
      char *s;
      ___SLOGAN_OBJ_to_CHARSTRING (___HEAD (lst), &s, 0);
      printf ("%s\n", s);
      lst = ___TAIL (lst);
    }
}

void c_print_array_of_strings (___SLOGAN_OBJ array, ___SLOGAN_OBJ len)
{
  ___SLOGAN_OBJ *elements = ___BODY (array);
  int i;
  for (i = 0; i < ___INT (len); ++i)
    {
      char *s;
      ___SLOGAN_OBJ_to_CHARSTRING (elements[i], &s, 0);
      printf ("%s\n", s);
    }
}

___SLOGAN_OBJ c_make_int_list (___SLOGAN_OBJ a, ___SLOGAN_OBJ b, ___SLOGAN_OBJ c)
{
  ___SLOGAN_OBJ result = ___PAIR (___PSTATE, a, 
                                  ___PAIR (___PSTATE, b, 
                                           ___PAIR (___PSTATE, c, ___NUL)));
  ___release_slogan_obj (result);
  return result;
}

void c_fill_int_array (___SLOGAN_OBJ array, ___SLOGAN_OBJ a, ___SLOGAN_OBJ b, ___SLOGAN_OBJ c)
{
  ___SLOGAN_OBJ *objs = malloc (sizeof (___SLOGAN_OBJ) * 3);
  objs[0] = a;
  objs[1] = b;
  objs[2] = c;
  memcpy (___BODY (array), objs, sizeof (___SLOGAN_OBJ) * 3);
  free (objs);
}

struct c_point { int x; int y; };

void *c_make_point (___SLOGAN_OBJ x, ___SLOGAN_OBJ y)
{
  struct c_point *p = malloc (sizeof (struct c_point));
  p->x = ___INT (x);
  p->y = ___INT (y);
  return p;
}

void c_print_point (void *ptr)
{
  struct c_point *p = (struct c_point *) ptr;
  printf ("%d:%d\n", p->x, p->y);
}

void c_destroy_point (void *ptr)
{
  struct c_point *p = (struct c_point *) ptr;
  free (p);
}

