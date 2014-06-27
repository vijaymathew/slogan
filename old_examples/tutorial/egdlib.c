// gcc -c -Wall -Werror -I${SLOGAN_ROOT}/platform/gsc/include -I${SLOGAN_ROOT}/src/include -fpic egdlib.c 
// gcc -shared -o libegdlib.so egdlib.o

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "slogan.h"
 
void c_hello (void)
{
  printf ("hello from C!\n");
}

void c_add_ten (___slogan_obj i)
{
  printf ("%d\n", ___int (i) + 10);
}

int c_add_two_ints (___slogan_obj i, ___slogan_obj j)
{
  return ___int (i) + ___int (j);
}

float c_add_two_floats (___slogan_obj i, ___slogan_obj j)
{
  float a, b;
  ___slogan_obj_to_float (i, &a);
  ___slogan_obj_to_float (j, &b);
  return a + b;
}

void c_print_msg (___slogan_obj msg)
{
  char *str;
  ___slogan_obj_to_charstring (msg, &str);
  printf ("%s\n", str);
}

char *c_get_greeting (___slogan_obj name)
{
  static char buf[100];
  char *str;
  strcpy (buf, "hello ");
  ___slogan_obj_to_charstring (name, &str);
  strcat (buf, str);
  return buf;
}

___slogan_obj c_get_greeting_as_string (___slogan_obj name)
{
  char *greetings = c_get_greeting (name);
  ___slogan_obj str;
  ___charstring_to_slogan_obj (greetings, &str);
  ___release_slogan_obj (str);
  return str;
}

void c_print_list_of_strings (___slogan_obj lst)
{
  while (lst != ___NUL)
    {
      char *s;
      ___slogan_obj_to_charstring (___head (lst), &s);
      printf ("%s\n", s);
      lst = ___tail (lst);
    }
}

void c_print_array_of_strings (___slogan_obj array, ___slogan_obj len)
{
  ___slogan_obj *elements = ___body (array);
  int i;
  for (i = 0; i < ___int (len); ++i)
    {
      char *s;
      ___slogan_obj_to_charstring (elements[i], &s);
      printf ("%s\n", s);
    }
}

___slogan_obj c_make_int_list (___slogan_obj a, ___slogan_obj b, ___slogan_obj c)
{
  ___slogan_obj result = ___pair (a, ___pair (b, ___pair (c, ___NUL)));
  ___release_slogan_obj (result);
  return result;
}

void c_fill_int_array (___slogan_obj array, ___slogan_obj a, ___slogan_obj b, ___slogan_obj c)
{
  ___slogan_obj *objs = malloc (sizeof (___slogan_obj) * 3);
  objs[0] = a;
  objs[1] = b;
  objs[2] = c;
  memcpy (___body (array), objs, sizeof (___slogan_obj) * 3);
  free (objs);
}

struct c_point { int x; int y; };

void *c_make_point (___slogan_obj x, ___slogan_obj y)
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

