#include <stdio.h>
#include <stdlib.h>

int add(int a, int b)
{
  return a + b;
}

static char *message = "hi from C";
          
char *say_hello(char *msg)
{
  printf("Slogan says: %s\n", msg);
  return message;
}

typedef struct point {
  int x;
  int y;
} point;

point *make_point(int x, int y)
{
  point *p = malloc(sizeof(point));
  p->x = x;
  p->y = y;
}

int point_x(point *p) { return p->x; }
int point_y(point *p) { return p->y; }

void free_point(point *p) { free(p); }

point copy_point(int x, int y)
{
  point p = {x, y};
  return p;
}

void print_point(point p)
{
  printf("%d:%d\n", p.x, p.y);
}
