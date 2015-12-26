#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int add_i(int a, int b)
{
  return a + b;
}

unsigned int add_ui(unsigned int a, unsigned int b)
{
  return a + b;
}

float add_f(float a, float b)
{
  return a + b;
}

long add_l(long a, long b)
{
  return a + b;
}

unsigned long add_ul(unsigned long a, unsigned long b)
{
  return a + b;
}

long long add_ll(long long a, long long b)
{
  return a + b;
}

double add_d(double a, double b)
{
  return a + b;
}

long double add_ld(long double a, long double b)
{
  return a + b;
}

char add_c(char a, char b)
{
  return a + b;
}

static char *message = "hi from C";
          
char *say_hello(char *msg)
{
  printf("Slogan says: %s\n", msg);
  return message;
}

typedef struct person {
  int age;
  char *name;  
} person;

person *alloc_person(char *name, int age)
{
  person *p = malloc(sizeof(person));
  p->age = age;
  p->name = (char *)malloc(strlen(name) + 1);
  strcpy(p->name, name);
  return p;
}

int person_age(person *p) { return p->age; }
char *person_name(person *p) { return p->name; }

void free_person(person *p)
{
  free(p->name);
  free(p);
}

typedef struct point {
  int x;
  int y;
} point;

point make_point(int x, int y)
{
  point p;
  p.x = x; p.y = y;
  return p;
}

void print_point(point p)
{
  printf("~~~~%p\n", &p);
  printf("%d:%d", p.x, p.y);
  fflush(stdout);
}

typedef struct ball {
  point position;
  float velocity;
} ball;

ball make_ball(point p, float v)
{
  ball vec;
  vec.position = p; vec.velocity = v;
  return vec;
}

void print_ball(ball v)
{
  print_point(v.position);
  printf(",%f", v.velocity);
  fflush(stdout);
}
