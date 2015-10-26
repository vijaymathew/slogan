/* Code derived from pygame. The original copyright notice is reproduced below. */

/*
  pygame - Python Game Library
  Copyright (C) 2000-2001  Pete Shinners

  This library is free software; you can redistribute it and/or
  modify it under the terms of the GNU Library General Public
  License as published by the Free Software Foundation; either
  version 2 of the License, or (at your option) any later version.

  This library is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Library General Public License for more details.

  You should have received a copy of the GNU Library General Public
  License along with this library; if not, write to the Free
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

  Pete Shinners
  pete@shinners.org
*/

#include <SDL.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include "slogan.h"

/* Many C libraries seem to lack the trunc call (added in C99) */
#define trunc(d)   (((d) >= 0.0) ? (floor(d)) : (ceil(d)))
#define FRAC(z)    ((z) - trunc(z))
#define INVFRAC(z) (1 - FRAC(z))

#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define MIN(x, y) (((x) < (y)) ? (x) : (y))

static int clip_and_draw_line(SDL_Surface* surf, SDL_Rect* rect, Uint32 color, int* pts);
static int clip_and_draw_aaline(SDL_Surface* surf, SDL_Rect* rect, Uint32 color, float* pts, int blend);
static int clip_and_draw_line_width(SDL_Surface* surf, SDL_Rect* rect, Uint32 color, int width, int* pts);
static int clipline(int* pts, int left, int top, int right, int bottom);
static int clipaaline(float* pts, int left, int top, int right, int bottom);
static void drawline(SDL_Surface* surf, Uint32 color, int startx, int starty, int endx, int endy);
static void drawaaline(SDL_Surface* surf, Uint32 color, float startx, float starty, float endx, float endy,
                       int blend);
static void drawhorzline(SDL_Surface* surf, Uint32 color, int startx, int starty, int endx);
static void drawvertline(SDL_Surface* surf, Uint32 color, int x1, int y1, int y2);
static void draw_arc(SDL_Surface *dst, int x, int y, int radius1, int radius2, double angle_start, double angle_stop, Uint32 color);
static void draw_ellipse(SDL_Surface *dst, int x, int y, int rx, int ry, Uint32 color);
static void draw_fillellipse(SDL_Surface *dst, int x, int y, int rx, int ry, Uint32 color);
static void draw_fillpoly(SDL_Surface *dst, int *vx, int *vy, int n, Uint32 color);

static void RGBAFromColorObj(___slogan_obj colorobj, int rgba[4])
{
  ___slogan_obj_to_int(___CAR(colorobj), &rgba[0]);
  colorobj = ___CDR(colorobj);
  ___slogan_obj_to_int(___CAR(colorobj), &rgba[1]);
  colorobj = ___CDR(colorobj);
  ___slogan_obj_to_int(___CAR(colorobj), &rgba[2]);
  colorobj = ___CDR(colorobj);
  ___slogan_obj_to_int(___CAR(colorobj), &rgba[3]);
}

static ___slogan_obj ilist4(int a, int b, int c, int d)
{
  return ___pair(___FIX(a),
                 ___pair(___FIX(b),
                         ___pair(___FIX(c),
                                 ___pair(___FIX(d), ___NUL))));
}

___slogan_obj _media_draw_aaline(void *surfobj,
                                 ___slogan_obj so_rect,
                                 ___slogan_obj so_bounds,
                                 ___slogan_obj so_color)
{
  SDL_Surface* surf = (SDL_Surface *)surfobj;
  float startx, starty, endx, endy;
  int top, left, bottom, right;
  int blend = 1;
  float pts[4];
  int rgba[4];
  int color;
  int anydraw;
  
  if (surf->format->BytesPerPixel != 3 && surf->format->BytesPerPixel != 4)
    return ___FIX(-1);

  RGBAFromColorObj(so_color, rgba);
  color = SDL_MapRGBA(surf->format, rgba[0], rgba[1], rgba[2], rgba[3]);

  if (SDL_LockSurface(surf) != 0) return ___FIX(-2);
  
  ___slogan_obj_to_float(___CAR(so_rect), &startx);
  so_rect = ___CDR(so_rect);
  ___slogan_obj_to_float(___CAR(so_rect), &starty);
  so_rect = ___CDR(so_rect);
  ___slogan_obj_to_float(___CAR(so_rect), &endx);
  so_rect = ___CDR(so_rect);
  ___slogan_obj_to_float(___CAR(so_rect), &endy);

  pts[0] = startx; pts[1] = starty;
  pts[2] = endx; pts[3] = endy;
  anydraw = clip_and_draw_aaline(surf, &surf->clip_rect, color, pts, blend);

  SDL_UnlockSurface(surf);
  
  /*compute return rect*/
  if (!anydraw)
    return ilist4((int)startx, (int)starty, 0, 0);
  else
    {
      if(pts[0] < pts[2])
        {
          left = (int)(pts[0]);
          right = (int)(pts[2]);
        }
      else
        {
          left = (int)(pts[2]);
          right = (int)(pts[0]);
        }
      if(pts[1] < pts[3])
        {
          top = (int)(pts[1]);
          bottom = (int)(pts[3]);
        }
      else
        {
          top = (int)(pts[3]);
          bottom = (int)(pts[1]);
        }
    }
  return ilist4(left, top, right-left+2, bottom-top+2);
}

___slogan_obj _media_draw_line(void *surfobj,
                               ___slogan_obj so_color,
                               ___slogan_obj so_start,
                               ___slogan_obj so_end,
                               ___slogan_obj so_width)
{
  SDL_Surface* surf = (SDL_Surface *)surfobj;
  int startx, starty, endx, endy;
  int top, left, bottom, right;
  int width = 1;
  int pts[4];
  int rgba[4];
  int color;
  int anydraw;

  if (surf->format->BytesPerPixel <= 0 || surf->format->BytesPerPixel > 4)
    return ___FIX(-1);

  RGBAFromColorObj(so_color, rgba);
  color = SDL_MapRGBA(surf->format, rgba[0], rgba[1], rgba[2], rgba[3]);

  ___slogan_obj_to_int(___CAR(so_start), &startx);
  ___slogan_obj_to_int(___CDR(so_start), &starty);
  ___slogan_obj_to_int(___CAR(so_end), &endx);
  ___slogan_obj_to_int(___CDR(so_end), &endy);

  ___slogan_obj_to_int(so_width, &width);

  if (width < 1)
    return ilist4(startx, starty, 0, 0);
  
  if (SDL_LockSurface(surf) != 0) return ___FIX(-2);

  pts[0] = startx; pts[1] = starty;
  pts[2] = endx; pts[3] = endy;
  anydraw = clip_and_draw_line_width(surf, &surf->clip_rect, color, width, pts);

  SDL_UnlockSurface(surf);

  /*compute return rect*/
  if(!anydraw)
    return ilist4(startx, starty, 0, 0);
  if(pts[0] < pts[2])
    {
      left = pts[0];
      right = pts[2];
    }
  else
    {
      left = pts[2];
      right = pts[0];
    }
  if(pts[1] < pts[3])
    {
      top = pts[1];
      bottom = pts[3];
    }
  else
    {
      top = pts[3];
      bottom = pts[1];
    }
  return ilist4(left, top, right-left+1, bottom-top+1);
}


___slogan_obj _media_draw_aalines(void *surfobj,
                                  ___slogan_obj so_color,
                                  ___slogan_obj so_closed,
                                  ___slogan_obj so_points,
                                  ___slogan_obj so_blend)
{
  SDL_Surface* surf = (SDL_Surface *)surfobj;
  float x, y;
  int top, left, bottom, right;
  float pts[4];
  int rgba[4];
  int color;
  int closed, blend;
  int result, loop, length, drawn;
  float startx, starty;
  ___slogan_obj item;
  float oldx, oldy;
  
  if (surf->format->BytesPerPixel !=3 && surf->format->BytesPerPixel != 4)
    return ___FIX(-1);

  RGBAFromColorObj(so_color, rgba);
  color = SDL_MapRGBA(surf->format, rgba[0], rgba[1], rgba[2], rgba[3]);
  closed = so_closed == ___TRU;

  item = ___CAR(so_points);
  ___slogan_obj_to_float(___CAR(item), &x);
  ___slogan_obj_to_float(___CDR(item), &y);
  oldx = x; oldy = y;
  
  startx = pts[0] = x;
  starty = pts[1] = y;
  left = right = (int)x;
  top = bottom = (int)y;

  if (SDL_LockSurface(surf) != 0) return ___FIX(-2);

  drawn = 1;
  so_points = ___CDR(so_points);
  while (so_points != ___NUL)
    {
      item = ___CAR(so_points);
      ___slogan_obj_to_float(___CAR(item), &x);
      ___slogan_obj_to_float(___CDR(item), &y);
      ++drawn;
      pts[0] = startx;
      pts[1] = starty;
      startx = pts[2] = x;
      starty = pts[3] = y;
      if (clip_and_draw_aaline(surf, &surf->clip_rect, color, pts, blend))
        {
          left = MIN((int)MIN(pts[0], pts[2]), left);
          top = MIN((int)MIN(pts[1], pts[3]), top);
          right = MAX((int)MAX(pts[0], pts[2]), right);
          bottom = MAX((int)MAX(pts[1], pts[3]), bottom);
        }
      so_points = ___CDR(so_points);
    }
  if (closed && drawn > 2)
    {
      x = oldx; y = oldy;
      pts[0] = startx;
      pts[1] = starty;
      pts[2] = x;
      pts[3] = y;
      clip_and_draw_aaline(surf, &surf->clip_rect, color, pts, blend);
    }

  SDL_UnlockSurface(surf);
  return ilist4(left, top, right-left+2, bottom-top+2);
}

___slogan_obj _media_draw_lines(void *surfobj,
                                ___slogan_obj colorobj,
                                ___slogan_obj closedobj,
                                ___slogan_obj points,
                                ___slogan_obj widthobj)
{
  SDL_Surface* surf = (SDL_Surface *)surfobj;
  ___slogan_obj item;
  int x, y, oldx, oldy;
  int top, left, bottom, right;
  int pts[4], width=1;
  int rgba[4];
  int color;
  int closed;
  int result, loop, length, drawn;
  int startx, starty;
  
  if (surf->format->BytesPerPixel <= 0 || surf->format->BytesPerPixel > 4)
    return ___FIX(-1);
  
  RGBAFromColorObj(colorobj, rgba);
  color = SDL_MapRGBA(surf->format, rgba[0], rgba[1], rgba[2], rgba[3]);
  closed = closedobj == ___TRU ? 1 : 0;
  
  item = ___CAR(points);
  ___slogan_obj_to_int(___CAR(item), &x);
  ___slogan_obj_to_int(___CDR(item), &y);
  oldx = x; oldy = y;
  points = ___CDR(points);
  
  startx = pts[0] = left = right = x;
  starty = pts[1] = top = bottom = y;

  ___slogan_obj_to_int(widthobj, &width);
  if (width < 1)
    return ilist4(left, top, 0, 0);
  
  if (SDL_LockSurface(surf) != 0) return ___FIX(-2);
  
  drawn = 1;
  while (points != ___NUL)
    {
      item = ___CAR(points);
      ___slogan_obj_to_int(___CAR(item), &x);
      ___slogan_obj_to_int(___CDR(item), &y);
      ++drawn;
      pts[0] = startx;
      pts[1] = starty;
      startx = pts[2] = x;
      starty = pts[3] = y;
      if (clip_and_draw_line_width(surf, &surf->clip_rect, color, width, pts))
        {
          left = MIN(MIN(pts[0], pts[2]), left);
          top = MIN(MIN(pts[1], pts[3]), top);
          right = MAX(MAX(pts[0], pts[2]), right);
          bottom = MAX(MAX(pts[1], pts[3]), bottom);
        }
      points = ___CDR(points);
    }
  
  if (closed && drawn > 2)
    {
      x = oldx; y = oldy;
      pts[0] = startx;
      pts[1] = starty;
      pts[2] = x;
      pts[3] = y;
      clip_and_draw_line_width(surf, &surf->clip_rect, color, width, pts);
    }
  
  
  SDL_UnlockSurface(surf);
  
  return ilist4(left, top, right-left+1, bottom-top+1);
}


___slogan_obj _media_draw_arc(void *surfobj,
                              ___slogan_obj colorobj,
                              ___slogan_obj rectobj,
                              ___slogan_obj angle_startobj,
                              ___slogan_obj angle_stopobj,
                              ___slogan_obj widthobj)
{
  SDL_Surface *surf = (SDL_Surface *)surfobj;
  int rgba[4];
  int tx, ty, tw, th;
  int color;
  int width=1, loop, t, l, b, r;
  double angle_start, angle_stop;

  ___slogan_obj_to_double(angle_stopobj, &angle_start);
  ___slogan_obj_to_double(angle_startobj, &angle_start);
  ___slogan_obj_to_int(widthobj, &width);

  ___slogan_obj_to_int(___CAR(rectobj), &tx);
  rectobj = ___CDR(rectobj);
  ___slogan_obj_to_int(___CAR(rectobj), &ty);
  rectobj = ___CDR(rectobj);
  ___slogan_obj_to_int(___CAR(rectobj), &tw);
  rectobj = ___CDR(rectobj);
  ___slogan_obj_to_int(___CAR(rectobj), &th);

  if (surf->format->BytesPerPixel <= 0 || surf->format->BytesPerPixel > 4)
    return ___FIX(-1);

  RGBAFromColorObj(colorobj, rgba);
  color = SDL_MapRGBA(surf->format, rgba[0], rgba[1], rgba[2], rgba[3]);
  
  if (width < 0)
    return ___FIX(-2);
  if (width > tw / 2 || width > th / 2)
    return ___FIX(-3);
  if (angle_stop < angle_start)
    angle_stop += 360;

  if (SDL_LockSurface(surf) != 0) return ___FIX(-4);

  width = MIN(width, MIN(tw, th) / 2);
  for(loop=0; loop<width; ++loop)
    {
      draw_arc(surf, tx+tw/2, ty+th/2,
               tw/2-loop, th/2-loop,
               angle_start, angle_stop, color);
    }

  SDL_UnlockSurface(surf);

  l = MAX(tx, surf->clip_rect.x);
  t = MAX(ty, surf->clip_rect.y);
  r = MIN(tx + tw, surf->clip_rect.x + surf->clip_rect.w);
  b = MIN(ty + th, surf->clip_rect.y + surf->clip_rect.h);
  return ilist4(l, t, MAX(r-l, 0), MAX(b-t, 0));
}

___slogan_obj _media_draw_ellipse(void *surfobj,
                                  ___slogan_obj colorobj,
                                  ___slogan_obj rectobj,
                                  ___slogan_obj widthobj)
{
  SDL_Surface *surf = (SDL_Surface *)surfobj;
  int rgba[4];
  int color;
  int width=0, loop, t, l, b, r;
  int rx, ry, rh, rw;

  ___slogan_obj_to_int(___CAR(rectobj), &rx);
  rectobj = ___CDR(rectobj);
  ___slogan_obj_to_int(___CAR(rectobj), &ry);
  rectobj = ___CDR(rectobj);
  ___slogan_obj_to_int(___CAR(rectobj), &rh);
  rectobj = ___CDR(rectobj);
  ___slogan_obj_to_int(___CAR(rectobj), &rw);

  if (surf->format->BytesPerPixel <= 0 || surf->format->BytesPerPixel > 4)
    return ___FIX(-1);

  RGBAFromColorObj(colorobj, rgba);
  color = SDL_MapRGBA(surf->format, rgba[0], rgba[1], rgba[2], rgba[3]);

  ___slogan_obj_to_int(widthobj, &width);
  if (width < 0)
    return ___FIX(-2);
  if (width > rw / 2 || width > rh / 2)
    return ___FIX(-3);
  
  if (SDL_LockSurface(surf) != 0) return ___FIX(-4);

  if (!width)
    draw_fillellipse(surf, rx+rw/2, ry+rh/2, rw/2, rh/2, color);
  else
    {
      width = MIN(width, MIN(rw, rh) / 2);
      for (loop=0; loop<width; ++loop)
        {
          draw_ellipse(surf, rx+rw/2, ry+rh/2, rw/2-loop, rh/2-loop, color);
        }
    }

  SDL_UnlockSurface(surf);

  l = MAX(rx, surf->clip_rect.x);
  t = MAX(ry, surf->clip_rect.y);
  r = MIN(rx + rw, surf->clip_rect.x + surf->clip_rect.w);
  b = MIN(ry + rh, surf->clip_rect.y + surf->clip_rect.h);
  return ilist4(l, t, MAX(r-l, 0), MAX(b-t, 0));
}


___slogan_obj _media_draw_circle(void *surfobj,
                                 ___slogan_obj colorobj,
                                 ___slogan_obj posxobj,
                                 ___slogan_obj posyobj,
                                 ___slogan_obj radiusobj,
                                 ___slogan_obj widthobj)
{
  SDL_Surface *surf = (SDL_Surface *)surfobj;
  int rgba[4];
  int color;
  int posx, posy, radius, t, l, b, r;
  int width=0, loop;

  if (surf->format->BytesPerPixel <= 0 || surf->format->BytesPerPixel > 4)
    return ___FIX(-1);

  RGBAFromColorObj(colorobj, rgba);
  color = SDL_MapRGBA(surf->format, rgba[0], rgba[1], rgba[2], rgba[3]);

  ___slogan_obj_to_int(radiusobj, &radius);
  ___slogan_obj_to_int(posxobj, &posx);
  ___slogan_obj_to_int(posyobj, &posy);
  ___slogan_obj_to_int(widthobj, &width);
  
  if (radius < 0)
    return ___FIX(-2);
  if (width < 0)
    return ___FIX(-3);
  if (width > radius)
    return ___FIX(-4);

  if (SDL_LockSurface(surf) != 0) return ___FIX(-5);

  if (!width)
    draw_fillellipse(surf, posx, posy, radius, radius, color);
  else
    for (loop=0; loop<width; ++loop)
      draw_ellipse(surf, posx, posy, radius-loop, radius-loop, color);
  
  SDL_UnlockSurface(surf);

  l = MAX(posx - radius, surf->clip_rect.x);
  t = MAX(posy - radius, surf->clip_rect.y);
  r = MIN(posx + radius, surf->clip_rect.x + surf->clip_rect.w);
  b = MIN(posy + radius, surf->clip_rect.y + surf->clip_rect.h);
  return ilist4(l, t, MAX(r-l, 0), MAX(b-t, 0));
}

___slogan_obj _media_draw_polygon(void *surfobj,
                                  ___slogan_obj colorobj,
                                  ___slogan_obj points,
                                  ___slogan_obj widthobj)
{
  SDL_Surface *surf = (SDL_Surface *)surfobj;
  int rgba[4];
  int color;
  int width=0, length, loop, numpoints;
  int *xlist, *ylist;
  int x, y, top, left, bottom, right, result;
  ___slogan_obj item;

  ___slogan_obj_to_int(widthobj, &width);

  if (width)
    return _media_draw_lines(surfobj, colorobj, ___TRU, points, widthobj);

  if (surf->format->BytesPerPixel <= 0 || surf->format->BytesPerPixel > 4)
    return ___FIX(-1);

  RGBAFromColorObj(colorobj, rgba);
  color = SDL_MapRGBA(surf->format, rgba[0], rgba[1], rgba[2], rgba[3]);

  length = 0;
  while (points != ___NUL)
    {
      ++length;
      points = ___CDR(points);
    }
  
  item = ___CAR(points);
  ___slogan_obj_to_int(___CAR(item), &x);
  ___slogan_obj_to_int(___CDR(item), &y);

  left = right = x;
  top = bottom = y;
  
  xlist = (int *)malloc(sizeof(int) * length);
  if (xlist == NULL) return ___FIX(-2);
  ylist = (int *)malloc(sizeof(int) * length);
  if (ylist == NULL) return ___FIX(-3);
  numpoints = 0;
  for(loop = 0; loop < length; ++loop)
    {
      item = ___CAR(points);
      ___slogan_obj_to_int(___CAR(item), &x);
      ___slogan_obj_to_int(___CDR(item), &y);
      xlist[numpoints] = x;
      ylist[numpoints] = y;
      ++numpoints;
      left = MIN(x, left);
      top = MIN(y, top);
      right = MAX(x, right);
      bottom = MAX(y, bottom);
    }

  if (SDL_LockSurface(surf) != 0)
    {
      free(xlist); free(ylist);
      return ___FIX(-4);
    }
  
  draw_fillpoly(surf, xlist, ylist, numpoints, color);

  free(xlist); free(ylist);
  SDL_UnlockSurface(surf);
  
  left = MAX(left, surf->clip_rect.x);
  top = MAX(top, surf->clip_rect.y);
  right = MIN(right, surf->clip_rect.x + surf->clip_rect.w);
  bottom = MIN(bottom, surf->clip_rect.y + surf->clip_rect.h);
  return ilist4(left, top, right-left+1, bottom-top+1);
}

/*internal drawing tools*/

static int clip_and_draw_aaline(SDL_Surface* surf, SDL_Rect* rect, Uint32 color, float* pts, int blend)
{
  if (!clipaaline(pts, rect->x+1, rect->y+1, rect->x+rect->w-2, rect->y+rect->h-2))
    return 0;
  drawaaline(surf, color, pts[0], pts[1], pts[2], pts[3], blend);
  return 1;
}

static int clip_and_draw_line(SDL_Surface* surf, SDL_Rect* rect, Uint32 color, int* pts)
{
  if (!clipline(pts, rect->x, rect->y, rect->x+rect->w-1, rect->y+rect->h-1))
    return 0;
  if (pts[1] == pts[3])
    drawhorzline(surf, color, pts[0], pts[1], pts[2]);
  else if (pts[0] == pts[2])
    drawvertline(surf, color, pts[0], pts[1], pts[3]);
  else
    drawline(surf, color, pts[0], pts[1], pts[2], pts[3]);
  return 1;
}

static int clip_and_draw_line_width(SDL_Surface* surf, SDL_Rect* rect, Uint32 color, int width, int* pts)
{
  int loop;
  int xinc=0, yinc=0;
  int newpts[4];
  int range[4];
  int anydrawn = 0;

  if (abs(pts[0]-pts[2]) > abs(pts[1]-pts[3]))
    yinc = 1;
  else
    xinc = 1;

  memcpy(newpts, pts, sizeof(int)*4);
  if (clip_and_draw_line(surf, rect, color, newpts))
    {
      anydrawn = 1;
      memcpy(range, newpts, sizeof(int)*4);
    }
  else
    {
      range[0] = range[1] = 10000;
      range[2] = range[3] = -10000;
    }

  for (loop = 1; loop < width; loop += 2)
    {
      newpts[0] = pts[0] + xinc*(loop/2+1);
      newpts[1] = pts[1] + yinc*(loop/2+1);
      newpts[2] = pts[2] + xinc*(loop/2+1);
      newpts[3] = pts[3] + yinc*(loop/2+1);
      if (clip_and_draw_line(surf, rect, color, newpts))
        {
          anydrawn = 1;
          range[0] = MIN(newpts[0], range[0]);
          range[1] = MIN(newpts[1], range[1]);
          range[2] = MAX(newpts[2], range[2]);
          range[3] = MAX(newpts[3], range[3]);
        }
      if (loop+1<width)
        {
          newpts[0] = pts[0] - xinc*(loop/2+1);
          newpts[1] = pts[1] - yinc*(loop/2+1);
          newpts[2] = pts[2] - xinc*(loop/2+1);
          newpts[3] = pts[3] - yinc*(loop/2+1);
          if (clip_and_draw_line(surf, rect, color, newpts))
            {
              anydrawn = 1;
              range[0] = MIN(newpts[0], range[0]);
              range[1] = MIN(newpts[1], range[1]);
              range[2] = MAX(newpts[2], range[2]);
              range[3] = MAX(newpts[3], range[3]);
            }
        }
    }
  if (anydrawn)
    memcpy(pts, range, sizeof(int)*4);
  return anydrawn;
}


/*this line clipping based heavily off of code from
  http://www.ncsa.uiuc.edu/Vis/Graphics/src/clipCohSuth.c */
#define LEFT_EDGE   0x1
#define RIGHT_EDGE  0x2
#define BOTTOM_EDGE 0x4
#define TOP_EDGE    0x8
#define INSIDE(a)   (!a)
#define REJECT(a,b) (a&b)
#define ACCEPT(a,b) (!(a|b))

static int encode(int x, int y, int left, int top, int right, int bottom)
{
  int code = 0;
  if (x < left)  code |= LEFT_EDGE;
  if (x > right) code |= RIGHT_EDGE;
  if (y < top)   code |= TOP_EDGE;
  if (y > bottom)code |= BOTTOM_EDGE;
  return code;
}

static int encodeFloat(float x, float y, int left, int top, int right, int bottom)
{
  int code = 0;
  if (x < left)  code |= LEFT_EDGE;
  if (x > right) code |= RIGHT_EDGE;
  if (y < top)   code |= TOP_EDGE;
  if (y > bottom)code |= BOTTOM_EDGE;
  return code;
}

static int clipaaline(float* pts, int left, int top, int right, int bottom)
{
  float x1 = pts[0];
  float y1 = pts[1];
  float x2 = pts[2];
  float y2 = pts[3];
  int code1, code2;
  int draw = 0;
  float swaptmp;
  int intswaptmp;
  float m; /*slope*/

  while (1)
    {
      code1 = encodeFloat(x1, y1, left, top, right, bottom);
      code2 = encodeFloat(x2, y2, left, top, right, bottom);
      if (ACCEPT(code1, code2))
        {
          draw = 1;
          break;
        }
      else if (REJECT(code1, code2))
        {
          break;
        }
      else
        {
          if (INSIDE(code1))
            {
              swaptmp = x2; x2 = x1; x1 = swaptmp;
              swaptmp = y2; y2 = y1; y1 = swaptmp;
              intswaptmp = code2; code2 = code1; code1 = intswaptmp;
            }
          if (x2 != x1)
            m = (y2 - y1) / (x2 - x1);
          else
            m = 1.0f;
          if (code1 & LEFT_EDGE)
            {
              y1 += ((float)left - x1) * m;
              x1 = (float)left;
            }
          else if (code1 & RIGHT_EDGE)
            {
              y1 += ((float)right - x1) * m;
              x1 = (float)right;
            }
          else if (code1 & BOTTOM_EDGE)
            {
              if(x2 != x1)
                x1 += ((float)bottom - y1) / m;
              y1 = (float)bottom;
            }
          else if (code1 & TOP_EDGE)
            {
              if (x2 != x1)
                x1 += ((float)top - y1) / m;
              y1 = (float)top;
            }
        }
    }
  if (draw)
    {
      pts[0] = x1; pts[1] = y1;
      pts[2] = x2; pts[3] = y2;
    }
  return draw;
}

static int clipline(int* pts, int left, int top, int right, int bottom)
{
  int x1 = pts[0];
  int y1 = pts[1];
  int x2 = pts[2];
  int y2 = pts[3];
  int code1, code2;
  int draw = 0;
  int swaptmp;
  float m; /*slope*/

  while(1)
    {
      code1 = encode(x1, y1, left, top, right, bottom);
      code2 = encode(x2, y2, left, top, right, bottom);
      if (ACCEPT(code1, code2))
        {
          draw = 1;
          break;
        }
      else if (REJECT(code1, code2))
        break;
      else
        {
          if (INSIDE(code1))
            {
              swaptmp = x2; x2 = x1; x1 = swaptmp;
              swaptmp = y2; y2 = y1; y1 = swaptmp;
              swaptmp = code2; code2 = code1; code1 = swaptmp;
            }
          if (x2 != x1)
            m = (y2 - y1) / (float)(x2 - x1);
          else
            m = 1.0f;
          if (code1 & LEFT_EDGE)
            {
              y1 += (int)((left - x1) * m);
              x1 = left;
            }
          else if (code1 & RIGHT_EDGE)
            {
              y1 += (int)((right - x1) * m);
              x1 = right;
            }
          else if (code1 & BOTTOM_EDGE)
            {
              if(x2 != x1)
                x1 += (int)((bottom - y1) / m);
              y1 = bottom;
            }
          else if (code1 & TOP_EDGE)
            {
              if (x2 != x1)
                x1 += (int)((top - y1) / m);
              y1 = top;
            }
        }
    }
  if (draw)
    {
      pts[0] = x1; pts[1] = y1;
      pts[2] = x2; pts[3] = y2;
    }
  return draw;
}



static int set_at(SDL_Surface* surf, int x, int y, Uint32 color)
{
  SDL_PixelFormat* format = surf->format;
  Uint8* pixels = (Uint8*)surf->pixels;
  Uint8* byte_buf, rgb[4];

  if (x < surf->clip_rect.x || x >= surf->clip_rect.x + surf->clip_rect.w ||
      y < surf->clip_rect.y || y >= surf->clip_rect.y + surf->clip_rect.h)
    return 0;

  switch (format->BytesPerPixel)
    {
    case 1:
      *((Uint8*)pixels + y * surf->pitch + x) = (Uint8)color;
      break;
    case 2:
      *((Uint16*)(pixels + y * surf->pitch) + x) = (Uint16)color;
      break;
    case 4:
      *((Uint32*)(pixels + y * surf->pitch) + x) = color;
      /*			  *((Uint32*)(pixels + y * surf->pitch) + x) =
                                  ~(*((Uint32*)(pixels + y * surf->pitch) + x)) * 31;
      */			  break;
    default:/*case 3:*/
      SDL_GetRGB(color, format, rgb, rgb+1, rgb+2);
      byte_buf = (Uint8*)(pixels + y * surf->pitch) + x * 3;
#if (SDL_BYTEORDER == SDL_LIL_ENDIAN)
      *(byte_buf + (format->Rshift >> 3)) = rgb[0];
      *(byte_buf + (format->Gshift >> 3)) = rgb[1];
      *(byte_buf + (format->Bshift >> 3)) = rgb[2];
#else
      *(byte_buf + 2 - (format->Rshift >> 3)) = rgb[0];
      *(byte_buf + 2 - (format->Gshift >> 3)) = rgb[1];
      *(byte_buf + 2 - (format->Bshift >> 3)) = rgb[2];
#endif
      break;
    }
  return 1;
}


#define DRAWPIX32(pixel,colorptr,br,blend)                      \
  if (SDL_BYTEORDER == SDL_BIG_ENDIAN) color <<= 8;             \
  if (blend) {                                                  \
    int x;                                                      \
    float nbr = 1.0-br;                                         \
    x = colorptr[0]*br+pixel[0]*nbr;                            \
    pixel[0]= (x>254) ? 255: x;                                 \
    x = colorptr[1]*br+pixel[1]*nbr;                            \
    pixel[1]= (x>254) ? 255: x;                                 \
    x = colorptr[2]*br+pixel[2]*nbr;                            \
    pixel[2]= (x>254) ? 255: x;                                 \
    if(hasalpha) pixel[3] = pixel[0]+(br*255) - (pixel[3]*br);  \
  } else {                                                      \
    pixel[0]=(Uint8)(colorptr[0]*br);                           \
    pixel[1]=(Uint8)(colorptr[1]*br);                           \
    pixel[2]=(Uint8)(colorptr[2]*br);                           \
    if(hasalpha) pixel[3] = br*255;                             \
  } 

/* Adapted from http://freespace.virgin.net/hugo.elias/graphics/x_wuline.htm */
static void drawaaline(SDL_Surface* surf, Uint32 color, float x1, float y1, float x2, float y2, int blend) {
  float grad, xd, yd;
  float xgap, ygap, xend, yend, xf, yf;
  float brightness1, brightness2;
  float swaptmp;
  int x, y, ix1, ix2, iy1, iy2;
  int pixx, pixy;
  Uint8* pixel;
  Uint8* pm = (Uint8*)surf->pixels;
  Uint8* colorptr = (Uint8*)&color;
  const int hasalpha = surf->format->Amask;

  pixx = surf->format->BytesPerPixel;
  pixy = surf->pitch;

  xd = x2-x1;
  yd = y2-y1;

  if (xd == 0 && yd == 0)
    {
      /* Single point. Due to the nature of the aaline clipping, this
       * is less exact than the normal line. */
      set_at (surf, x1, y1, color);
      return;
    }

  if (fabs(xd)>fabs(yd))
    {
      if (x1>x2)
        {
          swaptmp=x1; x1=x2; x2=swaptmp;
          swaptmp=y1; y1=y2; y2=swaptmp;
          xd = (x2-x1);
          yd = (y2-y1);
        }
      grad = yd/xd;
      xend = trunc(x1)+0.5; /* This makes more sense than trunc(x1+0.5) */
      yend = y1+grad*(xend-x1);
      xgap = INVFRAC(x1);
      ix1 = (int)xend;
      iy1 = (int)yend;
      yf = yend+grad;
      brightness1 = INVFRAC(yend) * xgap;
      brightness2 =    FRAC(yend) * xgap;
      pixel = pm + pixx * ix1 + pixy * iy1;
      DRAWPIX32(pixel, colorptr, brightness1, blend)
        pixel += pixy;
      DRAWPIX32(pixel, colorptr, brightness2, blend)
        xend = trunc(x2)+0.5;
      yend = y2+grad*(xend-x2);
      xgap =    FRAC(x2); /* this also differs from Hugo's description. */
      ix2 = (int)xend;
      iy2 = (int)yend;
      brightness1 = INVFRAC(yend) * xgap;
      brightness2 =    FRAC(yend) * xgap;
      pixel = pm + pixx * ix2 + pixy * iy2;
      DRAWPIX32(pixel, colorptr, brightness1, blend)
        pixel += pixy;
      DRAWPIX32(pixel, colorptr, brightness2, blend)
        for (x=ix1+1; x<ix2; ++x)
          {
            brightness1=INVFRAC(yf);
            brightness2=   FRAC(yf);
            pixel = pm + pixx * x + pixy * (int)yf;
            DRAWPIX32(pixel, colorptr, brightness1, blend)
              pixel += pixy;
            DRAWPIX32(pixel, colorptr, brightness2, blend)
              yf += grad;
          }
    }
  else
    {
      if (y1>y2)
        {
          swaptmp=y1; y1=y2; y2=swaptmp;
          swaptmp=x1; x1=x2; x2=swaptmp;
          yd = (y2-y1);
          xd = (x2-x1);
        }
      grad = xd/yd;
      yend = trunc(y1)+0.5;  /* This makes more sense than trunc(x1+0.5) */
      xend = x1+grad*(yend-y1);
      ygap = INVFRAC(y1);
      iy1 = (int)yend;
      ix1 = (int)xend;
      xf = xend+grad;
      brightness1 = INVFRAC(xend) * ygap;
      brightness2 =    FRAC(xend) * ygap;
      pixel = pm + pixx * ix1 + pixy * iy1;
      DRAWPIX32(pixel, colorptr, brightness1, blend)
        pixel += pixx;
      DRAWPIX32(pixel, colorptr, brightness2, blend)
        yend = trunc(y2)+0.5;
      xend = x2+grad*(yend-y2);
      ygap = FRAC(y2);
      iy2 = (int)yend;
      ix2 = (int)xend;
      brightness1 = INVFRAC(xend) * ygap;
      brightness2 =    FRAC(xend) * ygap;
      pixel = pm + pixx * ix2 + pixy * iy2;
      DRAWPIX32(pixel, colorptr, brightness1, blend)
        pixel += pixx;
      DRAWPIX32(pixel, colorptr, brightness2, blend)
        for (y=iy1+1; y<iy2; ++y)
          {
            brightness1=INVFRAC(xf);
            brightness2=   FRAC(xf);
            pixel = pm + pixx * (int)xf + pixy * y;
            DRAWPIX32(pixel, colorptr, brightness1, blend)
              pixel += pixx;
            DRAWPIX32(pixel, colorptr, brightness2, blend)
              xf += grad;
          }
    }
}

/*here's my sdl'ized version of bresenham*/
static void drawline(SDL_Surface* surf, Uint32 color, int x1, int y1, int x2, int y2)
{
  int deltax, deltay, signx, signy;
  int pixx, pixy;
  int x = 0, y = 0;
  int swaptmp;
  Uint8 *pixel;
  Uint8 *colorptr;

  deltax = x2 - x1;
  deltay = y2 - y1;
  signx = (deltax < 0) ? -1 : 1;
  signy = (deltay < 0) ? -1 : 1;
  deltax = signx * deltax + 1;
  deltay = signy * deltay + 1;

  pixx = surf->format->BytesPerPixel;
  pixy = surf->pitch;
  pixel = ((Uint8*)surf->pixels) + pixx * x1 + pixy * y1;

  pixx *= signx;
  pixy *= signy;
  if(deltax < deltay) /*swap axis if rise > run*/
    {
      swaptmp = deltax; deltax = deltay; deltay = swaptmp;
      swaptmp = pixx; pixx = pixy; pixy = swaptmp;
    }

  switch (surf->format->BytesPerPixel)
    {
    case 1:
      for (; x < deltax; x++, pixel += pixx)
        {
          *pixel = (Uint8)color;
          y += deltay; if(y >= deltax) {y -= deltax; pixel += pixy;}
        }break;
    case 2:
      for (; x < deltax; x++, pixel += pixx)
        {
          *(Uint16*)pixel = (Uint16)color;
          y += deltay; if(y >= deltax) {y -= deltax; pixel += pixy;}
        }break;
    case 3:
      if(SDL_BYTEORDER == SDL_BIG_ENDIAN) color <<= 8;
      colorptr = (Uint8*)&color;
      for (; x < deltax; x++, pixel += pixx)
        {
          pixel[0] = colorptr[0];
          pixel[1] = colorptr[1];
          pixel[2] = colorptr[2];
          y += deltay; if(y >= deltax) {y -= deltax; pixel += pixy;}
        }break;
    default: /*case 4*/
      for (; x < deltax; x++, pixel += pixx)
        {
          *(Uint32*)pixel = (Uint32)color;
          y += deltay; if(y >= deltax) {y -= deltax; pixel += pixy;}
        }break;
    }
}

static void drawhorzline(SDL_Surface* surf, Uint32 color, int x1, int y1, int x2)
{
  Uint8 *pixel, *end;
  Uint8 *colorptr;

  if (x1 == x2)
    {
      set_at(surf, x1, y1, color);
      return;
    }

  pixel = ((Uint8*)surf->pixels) + surf->pitch * y1;
  if (x1 < x2)
    {
      end = pixel + x2 * surf->format->BytesPerPixel;
      pixel += x1 * surf->format->BytesPerPixel;
    }
  else
    {
      end = pixel + x1 * surf->format->BytesPerPixel;
      pixel += x2 * surf->format->BytesPerPixel;
    }
  switch(surf->format->BytesPerPixel)
    {
    case 1:
      for(; pixel <= end; ++pixel)
        {
          *pixel = (Uint8)color;
        }break;
    case 2:
      for (; pixel <= end; pixel+=2)
        {
          *(Uint16*)pixel = (Uint16)color;
        }break;
    case 3:
      if (SDL_BYTEORDER == SDL_BIG_ENDIAN) color <<= 8;
      colorptr = (Uint8*)&color;
      for (; pixel <= end; pixel+=3)
        {
          pixel[0] = colorptr[0];
          pixel[1] = colorptr[1];
          pixel[2] = colorptr[2];
        }break;
    default: /*case 4*/
      for (; pixel <= end; pixel+=4)
        {
          *(Uint32*)pixel = color;
        }break;
    }
}

static void drawhorzlineclip(SDL_Surface* surf, Uint32 color, int x1, int y1, int x2)
{
  if (y1 < surf->clip_rect.y || y1 >= surf->clip_rect.y + surf->clip_rect.h)
    return;

  if (x2 < x1)
    {
      int temp = x1;
      x1 = x2; x2 = temp;
    }

  x1 = MAX(x1, surf->clip_rect.x);
  x2 = MIN(x2, surf->clip_rect.x + surf->clip_rect.w-1);

  if (x2 < surf->clip_rect.x || x1 >= surf->clip_rect.x + surf->clip_rect.w)
    return;

  if (x1 == x2)
    set_at(surf, x1, y1, color);
  else
    drawhorzline(surf, color, x1, y1, x2);
}

static void drawvertline(SDL_Surface* surf, Uint32 color, int x1, int y1, int y2)
{
  Uint8   *pixel, *end;
  Uint8   *colorptr;
  Uint32  pitch = surf->pitch;

  if (y1 == y2)
    {
      set_at(surf, x1, y1, color);
      return;
    }

  pixel = ((Uint8*)surf->pixels) + x1 * surf->format->BytesPerPixel;
  if (y1 < y2)
    {
      end	  = pixel + surf->pitch * y2;
      pixel += surf->pitch * y1;
    }
  else
    {
      end	  = pixel + surf->pitch * y1;
      pixel += surf->pitch * y2;
    }

  switch (surf->format->BytesPerPixel)
    {
    case 1:
      for (; pixel <= end; pixel+=pitch)
        {
          *pixel = (Uint8)color;
        }break;
    case 2:
      for (; pixel <= end; pixel+=pitch)
        {
          *(Uint16*)pixel = (Uint16)color;
        }break;
    case 3:
      if (SDL_BYTEORDER == SDL_BIG_ENDIAN) color <<= 8;
      colorptr = (Uint8*)&color;
      for (; pixel <= end; pixel+=pitch)
        {
          pixel[0] = colorptr[0];
          pixel[1] = colorptr[1];
          pixel[2] = colorptr[2];
        }break;
    default: /*case 4*/
      for (; pixel <= end; pixel+=pitch)
        {
          *(Uint32*)pixel = color;
        }break;
    }
}

static void drawvertlineclip(SDL_Surface* surf, Uint32 color, int x1, int y1, int y2)
{
  if (x1 < surf->clip_rect.x || x1 >= surf->clip_rect.x + surf->clip_rect.w)
    return;
  if (y2 < y1)
    {
      int temp = y1;
      y1 = y2; y2 = temp;
    }
  y1 = MAX(y1, surf->clip_rect.y);
  y2 = MIN(y2, surf->clip_rect.y + surf->clip_rect.h-1);
  if (y2 - y1 < 1)
    set_at(surf, x1, y1, color);
  else
    drawvertline(surf, color, x1, y1, y2);
}

static void draw_arc(SDL_Surface *dst, int x, int y, int radius1, int radius2,
                     double angle_start, double angle_stop, Uint32 color)
{
  double aStep;            // Angle Step (rad)
  double a;                // Current Angle (rad)
  int x_last, x_next, y_last, y_next;

  // Angle step in rad
  if (radius1<radius2)
    {
      if (radius1<1.0e-4)
        {
          aStep=1.0;
        }
      else
        {
          aStep=asin(2.0/radius1);
        }
    }
  else
    {
      if (radius2<1.0e-4)
        {
          aStep=1.0;
        }
      else
        {
          aStep=asin(2.0/radius2);
        }
    }

  if (aStep<0.05)
    {
      aStep = 0.05;
    }

  x_last = x+cos(angle_start)*radius1;
  y_last = y-sin(angle_start)*radius2;
  for (a=angle_start+aStep; a<=angle_stop; a+=aStep)
    {
      int points[4];
      x_next = x+cos(a)*radius1;
      y_next = y-sin(a)*radius2;
      points[0] = x_last; points[1] = y_last;
      points[2] = x_next; points[3] = y_next;
      clip_and_draw_line(dst, &dst->clip_rect, color, points);
      x_last = x_next;
      y_last = y_next;
    }
}

static void draw_ellipse(SDL_Surface *dst, int x, int y, int rx, int ry, Uint32 color)
{
  int ix, iy;
  int h, i, j, k;
  int oh, oi, oj, ok;
  int xmh, xph, ypk, ymk;
  int xmi, xpi, ymj, ypj;
  int xmj, xpj, ymi, ypi;
  int xmk, xpk, ymh, yph;

  if (rx==0 && ry==0)
    {  /* Special case - draw a single pixel */
      set_at( dst, x, y, color);
      return;
    }
  if (rx==0)
    { /* Special case for rx=0 - draw a vline */
      drawvertlineclip( dst, color, x, (Sint16)(y-ry), (Sint16)(y+ry) );
      return;
    }
  if (ry==0)
    { /* Special case for ry=0 - draw a hline */
      drawhorzlineclip( dst, color, (Sint16)(x-rx), y, (Sint16)(x+rx) );
      return;
    }


  /* Init vars */
  oh = oi = oj = ok = 0xFFFF;
  if (rx > ry)
    {
      ix = 0;
      iy = rx * 64;
      do {
        h = (ix + 16) >> 6;
        i = (iy + 16) >> 6;
        j = (h * ry) / rx;
        k = (i * ry) / rx;

        if (((ok!=k) && (oj!=k)) || ((oj!=j) && (ok!=j)) || (k!=j))
          {
            xph=x+h-1;
            xmh=x-h;
            if (k>0)
              {
                ypk=y+k-1;
                ymk=y-k;
                if (h > 0)
                  {
                    set_at(dst, xmh, ypk, color);
                    set_at(dst, xmh, ymk, color);
                  }
                set_at(dst, xph, ypk, color);
                set_at(dst, xph, ymk, color);
              }
            ok=k;
            xpi=x+i-1;
            xmi=x-i;
            if (j>0)
              {
                ypj=y+j-1;
                ymj=y-j;
                set_at(dst, xmi, ypj, color);
                set_at(dst, xpi, ypj, color);
                set_at(dst, xmi, ymj, color);
                set_at(dst, xpi, ymj, color);
              }
            oj=j;
          }
        ix = ix + iy / rx;
        iy = iy - ix / rx;

      } while (i > h);
    }
  else
    {
      ix = 0;
      iy = ry * 64;
      do {
        h = (ix + 32) >> 6;
        i = (iy + 32) >> 6;
        j = (h * rx) / ry;
        k = (i * rx) / ry;

        if (((oi!=i) && (oh!=i)) || ((oh!=h) && (oi!=h) && (i!=h)))
          {
            xmj=x-j;
            xpj=x+j-1;
            if (i>0)
              {
                ypi=y+i-1;
                ymi=y-i;
                if (j > 0)
                  {
                    set_at(dst, xmj, ypi, color);
                    set_at(dst, xmj, ymi, color);
                  }
                set_at(dst, xpj, ypi, color);
                set_at(dst, xpj, ymi, color);
              }
            oi=i;
            xmk=x-k;
            xpk=x+k-1;
            if (h>0)
              {
                yph=y+h-1;
                ymh=y-h;
                set_at(dst, xmk, yph, color);
                set_at(dst, xpk, yph, color);
                set_at(dst, xmk, ymh, color);
                set_at(dst, xpk, ymh, color);
              }
            oh=h;
          }
        ix = ix + iy / ry;
        iy = iy - ix / ry;
      } while(i > h);
    }
}

static void draw_fillellipse(SDL_Surface *dst, int x, int y, int rx, int ry, Uint32 color)
{
  int ix, iy;
  int h, i, j, k;
  int oh, oi, oj, ok;

  if (rx==0 && ry==0)
    {  /* Special case - draw a single pixel */
      set_at( dst, x, y, color);
      return;
    }
  if (rx==0)
    { /* Special case for rx=0 - draw a vline */
      drawvertlineclip( dst, color, x, (Sint16)(y-ry), (Sint16)(y+ry) );
      return;
    }
  if (ry==0)
    { /* Special case for ry=0 - draw a hline */
      drawhorzlineclip( dst, color, (Sint16)(x-rx), y, (Sint16)(x+rx) );
      return;
    }

  /* Init vars */
  oh = oi = oj = ok = 0xFFFF;

  /* Draw */
  if (rx >= ry)
    {
      ix = 0;
      iy = rx * 64;

      do {
        h = (ix + 8) >> 6;
        i = (iy + 8) >> 6;
        j = (h * ry) / rx;
        k = (i * ry) / rx;
        if ((ok!=k) && (oj!=k) && (k<ry))
          {
            drawhorzlineclip(dst, color, x-h, y-k-1, x+h-1);
            drawhorzlineclip(dst, color, x-h, y+k, x+h-1);
            ok=k;
          }
        if ((oj!=j) && (ok!=j) && (k!=j))
          {
            drawhorzlineclip(dst, color, x-i, y+j, x+i-1);
            drawhorzlineclip(dst, color, x-i, y-j-1, x+i-1);
            oj=j;
          }
        ix = ix + iy / rx;
        iy = iy - ix / rx;

      } while (i > h);
    }
  else
    {
      ix = 0;
      iy = ry * 64;

      do {
        h = (ix + 8) >> 6;
        i = (iy + 8) >> 6;
        j = (h * rx) / ry;
        k = (i * rx) / ry;

        if ((oi!=i) && (oh!=i) && (i<ry))
          {
            drawhorzlineclip(dst, color, x-j, y+i, x+j-1);
            drawhorzlineclip(dst, color, x-j, y-i-1, x+j-1);
            oi=i;
          }
        if ((oh!=h) && (oi!=h) && (i!=h))
          {
            drawhorzlineclip(dst, color, x-k, y+h, x+k-1);
            drawhorzlineclip(dst, color, x-k, y-h-1, x+k-1);
            oh=h;
          }

        ix = ix + iy / ry;
        iy = iy - ix / ry;

      } while(i > h);
    }
}


static int compare_int(const void *a, const void *b)
{
  return (*(const int *)a) - (*(const int *)b);
}

static void draw_fillpoly(SDL_Surface *dst, int *vx, int *vy, int n, Uint32 color)
{
  int i;
  int y;
  int miny, maxy;
  int x1, y1;
  int x2, y2;
  int ind1, ind2;
  int ints;
  int *polyints = (int *)malloc(sizeof(int) * n);
  if (polyints == NULL)
    {
      fprintf(stderr, "draw_fillpoly - failed to allocate memory for polyints");
      return;
    }

  /* Determine Y maxima */
  miny = vy[0];
  maxy = vy[0];
  for (i=1; (i < n); i++)
    {
      miny = MIN(miny, vy[i]);
      maxy = MAX(maxy, vy[i]);
    }

  /* Draw, scanning y */
  for (y=miny; (y <= maxy); y++)
    {
      ints = 0;
      for (i=0; (i < n); i++)
        {
          if (!i)
            {
              ind1 = n-1;
              ind2 = 0;
            }
          else
            {
              ind1 = i-1;
              ind2 = i;
            }
          y1 = vy[ind1];
          y2 = vy[ind2];
          if (y1 < y2)
            {
              x1 = vx[ind1];
              x2 = vx[ind2];
            }
          else if (y1 > y2)
            {
              y2 = vy[ind1];
              y1 = vy[ind2];
              x2 = vx[ind1];
              x1 = vx[ind2];
            }
          else
            {
              continue;
            }
          if ((y >= y1) && (y < y2))
            {
              polyints[ints++] = (y-y1) * (x2-x1) / (y2-y1) + x1;
            }
          else if ((y == maxy) && (y > y1) && (y <= y2))
            {
              polyints[ints++] = (y-y1) * (x2-x1) / (y2-y1) + x1;
            }
        }
      qsort(polyints, ints, sizeof(int), compare_int);

      for (i=0; (i<ints); i+=2)
        {
          drawhorzlineclip(dst, color, polyints[i], y, polyints[i+1]);
        }
    }
  free(polyints);
}

