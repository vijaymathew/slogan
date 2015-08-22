/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#include <stdlib.h>
#include "huge.h"
#include "ecc.h"

void add_points( point *p1, point *p2, huge *p )
{
  point p3;
  huge denominator;
  huge numerator;
  huge invdenom;
  huge lambda;

  set_huge( &denominator, 0 ); 
  copy_huge( &denominator, &p2->x );    // denominator = x2
  subtract( &denominator, &p1->x );     // denominator = x2 - x1
  set_huge( &numerator, 0 );
  copy_huge( &numerator, &p2->y );      // numerator = y2
  subtract( &numerator, &p1->y );       // numerator = y2 - y1
  set_huge( &invdenom, 0 );
  copy_huge( &invdenom, &denominator );
  inv( &invdenom, p );
  set_huge( &lambda, 0 );
  copy_huge( &lambda, &numerator );
  multiply( &lambda, &invdenom );       // lambda = numerator / denominator
  set_huge( &p3.x, 0 );
  copy_huge( &p3.x, &lambda );    // x3 = lambda
  multiply( &p3.x, &lambda );     // x3 = lambda * lambda
  subtract( &p3.x, &p1->x );      // x3 = ( lambda * lambda ) - x1
  subtract( &p3.x, &p2->x );      // x3 = ( lambda * lambda ) - x1 - x2

  divide( &p3.x, p, NULL );       // x3 = ( ( lamdba * lambda ) - x1 - x2 ) % p

  // positive remainder always
  if ( p3.x.sign ) 
  {
    p3.x.sign = 0;
    subtract( &p3.x, p );
    p3.x.sign = 0;
  }

  set_huge( &p3.y, 0 );
  copy_huge( &p3.y, &p1->x );    // y3 = x1
  subtract( &p3.y, &p3.x );      // y3 = x1 - x3
  multiply( &p3.y, &lambda );    // y3 = ( x1 - x3 ) * lambda
  subtract( &p3.y, &p1->y );     // y3 = ( ( x1 - x3 ) * lambda ) - y

  divide( &p3.y, p, NULL );
  // positive remainder always
  if ( p3.y.sign )
  {
    p3.y.sign = 0;
    subtract( &p3.y, p );
    p3.y.sign = 0;
  }

  // p1->x = p3.x
  // p1->y = p3.y
  copy_huge( &p1->x, &p3.x );
  copy_huge( &p1->y, &p3.y );

  free_huge( &p3.x );
  free_huge( &p3.y );
  free_huge( &denominator );
  free_huge( &numerator );
  free_huge( &invdenom );
  free_huge( &lambda );
}

static void double_point( point *p1, huge *a, huge *p )
{
  huge lambda;
  huge l1;
  huge x1;
  huge y1;

  set_huge( &lambda, 0 );
  set_huge( &x1, 0 );
  set_huge( &y1, 0 );
  set_huge( &lambda, 2 );     // lambda = 2;
  multiply( &lambda, &p1->y );  // lambda = 2 * y1
  inv( &lambda, p );       // lambda = ( 2 * y1 ) ^ -1 (% p)

  set_huge( &l1, 3 );       // l1 = 3
  multiply( &l1, &p1->x );    // l1 = 3 * x
  multiply( &l1, &p1->x );    // l1 = 3 * x ^ 2
  add( &l1, a );         // l1 = ( 3 * x ^ 2 ) + a
  multiply( &lambda, &l1 );    // lambda = [ ( 3 * x ^ 2 ) + a ] / [ 2 * y1 ] ) % p
  copy_huge( &y1, &p1->y );
  // Note - make two copies of x2; this one is for y1 below
  copy_huge( &p1->y, &p1->x );
  set_huge( &x1, 2 );
  multiply( &x1, &p1->x );    // x1 = 2 * x1

  copy_huge( &p1->x, &lambda );  // x1 = lambda
  multiply( &p1->x, &lambda );  // x1 = ( lambda ^ 2 );
  subtract( &p1->x, &x1 );    // x1 = ( lambda ^ 2 ) - ( 2 * x1 )
  divide( &p1->x, p, NULL );   // [ x1 = ( lambda ^ 2 ) - ( 2 * x1 ) ] % p
  
  if ( p1->x.sign )
  {
    subtract( &p1->x, p );
    p1->x.sign = 0;
    subtract( &p1->x, p );
  }
  subtract( &p1->y, &p1->x );  // y3 = x3 – x1
  multiply( &p1->y, &lambda ); // y3 = lambda * ( x3 - x1 );
  subtract( &p1->y, &y1 );   // y3 = ( lambda * ( x3 - x1 ) ) - y1
  divide( &p1->y, p, NULL );  // y3 = [ ( lambda * ( x3 - x1 ) ) - y1 ] % p
  if ( p1->y.sign )
  {
    p1->y.sign = 0;
    subtract( &p1->y, p );
    p1->y.sign = 0;
  }

  free_huge( &lambda );
  free_huge( &x1 );
  free_huge( &y1 );
  free_huge( &l1 );
}

void multiply_point( point *p1, huge *k, huge *a, huge *p )
{
  int i;
  unsigned char mask;
  point dp;
  int paf = 1;

  set_huge( &dp.x, 0 );
  set_huge( &dp.y, 0 );
  copy_huge( &dp.x, &p1->x );
  copy_huge( &dp.y, &p1->y );
  for ( i = k->size; i; i-- )
  {
    for ( mask = 0x01; mask; mask <<= 1 )
    {
      if ( k->rep[ i - 1 ] & mask )
      {
       if ( paf )
       {
         paf = 0;
         copy_huge( &p1->x, &dp.x );
         copy_huge( &p1->y, &dp.y );
       }
       else
       {
         add_points( p1, &dp, p );
       }
     }
     // double dp
     double_point( &dp, a, p );
    }
  } 

  free_huge( &dp.x );
  free_huge( &dp.y );
}
