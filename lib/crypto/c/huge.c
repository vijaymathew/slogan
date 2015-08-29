/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "slogan.h"
#include "huge.h"

/**
 * Extend the space for h by 1 char and set the LSB of that int
 * to 1.
 */
void expand( huge *h )
{
  unsigned char *tmp = h->rep;
  h->size++;
  h->rep = ( unsigned char * ) 
    calloc( h->size, sizeof( unsigned char ) );
  memcpy( h->rep + 1, tmp, 
    ( h->size - 1 ) * sizeof( unsigned char ) );
  h->rep[ 0 ] = 0x01;
  free( tmp );
}

/**
 * Given a byte array, load it into a "huge", aligning integers
 * appropriately
 */
void load_huge( huge *h, const unsigned char *bytes, int length )
{
  while ( !( *bytes ) ) 
  { 
    bytes++; 
    length--; 
  }

  h->sign = 0;
  h->size = length; 
  h->rep = ( unsigned char * ) malloc( length ); 
  memcpy( h->rep, bytes, length ); 
}

void unload_huge( const huge *h, unsigned char *bytes, int length )
{
  memcpy( bytes + ( length - h->size ), h->rep, length ); 
}

/**
 * Add two huges - overwrite h1 with the result.
 */
void add_magnitude( huge *h1, huge *h2 )
{
  unsigned int i, j; 
  unsigned int sum; 
  unsigned int carry = 0; 

  // Adding h2 to h1. If h2 is > h1 to begin with, resize h1. 
  if ( h2->size > h1->size ) 
  { 
    unsigned char *tmp = h1->rep; 
    h1->rep = ( unsigned char * ) calloc( h2->size, 
      sizeof( unsigned char ) ); 
    memcpy( h1->rep + ( h2->size - h1->size ), tmp, h1->size ); 
    h1->size = h2->size; 
    free( tmp ); 
  } 

  i = h1->size; 
  j = h2->size; 

  do 
  { 
    i--; 
    if ( j ) 
    { 
      j--; 
      sum = h1->rep[ i ] + h2->rep[ j ] + carry; 
    } 
    else 
    { 
      sum = h1->rep[ i ] + carry; 
    } 
    
    carry = sum > 0xFF; 
    h1->rep[ i ] = sum; 
  } 
  while ( i ); 

  if ( carry ) 
  { 
    // Still overflowed; allocate more space 
    expand( h1 ); 
  } 
}

/**
 * Go through h and see how many of the left-most bytes are unused.
 * Remove them and resize h appropriately.
 */
void contract( huge *h )
{
  int i = 0;

  while ( !( h->rep[ i ] ) && ( i < h->size ) ) { i++; }

  if ( i && i < h->size )
  {
    unsigned char *tmp = &h->rep[ i ];
    h->rep = ( unsigned char * ) calloc( h->size - i, 
      sizeof( unsigned char ) );
    memcpy( h->rep, tmp, h->size - i );
    h->size -= i;
  }
}

static void subtract_magnitude( huge *h1, huge *h2 ) 
{ 
  int i = h1->size; 
  int j = h2->size; 
  int difference; // signed int - important! 
  unsigned int borrow = 0; 

  do 
  { 
    i--; 

    if ( j )
    {
      j--; 
      difference = h1->rep[ i ] - h2->rep[ j ] - borrow; 
    }
    else
    {
      difference = h1->rep[ i ] - borrow;
    }
    borrow = ( difference < 0 ) ; 
    h1->rep[ i ] = difference; 
  } 
  while ( i ); 
  
  if ( borrow && i ) 
  { 
    if ( h1->rep[ i - 1 ] ) // Don't borrow i 
    {
      // negative reults are now OK 
      h1->rep[ i - 1 ]--; 
    }
  } 
  
  contract( h1 ); 
}

void add( huge *h1, huge *h2 )
{
  int result_sign;
  
  // First compute sign of result, then compute magnitude
  if ( compare( h1, h2 ) > 0 )
  {
    result_sign = h1->sign;
    
    if ( h1->sign == h2->sign )
    {
      add_magnitude( h1, h2 );
    }
    else
    {
      subtract_magnitude( h1, h2 );
    }
  }
  else
  {
    huge tmp;

    // put h1 into tmp and h2 into h1 to swap the operands    
    set_huge( &tmp, 0 ); // initialize
    copy_huge( &tmp, h1 );
    copy_huge( h1, h2 );

    if ( h1->sign == tmp.sign )
    {
      result_sign = h1->sign;
      add_magnitude( h1, &tmp );
    }
    else
    {
      result_sign = h2->sign;
      subtract_magnitude( h1, &tmp );
    }

    free_huge( &tmp );
  }

  // Use the stored sign to set the result
  h1->sign = result_sign;
}

void subtract( huge *h1, huge *h2 )
{   
  int result_sign;

  // First compute sign of result, then compute magnitude
  if ( compare( h1, h2 ) > 0 )
  {
    result_sign = h1->sign;
    
    if ( h1->sign == h2->sign )
    {
      subtract_magnitude( h1, h2 );
    }
    else
    {
      add_magnitude( h1, h2 );
    }
  }
  else
  {
    huge tmp;

    // put h1 into tmp and h2 into h1 to swap the operands
    set_huge( &tmp, 0 ); // initialize
    copy_huge( &tmp, h1 );
    copy_huge( h1, h2 );
    
    if ( h1->sign == tmp.sign )
    {
      result_sign = !( h1->sign );
      subtract_magnitude( h1, &tmp );
    }
    else
    {
      result_sign = !( h2->sign );
      add_magnitude( h1, &tmp );
    }
    
    free_huge( &tmp );
  }
  
  // Use the stored sign to set the result
  h1->sign = result_sign;
} 

void copy_huge( huge *tgt, huge *src )
{
  if ( tgt->rep )
  {
    free( tgt->rep );
  }

  tgt->sign = src->sign;
  tgt->size = src->size;
  tgt->rep = ( unsigned char * ) 
    calloc( src->size, sizeof( unsigned char ) );
  memcpy( tgt->rep, src->rep, 
     ( src->size * sizeof( unsigned char ) ) );
}

void free_huge( huge *h )
{
  if ( h->rep )
  {
    free( h->rep );
  }
}

void set_huge( huge *h, unsigned int val )
{

  unsigned int mask, i, shift;
  // Negative number support
  h->sign = 0;  // sign of 0 means positive

  h->size = 4;

  // Figure out the minimum amount of space this "val" will take
  // up in chars (leave at least one byte, though, if val is 0).
  for ( mask = 0xFF000000; mask > 0x000000FF; mask >>=8 )
  {
    if ( val & mask )
    {
      break;
    }
    h->size--;
  }

  h->rep = ( unsigned char * ) malloc( h->size );

  // Now work backwards through the int, masking off each 8-bit
  // byte (up to the first 0 byte) and copy it into the huge
  // array in big-endian format.
  mask = 0x000000FF;
  shift = 0;
  for ( i = h->size; i; i-- )
  {
    h->rep[ i - 1 ] = ( val & mask ) >> shift;
    mask <<= 8;
    shift += 8;
  }
}

void left_shift( huge *h1 )
{
  int i; 
  int old_carry, carry = 0; 

  i = h1->size; 
  do 
  { 
    i--; 
    old_carry = carry; 
    carry = ( h1->rep[ i ] & 0x80 ) == 0x80; 
    h1->rep[ i ] = ( h1->rep[ i ] << 1 ) | old_carry; 
    // Again, if C exposed the overflow bit... 
  } 
  while ( i ); 

  if ( carry ) 
  { 
    expand( h1 ); 
  } 
}


/**
 * Multiply h1 by h2, overwriting the value of h1.
 */
void multiply( huge *h1, huge *h2 )
{
  unsigned char mask; 
  unsigned int i; 
  int result_sign;
  huge temp; 

  set_huge( &temp, 0 ); 
  copy_huge( &temp, h1 ); 

  result_sign = !( h1->sign == h2->sign );

  set_huge( h1, 0 ); 

  i = h2->size; 
  do 
  { 
    i--; 
    for ( mask = 0x01; mask; mask <<= 1 ) 
    { 
      if ( mask & h2->rep[ i ] ) 
      { 
        add( h1, &temp ); 
      } 
      left_shift( &temp ); 
    } 
  } 
  while ( i ); 

  h1->sign = result_sign;
}

/**
 * Compare h1 to h2. Return:
 * 0 if h1 == h2
 * a positive number if h1 > h2
 * a negative number if h1 < h2
 */
int compare( huge *h1, huge *h2 )
{
  int i, j;

  if ( h1->size > h2->size )
  {
    return 1;
  }

  if ( h1->size < h2->size )
  {
    return -1;
  }

  // Otherwise, sizes are equal, have to actually compare.
  // only have to compare "hi-int", since the lower ints
  // can't change the comparison.
  i = j = 0;
  
  // Otherwise, keep searching through the representational integers
  // until one is bigger than another - once we've found one, it's 
  // safe to stop, since the "lower order bytes" can't affect the
  // comparison
  while ( i < h1->size && j < h2->size )
  {
    if ( h1->rep[ i ] < h2->rep[ j ] )
    {
      return -1;
    }
    else if ( h1->rep[ i ] > h2->rep[ j ] )
    {
      return 1;
    }
    i++;
    j++;
  } 

  // If we got all the way to the end without a comparison, the 
  // two are equal
  return 0;
}

static void right_shift( huge *h1 )
{
  int i;
  unsigned int old_carry, carry = 0;

  i = 0;
  do
  {
    old_carry = carry;
    carry = ( h1->rep[ i ] & 0x01 ) << 7;
    h1->rep[ i ] = ( h1->rep[ i ] >> 1 ) | old_carry;
  }
  while ( ++i < h1->size );

  contract( h1 );
}

/**
 * dividend = numerator, divisor = denominator
 *
 * Note that this process destroys divisor (and, of course,
 * overwrites quotient). The dividend is the remainder of the 
 * division (if that's important to the caller). The divisor will 
 * be modified by this routine, but it will end up back where it
 * started.
 */
void divide( huge *dividend, huge *divisor, huge *quotient )
{
  int bit_size, bit_position;

  // "bit_position" keeps track of which bit, of the quotient, 
  // is being set or cleared on the current operation.
  bit_size = bit_position = 0;

  // First, left-shift divisor until it's >= than the dividend
  while ( compare( divisor, dividend ) < 0 )
  {
    left_shift( divisor );
    bit_size++;
  }

  // overestimates a bit in some cases
  if ( quotient )
  {
    quotient->sign = !( dividend->sign == dividend->sign );
    quotient->size = ( bit_size / 8 ) + 1;
    quotient->rep = ( unsigned char * ) 
      calloc(quotient->size, sizeof( unsigned char ) );
    memset( quotient->rep, 0, quotient->size );
  }
  
  bit_position = 8 - ( bit_size % 8 ) - 1;

  do
  {
    if ( compare( divisor, dividend ) <= 0 )
    {
      subtract_magnitude( dividend, divisor );  // dividend -= divisor
      if ( quotient )
      {
        quotient->rep[ ( int ) ( bit_position / 8 ) ] |=
          ( 0x80 >> ( bit_position % 8 ) );
      }
    }

    if ( bit_size )
    {
      right_shift( divisor );
    }
    bit_position++;
  }
  while ( bit_size-- );
}

/**
 * Raise h1 to the power of exp. Return the result in h1.
 */
void exponentiate( huge *h1, huge *exp )
{
  int i = exp->size, mask;
  huge tmp1, tmp2;

  set_huge( &tmp1, 0 );
  set_huge( &tmp2, 0 );

  copy_huge( &tmp1, h1 );
  set_huge( h1, 1 );

  do
  {
    i--;
    for ( mask = 0x01; mask; mask <<= 1 )
    {
      if ( exp->rep[ i ] & mask )
      {
        multiply( h1, &tmp1 );
      }

      // Square tmp1
      copy_huge( &tmp2, &tmp1 );
      multiply( &tmp1, &tmp2 );
    }
  }
  while ( i );

  free_huge( &tmp1 );
  free_huge( &tmp2 );
}

/**
 * Compute c = m^e mod n.
 *
 * Note that this same routine is used for encryption and 
 * decryption; the only difference is in the exponent passed in.
 * This is the "exponentiate" algorithm, with the addition of a
 * modulo computation at each stage. 
 */
void mod_pow( huge *h1, huge *exp, huge *n, huge *h2 )
{
  unsigned int i = exp->size;
  unsigned char mask;
  huge tmp1, tmp2; 

  set_huge( &tmp1, 0 );
  set_huge( &tmp2, 0 );

  copy_huge( &tmp1, h1 );
  set_huge( h2, 1 );

  do
  {
    i--;
    for ( mask = 0x01; mask; mask <<= 1 )
    {
      if ( exp->rep[ i ] & mask )
      {
        multiply( h2, &tmp1 );
        divide( h2, n, NULL );
      }
      // square tmp1
      copy_huge( &tmp2, &tmp1 );
      multiply( &tmp1, &tmp2 );
      divide( &tmp1, n, NULL );
    }
  }
  while ( i );

  free_huge( &tmp1 );
  free_huge( &tmp2 );

  // Result is now in "h2"
}

void inv( huge *z, huge *a )
{
  huge i, j, y2, y1, y, quotient, remainder, a_temp;

  set_huge( &i, 1 );  // initialize for copy
  set_huge( &j, 1 );  // initialize for copy
  set_huge( &remainder, 1 );  // initialize for copy
  set_huge( &y, 1 );

  set_huge( &a_temp, 1 );

  set_huge( &y2, 0 );
  set_huge( &y1, 1 );
  
  copy_huge( &i, a );
  copy_huge( &j, z );
  if ( z->sign )
  {
    divide( &j, a, NULL );
    // force positive remainder always
    j.sign = 0;
    subtract( &j, a );
  }

  while ( !( ( j.size == 1 ) && ( !j.rep[ 0 ] ) ) )
  {
    copy_huge( &remainder, &i );
    copy_huge( &i, &j );
    divide( &remainder, &j, &quotient );

    multiply( &quotient, &y1 ); // quotient = y1 * quotient
    copy_huge( &y, &y2 );
    subtract( &y, &quotient );  // y = y2 - ( y1 * quotient )

    copy_huge( &j, &remainder );
    copy_huge( &y2, &y1 );
    copy_huge( &y1, &y );
  }

  copy_huge( z, &y2 );
  copy_huge( &a_temp, a );
  divide( z, &a_temp, NULL );  // inv_z = y2 % a

  if ( z->sign )
  {
    z->sign = 0;
    subtract( z, &a_temp );
    if ( z->sign )
    {
      z->sign = 0;
    }
  }
}

___slogan_obj huge_to_sobj(huge *h)
{
  ___slogan_obj result = ___alloc_u8array(h->size);
  memcpy(___BODY(result), h->rep, h->size);
  return result;
}

