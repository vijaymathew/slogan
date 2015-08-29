/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "md5.h"

unsigned int F( unsigned int x, unsigned int y, unsigned int z )
{
  return ( x & y ) | ( ~x & z );
}

unsigned int G( unsigned int x, unsigned int y, unsigned int z )
{
  return ( x & z ) | ( y & ~z );
}

unsigned int H( unsigned int x, unsigned int y, unsigned int z )
{
  return ( x ^ y ^ z );
}

unsigned int I( unsigned int x, unsigned int y, unsigned int z )
{
  return y ^ ( x | ~z );
}

#define BASE_T 4294967296.0

#define ROUND( F, a, b, c, d, k, s, i ) \
 a = ( a + F( b, c, d ) + x[ k ] + \
   ( unsigned int ) ( BASE_T * fabs( sin( ( double ) i ) ) ) ); \
 a = ( a << s ) | ( a >> ( 32 - s ) ); \
 a += b;

unsigned int md5_initial_hash[ ] = {
  0x67452301,
  0xefcdab89,
  0x98badcfe,
  0x10325476
};

void md5_block_operate( const unsigned char *input, 
                unsigned int hash[ MD5_RESULT_SIZE ] )
{
  unsigned int a, b, c, d;
  int j;
  unsigned int x[ 16 ];
 
  a = hash[ 0 ];
  b = hash[ 1 ];
  c = hash[ 2 ];
  d = hash[ 3 ];

  for ( j = 0; j < 16; j++ )
  {
     x[ j ] = input[ ( j * 4 ) + 3 ] << 24 |
                        input[ ( j * 4 ) + 2 ] << 16 |
                        input[ ( j * 4 ) + 1 ] << 8 |
                        input[ ( j * 4 ) ];
  }
 
  // Round 1
  ROUND( F, a, b, c, d, 0, 7, 1 );
  ROUND( F, d, a, b, c, 1, 12, 2 );
  ROUND( F, c, d, a, b, 2, 17, 3 );
  ROUND( F, b, c, d, a, 3, 22, 4 );
  ROUND( F, a, b, c, d, 4, 7, 5 );
  ROUND( F, d, a, b, c, 5, 12, 6 );
  ROUND( F, c, d, a, b, 6, 17, 7 );
  ROUND( F, b, c, d, a, 7, 22, 8 );
  ROUND( F, a, b, c, d, 8, 7, 9 );
  ROUND( F, d, a, b, c, 9, 12, 10 );
  ROUND( F, c, d, a, b, 10, 17, 11 );
  ROUND( F, b, c, d, a, 11, 22, 12 );
  ROUND( F, a, b, c, d, 12, 7, 13 );
  ROUND( F, d, a, b, c, 13, 12, 14 );
  ROUND( F, c, d, a, b, 14, 17, 15 );
  ROUND( F, b, c, d, a, 15, 22, 16 );
  
  // Round 2
  ROUND( G, a, b, c, d, 1, 5, 17 );
  ROUND( G, d, a, b, c, 6, 9, 18 );
  ROUND( G, c, d, a, b, 11, 14, 19 );
  ROUND( G, b, c, d, a, 0, 20, 20 );
  ROUND( G, a, b, c, d, 5, 5, 21 );
  ROUND( G, d, a, b, c, 10, 9, 22 );
  ROUND( G, c, d, a, b, 15, 14, 23 );
  ROUND( G, b, c, d, a, 4, 20, 24 );
  ROUND( G, a, b, c, d, 9, 5, 25 );
  ROUND( G, d, a, b, c, 14, 9, 26 );
  ROUND( G, c, d, a, b, 3, 14, 27 );
  ROUND( G, b, c, d, a, 8, 20, 28 );
  ROUND( G, a, b, c, d, 13, 5, 29 );
  ROUND( G, d, a, b, c, 2, 9, 30 );
  ROUND( G, c, d, a, b, 7, 14, 31 );
  ROUND( G, b, c, d, a, 12, 20, 32 );
  
  // Round 3
  ROUND( H, a, b, c, d, 5, 4, 33 );
  ROUND( H, d, a, b, c, 8, 11, 34 );
  ROUND( H, c, d, a, b, 11, 16, 35 );
  ROUND( H, b, c, d, a, 14, 23, 36 );
  ROUND( H, a, b, c, d, 1, 4, 37 );
  ROUND( H, d, a, b, c, 4, 11, 38 );
  ROUND( H, c, d, a, b, 7, 16, 39 );
  ROUND( H, b, c, d, a, 10, 23, 40 );
  ROUND( H, a, b, c, d, 13, 4, 41 );
  ROUND( H, d, a, b, c, 0, 11, 42 );
  ROUND( H, c, d, a, b, 3, 16, 43 );
  ROUND( H, b, c, d, a, 6, 23, 44 );
  ROUND( H, a, b, c, d, 9, 4, 45 );
  ROUND( H, d, a, b, c, 12, 11, 46 );
  ROUND( H, c, d, a, b, 15, 16, 47 );
  ROUND( H, b, c, d, a, 2, 23, 48 );
  
  // Round 4
  ROUND( I, a, b, c, d, 0, 6, 49 );
  ROUND( I, d, a, b, c, 7, 10, 50 );
  ROUND( I, c, d, a, b, 14, 15, 51 );
  ROUND( I, b, c, d, a, 5, 21, 52 );
  ROUND( I, a, b, c, d, 12, 6, 53 );
  ROUND( I, d, a, b, c, 3, 10, 54 );
  ROUND( I, c, d, a, b, 10, 15, 55 );
  ROUND( I, b, c, d, a, 1, 21, 56 );
  ROUND( I, a, b, c, d, 8, 6, 57 );
  ROUND( I, d, a, b, c, 15, 10, 58 );
  ROUND( I, c, d, a, b, 6, 15, 59 );
  ROUND( I, b, c, d, a, 13, 21, 60 );
  ROUND( I, a, b, c, d, 4, 6, 61 );
  ROUND( I, d, a, b, c, 11, 10, 62 );
  ROUND( I, c, d, a, b, 2, 15, 63 );
  ROUND( I, b, c, d, a, 9, 21, 64 );
  
  hash[ 0 ] += a;
  hash[ 1 ] += b;
  hash[ 2 ] += c;
  hash[ 3 ] += d; 
} 

#define MD5_BLOCK_SIZE 64
#define MD5_INPUT_BLOCK_SIZE 56
#define MD5_RESULT_SIZE 4

int md5_hash( const unsigned char *input, 
       int len, 
       unsigned int hash[ MD5_RESULT_SIZE ] )
{
  unsigned char padded_block[ MD5_BLOCK_SIZE ];
  int length_in_bits = len * 8;

  // XXX should verify that len < 2^64, but since len is only 32 bits, this won't
  // be a problem.

  hash[ 0 ] = md5_initial_hash[ 0 ];
  hash[ 1 ] = md5_initial_hash[ 1 ];
  hash[ 2 ] = md5_initial_hash[ 2 ];
  hash[ 3 ] = md5_initial_hash[ 3 ];

  while ( len >= MD5_INPUT_BLOCK_SIZE )
  {
    // Special handling for blocks between 56 and 64 bytes
    // (not enough room for the 8 bytes of length, but also
    // not enough to fill up a block)
    if ( len < MD5_BLOCK_SIZE )
    {
      memset( padded_block, 0, sizeof( padded_block ) );
      memcpy( padded_block, input, len );
      padded_block[ len ] = 0x80;
      md5_block_operate( padded_block, hash ); 

      input += len;
      len = -1;
   }
   else
   {
      md5_block_operate( input, hash );

      input += MD5_BLOCK_SIZE;
      len -= MD5_BLOCK_SIZE;
    }
  }

  // There's always at least one padded block at the end, which includes
  // the length of the message
  memset( padded_block, 0, sizeof( padded_block ) );
  if ( len >= 0 )
  {
    memcpy( padded_block, input, len );
    padded_block[ len ] = 0x80;
  }

  // Only append the length for the very last block
  // Technically, this allows for 64 bits of length, but since we can only
  // process 32 bits worth, we leave the upper four bytes empty
  // This is sort of a bizarre concept of "little endian"...
  padded_block[ MD5_BLOCK_SIZE - 5 ] = ( length_in_bits & 0xFF000000 ) >> 24;
  padded_block[ MD5_BLOCK_SIZE - 6 ] = ( length_in_bits & 0x00FF0000 ) >> 16;
  padded_block[ MD5_BLOCK_SIZE - 7 ] = ( length_in_bits & 0x0000FF00 ) >> 8;
  padded_block[ MD5_BLOCK_SIZE - 8 ] = ( length_in_bits & 0x000000FF );

  md5_block_operate( padded_block, hash );

  return 0;
}

void md5_finalize( unsigned char *padded_block, int length_in_bits )
{
  padded_block[ MD5_BLOCK_SIZE - 5 ] = ( length_in_bits & 0xFF000000 ) >> 24;
  padded_block[ MD5_BLOCK_SIZE - 6 ] = ( length_in_bits & 0x00FF0000 ) >> 16;
  padded_block[ MD5_BLOCK_SIZE - 7 ] = ( length_in_bits & 0x0000FF00 ) >> 8;
  padded_block[ MD5_BLOCK_SIZE - 8 ] = ( length_in_bits & 0x000000FF );
}

void new_md5_digest( digest_ctx *context )
{  
  context->hash_len = 4;
  context->input_len = 0;
  context->block_len = 0;
  context->hash = ( unsigned int * ) 
   malloc( context->hash_len * sizeof( unsigned int ) );
  memcpy( context->hash, md5_initial_hash, 
   context->hash_len * sizeof( unsigned int ) );
  memset( context->block, '\0', DIGEST_BLOCK_SIZE );
  context->block_operate = md5_block_operate;
  context->block_finalize = md5_finalize;
} 

int crypto_md5_result_size()
{
  return MD5_RESULT_SIZE;
}
