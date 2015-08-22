/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <arpa/inet.h>
#include "sha.h"
#include "digest.h"
#include "huge.h"
#include "ecdsa.h"

void ecdsa_sign( elliptic_curve *params, 
         huge *private_key,
         unsigned int *hash, 
         int hash_len, 
         dsa_signature *signature )
{
  unsigned char K[] = {
    0x9E, 0x56, 0xF5, 0x09, 0x19, 0x67, 0x84, 0xD9, 0x63, 0xD1, 0xC0, 
    0xA4, 0x01, 0x51, 0x0E, 0xE7, 0xAD, 0xA3, 0xDC, 0xC5, 0xDE, 0xE0, 
    0x4B, 0x15, 0x4B, 0xF6, 0x1A, 0xF1, 0xD5, 0xA6, 0xDE, 0xCE
  };
  huge k;
  point X;
  huge z;

  // This should be a random number between 0 and n-1
  load_huge( &k, ( unsigned char * ) K, sizeof( K ) );

  set_huge( &X.x, 0 );
  set_huge( &X.y, 0 );
  copy_huge( &X.x, &params->G.x );
  copy_huge( &X.y, &params->G.y );

  multiply_point( &X, &k, &params->a, &params->p );

  set_huge( &signature->r, 0 );
  copy_huge( &signature->r, &X.x );
  divide( &signature->r, &params->n, NULL ); // r = x1 % n

  // z is the L_n leftmost bits of hash - cannot be longer than n
  load_huge( &z, ( unsigned char * ) hash,
     ( ( hash_len * 4 ) < params->n.size ) ? ( hash_len * 4 ) : params->n.size );

  // s = k^-1 ( z + r d_a ) % n
  inv( &k, &params->n );
  set_huge( &signature->s, 0 );
  copy_huge( &signature->s, private_key );
  multiply( &signature->s, &signature->r );
  add( &signature->s, &z );
  multiply( &signature->s, &k );
  divide( &signature->s, &params->n, NULL );

  free_huge( &k );
  free_huge( &z );
  free_huge( &X.x );
  free_huge( &X.y );
}

int ecdsa_verify( elliptic_curve *params,
         point *public_key,
         unsigned int *hash,
         int hash_len,
         dsa_signature *signature )
{
  huge z;
  huge w;
  point G;
  point Q;
  int match;

  // w = s^-1 % n
  set_huge( &w, 0 );
  copy_huge( &w, &signature->s );
  inv( &w, &params->n );

  // z is the L_n leftmost bits of hash - cannot be longer than n
  load_huge( &z, ( unsigned char * ) hash, 
   ( ( hash_len * 4 ) < params->n.size ) ? ( hash_len * 4 ) : params->n.size );

  // u1 = zw % n
  multiply( &z, &w );
  divide( &z, &params->n, NULL );  // u1 = z

  // u2 = (rw) % q
  multiply( &w, &signature->r );
  divide( &w, &params->n, NULL ); // u2 = w

  // (x1,y1) = u1 * G + u2 * Q
  set_huge( &G.x, 0 );
  set_huge( &G.y, 0 );
  set_huge( &Q.x, 0 );
  set_huge( &Q.y, 0 );
  copy_huge( &G.x, &params->G.x );
  copy_huge( &G.y, &params->G.y );
  copy_huge( &Q.x, &public_key->x );
  copy_huge( &Q.y, &public_key->y ); 

  multiply_point( &G, &z, &params->a, &params->p );
  multiply_point( &Q, &w, &params->a, &params->p );
  add_points( &G, &Q, &params->p );
 
  // r = x1 % n
  divide( &G.x, &params->n, NULL );

  match = !compare( &G.x, &signature->r );

  free_huge( &z );
  free_huge( &w );
  free_huge( &G.x );
  free_huge( &G.y );
  free_huge( &Q.x );
  free_huge( &Q.y );

  return match;
}
