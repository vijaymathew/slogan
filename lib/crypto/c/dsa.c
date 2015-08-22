/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "sha.h"
#include "digest.h"
#include "dsa.h"

static void generate_message_secret( dsa_params *params, huge *k )
{  
  int i;
  huge q;
  huge one;

  set_huge( &q, 0 ); // initialize this so that copy works
  set_huge( &one, 1 );

  copy_huge( &q, &params->q );
  subtract( &q, &one );

  // XXX the extra + 8 aren't really necessary since we're not generating
  // a random "c"
  k->sign = 0;
  k->size = params->q.size + 8;
  k->rep = malloc( k->size );
  // TODO this should be filled with random bytes
  for ( i = 0; i < k->size; i++ )
  {
    k->rep[ i ] = i + 1;
  }

  // k will become k % ( q - 1 );
  divide( k, &q, NULL );
  add( k, &one );
}

void dsa_sign( dsa_params *params,
       huge *private_key,
       unsigned int *hash,
       int hash_len, 
       dsa_signature *signature )
{
  huge k;
  huge z; 
  huge q;
 
  set_huge( &q, 1 );
 
  generate_message_secret( params, &k );
  // r = ( g ^ k % p ) % q
  mod_pow( &params->g, &k, &params->p, &signature->r );
  copy_huge( &q, &params->q );
  divide( &signature->r, &q, NULL );

  // z = hash(message), only approved with SHA
  load_huge( &z, ( unsigned char * ) hash,
   ( (hash_len * 4 ) < params->q.size ) ? 
   (hash_len * 4 ) : params->q.size );
  
  // s = ( inv(k) * ( z + xr ) ) % q
  inv( &k, &params->q );
  set_huge( &signature->s, 0 );
  copy_huge( &signature->s, private_key );
  multiply( &signature->s, &signature->r );
  add( &signature->s, &z );
  multiply( &signature->s, &k );
  copy_huge( &q, &params->q );
  divide( &signature->s, &q, NULL );

  free_huge( &z );
}

int dsa_verify( dsa_params *params,
                huge *public_key,
                unsigned int *hash,
                int hash_len,
                dsa_signature *signature )
{
  int match;
  huge w, z, u1, u2, q, p;

  set_huge( &q, 1 );
  set_huge( &p, 1 );
  set_huge( &w, 0 );

  // w = inv(s) % q
  copy_huge( &w, &signature->s );
  inv( &w, &params->q );

  // z = hash(message), truncated to sizeof(q)
  load_huge( &z, ( unsigned char * ) hash,
   ( (hash_len * 4 ) < params->q.size ) ? 
   (hash_len * 4 ) : params->q.size );

  // u1 = (zw) % q
  multiply( &z, &w );
  copy_huge( &q, &params->q );
  divide( &z, &params->q, NULL );  // u1 = z

  // u2 = (rw) % q
  multiply( &w, &signature->r );
  copy_huge( &q, &params->q );
  divide( &w, &q, NULL ); // u2 = w

  // v = ( ( ( g^u1) % p * (y^u2) %p ) % p ) % q
  mod_pow( &params->g, &z, &params->p, &u1 );
  mod_pow( public_key, &w, &params->p, &u2 );
  multiply( &u1, &u2 );
  copy_huge( &p, &params->p );
  divide( &u1, &p, NULL );
  copy_huge( &q, &params->q );
  divide( &u1, &q, NULL ); // u1 is "v" now

  // Check to see if v & s match
  match = !compare( &u1, &signature->r );

  free_huge( &w );
  free_huge( &z );
  free_huge( &u1 );
  free_huge( &u2 );
  
  return match;
}

