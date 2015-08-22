/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "sha.h"
#include "md5.h"
#include "digest.h"
#include "hmac.h"

/**
 * Note: key_length, text_length, hash_block_length are in bytes.
 * hash_code_length is in ints.
 */
void hmac( const unsigned char *key, 
      int key_length, 
      const unsigned char *text, 
      int text_length,
      digest_ctx *digest )
      /*
      void (*hash_block_operate)(const unsigned char *input, unsigned int hash[] ),
      void (*hash_block_finalize)(unsigned char *block, int length ),
      int hash_block_length,
      int hash_code_length,
      unsigned int *hash_out )
      */
{
  unsigned char ipad[ DIGEST_BLOCK_SIZE ];
  unsigned char opad[ DIGEST_BLOCK_SIZE ];
  digest_ctx hash1;
  int i;

  // TODO if key_length > hash_block_length, should hash it using "hash_function"
  // first and then use that as the key.
  assert( key_length < DIGEST_BLOCK_SIZE );

  // "cheating"; copy the supplied digest context in here, since we don't
  // know which digest algorithm is being used
  memcpy( &hash1, digest, sizeof( digest_ctx ) );
  hash1.hash = ( unsigned int * ) malloc( 
  hash1.hash_len * sizeof( unsigned int ) );
  memcpy( hash1.hash, digest->hash, hash1.hash_len * sizeof( unsigned int ) );
 
  memset( ipad, 0x36, DIGEST_BLOCK_SIZE );

  for ( i = 0; i < key_length; i++ )
  {
    ipad[ i ] ^= key[ i ];
  }

  update_digest( &hash1, ipad, DIGEST_BLOCK_SIZE );
  update_digest( &hash1, text, text_length );
  finalize_digest( &hash1 );

  memset( opad, 0x5C, DIGEST_BLOCK_SIZE );

  for ( i = 0; i < key_length; i++ )
  {
    opad[ i ] ^= key[ i ];
  }

  update_digest( digest, opad, DIGEST_BLOCK_SIZE );
  update_digest( digest, ( unsigned char * ) hash1.hash, 
  hash1.hash_len * sizeof( int ) );
  finalize_digest( digest );

  free( hash1.hash );
}
