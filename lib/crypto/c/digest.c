/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "digest.h"
#include "md5.h"
#include "sha.h"
#include "slogan.h"

typedef void (*block_operate)(const unsigned char *input, unsigned int hash[]);
typedef void (*block_finalize)(unsigned char *block, int length);
  
/**
 * Generic digest hash computation. The hash should be set to its initial
 * value *before* calling this function.
 */
int digest_hash( unsigned char *input, 
                 int len, 
                 unsigned int *hash, 
                 void (*block_operate)(const unsigned char *input, 
                 unsigned int hash[] ),
                 void (*block_finalize)(unsigned char *block, int length ) )
{
  unsigned char padded_block[ DIGEST_BLOCK_SIZE ];
  int length_in_bits = len * 8;

  while ( len >= INPUT_BLOCK_SIZE )
  {
    // Special handling for blocks between 56 and 64 bytes
    // (not enough room for the 8 bytes of length, but also
    // not enough to fill up a block)
    if ( len < DIGEST_BLOCK_SIZE )
    {
     memset( padded_block, 0, sizeof( padded_block ) );
     memcpy( padded_block, input, len );
     padded_block[ len ] = 0x80;
     block_operate( padded_block, hash );

     input += len;
     len = -1;
   }
   else
   {
     block_operate( input, hash );

     input += DIGEST_BLOCK_SIZE;
     len -= DIGEST_BLOCK_SIZE;
   }
  }

  memset( padded_block, 0, sizeof( padded_block ) );
  if ( len >= 0 )
  {
    memcpy( padded_block, input, len );
    padded_block[ len ] = 0x80;
  }

  block_finalize( padded_block, length_in_bits );

  block_operate( padded_block, hash );

  return 0;
}

void update_digest( digest_ctx *context, const unsigned char *input, int input_len )
{
  context->input_len += input_len;

  // Process any left over from the last call to "update_digest"
  if ( context->block_len > 0 )
  {
    // How much we need to make a full block
    int borrow_amt = DIGEST_BLOCK_SIZE - context->block_len;

    if ( input_len < borrow_amt )
    {
      memcpy( context->block + context->block_len, input, input_len );
      context->block_len += input_len;
      input_len = 0;
    }
    else
    {
      memcpy( context->block + context->block_len, input, borrow_amt );
      context->block_operate( context->block, context->hash );
      context->block_len = 0;
      input += borrow_amt;
      input_len -= borrow_amt;
   }
 }

  while ( input_len >= DIGEST_BLOCK_SIZE )
  {
  context->block_operate( input, context->hash );

    input += DIGEST_BLOCK_SIZE;
    input_len -= DIGEST_BLOCK_SIZE;
  }

  // Have some non-aligned data left over; save it for next call, or
  // "finalize" call.
  if ( input_len > 0 )
  {
    memcpy( context->block, input, input_len );
    context->block_len = input_len;
  }
}

/**
 * Process whatever's left over in the context buffer, append
 * the length in bits, and update the hash one last time.
 */
void finalize_digest( digest_ctx *context )
{
  memset( context->block + context->block_len, 0, DIGEST_BLOCK_SIZE -
    context->block_len );
  context->block[ context->block_len ] = 0x80;
  // special handling if the last block is < 64 but > 56
  if ( context->block_len >= INPUT_BLOCK_SIZE )
  {
    context->block_operate( context->block, context->hash );
    context->block_len = 0;
  memset( context->block + context->block_len, 0, DIGEST_BLOCK_SIZE -
    context->block_len );
  }
  // Only append the length for the very last block
  // Technically, this allows for 64 bits of length, but since we can only
  // process 32 bits worth, we leave the upper four bytes empty
  context->block_finalize( context->block, context->input_len * 8 );

  context->block_operate( context->block, context->hash );
}

void crypto_digest(___slogan_obj itype,
                   ___slogan_obj u8arrin,
                   ___slogan_obj inlen,
                   ___slogan_obj arrout)
{
  int type;
  int len;
  unsigned int *hash;
  int hash_len;
  unsigned int *init_hash;
  block_operate opr;
  block_finalize fnl;
  int i;
  ___slogan_obj *outarr;

  ___slogan_obj_to_int(itype, &type);
  ___slogan_obj_to_int(inlen, &len);

  if (type == 0) /* MD5 */
    {
      hash_len = MD5_RESULT_SIZE;
      init_hash = md5_initial_hash;
      opr = md5_block_operate;
      fnl = md5_finalize;
    }
  else if (type == 1) /* SHA1 */
    {
      hash_len = SHA1_RESULT_SIZE;
      init_hash = sha1_initial_hash;
      opr = sha1_block_operate;
      fnl = sha1_finalize;
    }
  else if (type == 2) /* SHA256 */
    {
      hash_len = SHA256_RESULT_SIZE;
      init_hash = sha256_initial_hash;
      opr = sha256_block_operate;
      fnl = sha1_finalize;
    }
  else
    {
      fprintf(stderr, "unsupported digest algorithm");
      exit(2);
    }

  hash = malloc(sizeof(int) * hash_len);
  assert(hash != NULL);
  memcpy(hash, init_hash, sizeof(int) * hash_len);
  digest_hash((unsigned char *)___BODY(u8arrin), len, hash, opr, fnl);

  outarr = ___BODY(arrout);
  for (i = 0; i < hash_len; ++i)
    outarr[i] = ___fix(hash[i]);
  free(hash);
}

