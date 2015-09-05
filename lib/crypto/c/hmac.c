/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "slogan.h"
#include "sha.h"
#include "md5.h"
#include "digest.h"
#include "hmac.h"

/**
 * Note: key_length, text_length, hash_block_length are in bytes.
 * hash_code_length is in ints.
 */
void hmac(const unsigned char *key, 
          int key_length, 
          const unsigned char *text, 
          int text_length,
          digest_ctx *digest)
/*
  void (*hash_block_operate)(const unsigned char *input, unsigned int hash[]),
  void (*hash_block_finalize)(unsigned char *block, int length),
  int hash_block_length,
  int hash_code_length,
  unsigned int *hash_out)
*/
{
  unsigned char ipad[DIGEST_BLOCK_SIZE];
  unsigned char opad[DIGEST_BLOCK_SIZE];
  digest_ctx hash1;
  int i;

  // TODO if key_length > hash_block_length, should hash it using "hash_function"
  // first and then use that as the key.
  assert(key_length < DIGEST_BLOCK_SIZE);

  // "cheating"; copy the supplied digest context in here, since we don't
  // know which digest algorithm is being used
  memcpy(&hash1, digest, sizeof(digest_ctx));
  hash1.hash = (unsigned int *) malloc(hash1.hash_len * sizeof(unsigned int));
  memcpy(hash1.hash, digest->hash, hash1.hash_len * sizeof(unsigned int));
 
  memset(ipad, 0x36, DIGEST_BLOCK_SIZE);

  for (i = 0; i < key_length; i++)
    {
      ipad[i] ^= key[i];
    }

  update_digest(&hash1, ipad, DIGEST_BLOCK_SIZE);
  update_digest(&hash1, text, text_length);
  finalize_digest(&hash1);

  memset(opad, 0x5C, DIGEST_BLOCK_SIZE);

  for (i = 0; i < key_length; i++)
    {
      opad[i] ^= key[i];
    }

  update_digest(digest, opad, DIGEST_BLOCK_SIZE);
  update_digest(digest, (unsigned char *) hash1.hash, 
                hash1.hash_len * sizeof(int));
  finalize_digest(digest);

  free(hash1.hash);
}

___slogan_obj crypto_hmac(___slogan_obj u8arr_key,
                          ___slogan_obj ikey_len,
                          ___slogan_obj u8arr_text,
                          ___slogan_obj itext_len,
                          ___slogan_obj ihash_type)
{
  int i;
  digest_ctx digest;
  int key_len;
  int text_len;
  int hash_type;
  ___slogan_obj result;
  size_t result_len;
  ___slogan_obj *r;
  
  ___slogan_obj_to_int(ihash_type, &hash_type);
  ___slogan_obj_to_int(ikey_len, &key_len);
  ___slogan_obj_to_int(itext_len, &text_len);

  if (hash_type == 1)
    init_sha1_digest(&digest);
  else if (hash_type == 0)
    init_md5_digest(&digest);
  else
    {
      fprintf(stderr, "unsupportd hash algorithm");
      exit(1);
    }

  hmac((unsigned char *)___BODY(u8arr_key), key_len,
       (unsigned char *)___BODY(u8arr_text), text_len,
       &digest);
  result_len = digest.hash_len * sizeof(int);
  result = ___alloc_u8array(result_len);
  memcpy(___BODY(result), (unsigned char *)digest.hash, result_len);

  if (hash_type == 1) release_sha1_digest(&digest);
  else release_md5_digest(&digest);
  
  return result;
}
