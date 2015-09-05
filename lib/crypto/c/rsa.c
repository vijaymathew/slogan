/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "slogan.h"
#include "huge.h"

struct rsa_key
{
  huge *modulus;
  huge *exponent;
};

/**
 * Compute c = m^e mod n.
 */
void rsa_compute(huge *m, huge *e, huge *n, huge *c)
{
  huge counter;
  huge one;

  init_huge(&counter);
  init_huge(&one);
  copy_huge(c, m);
  set_huge(&counter, 1);
  set_huge(&one, 1);
  while (compare(&counter, e) < 0)
    {
      multiply(c, m);
      add(&counter, &one);
    }

  divide(c, n, NULL);

  release_huge(&counter);
  release_huge(&one);
  // Remainder (result) is now in c
}

/**
 * The input should be broken up into n-bit blocks, where n is the
 * length in bits of the modulus. The output will always be n bits
 * or less. Per RFC 2313, there must be at least 8 bytes of padding
 * to prevent an attacker from trying all possible padding bytes.
 *
 * output will be allocated by this routine, must be freed by the
 * caller.
 *
 * returns the length of the data encrypted in output
 */
int rsa_encrypt(unsigned char *input,
                unsigned int len,
                unsigned char **output,
                struct rsa_key *public_key)
{
  int i;
  huge c, m;
  int modulus_length = public_key->modulus->size;
  int block_size;
  unsigned char *padded_block = (unsigned char *) malloc(modulus_length);
  int encrypted_size = 0;

  *output = NULL;
  
  while (len)
    {
      init_huge(&c);
      init_huge(&m);

      encrypted_size += modulus_length;
      block_size = (len < modulus_length - 11) ? 
        len : (modulus_length - 11);
      memset(padded_block, 0, modulus_length);
      memcpy(padded_block + (modulus_length - block_size), 
             input, block_size);
      // set block type
      padded_block[1] = 0x02;

      for (i = 2; i < (modulus_length - block_size - 1); i++)
        {
          // TODO make these random
          padded_block[i] = i;
        }
    
      load_huge(&m, padded_block, modulus_length);
      mod_pow(&m, public_key->exponent, public_key->modulus, &c);

      *output = (unsigned char *) realloc(*output, encrypted_size);

      unload_huge(&c, *output + (encrypted_size - modulus_length), 
                  modulus_length);

      len -= block_size;
      input += block_size;

      release_huge(&m);
      release_huge(&c);
    } 
  
  free(padded_block);
  
  return encrypted_size;
}

/**
 * Convert the input into key-length blocks and decrypt, unpadding
 * each time.
 * Return -1 if the input is not an even multiple of the key modulus
 * length or if the padding type is not 2, otherwise return the
 * length of the decrypted data.
 */
int rsa_decrypt(unsigned char *input,
                unsigned int len, 
                unsigned char **output,
                struct rsa_key *private_key)
{
  int i, out_len = 0;
  huge c, m;
  int modulus_length = private_key->modulus->size;
  unsigned char *padded_block = (unsigned char *) malloc(
                                                         modulus_length);
   
  *output = NULL;
  
  while (len)
    {
      if (len < modulus_length)
        {
          fprintf(stderr, "Error - input must be an even multiple \
        of key modulus %d (got %d)\n",
                  private_key->modulus->size, len);
          free(padded_block);
          return -1;
        }

      init_huge(&c);
      init_huge(&m);
      
      load_huge(&c, input, modulus_length);
      mod_pow(&c, private_key->exponent, 
              private_key->modulus, &m);
    
      unload_huge(&m, padded_block, modulus_length);
    
      if (padded_block[1] > 0x02)
        {
          fprintf(stderr, "Decryption error or unrecognized block \
        type %d.\n", padded_block[1]);
          release_huge(&c); 
          release_huge(&m);
          free(padded_block);
          return -1;
        }
    
      // Find next 0 byte after the padding type byte; this signifies
      // start-of-data
      i = 2;
      while (padded_block[i++]);
    
      out_len += modulus_length - i;
      *output = realloc(*output, out_len);
      memcpy(*output + (out_len - (modulus_length - i)),
             padded_block + i, modulus_length - i);
    
      len -= modulus_length;
      input += modulus_length;
      release_huge(&c);
      release_huge(&m);
    } 
  
  free(padded_block);
  
  return out_len;
} 

___slogan_obj crypto_rsa_encdec(___slogan_obj key,
                                ___slogan_obj u8arr_input,
                                ___slogan_obj i_input_len,
                                int enc)
{
  int input_len;
  ___slogan_obj modulus;
  ___slogan_obj exponent;
  int modulus_len;
  int exponent_len;
  ___slogan_obj result;
  int size;
  unsigned char *data;
  struct rsa_key pkey;

  modulus = ___head(key);
  exponent = ___tail(key);
  ___slogan_obj_to_int(___head(modulus), &modulus_len);
  ___slogan_obj_to_int(___head(exponent), &exponent_len);
  ___slogan_obj_to_int(i_input_len, &input_len);
  
  pkey.modulus = (huge *)malloc(sizeof(huge));
  pkey.exponent = (huge *)malloc(sizeof(huge));
  init_huge(pkey.modulus);
  init_huge(pkey.exponent);
  load_huge(pkey.modulus, (const unsigned char *)___BODY(___tail(modulus)), modulus_len);
  load_huge(pkey.exponent, (const unsigned char *)___BODY(___tail(exponent)), exponent_len);

  if (enc)
    size = rsa_encrypt((unsigned char *)___BODY(u8arr_input),
                       input_len, &data, &pkey);
  else
    size = rsa_decrypt((unsigned char *)___BODY(u8arr_input),
                       input_len, &data, &pkey);

  if (size <= 0)
    result = ___FAL;
  else
    {
      result = ___alloc_u8array(size);
      memcpy(___BODY (result), data, size);
      free(data);
    }

  release_huge(pkey.modulus);
  release_huge(pkey.exponent);
  free(pkey.modulus);
  free(pkey.exponent);
  return result;
}

___slogan_obj crypto_rsa_encrypt(___slogan_obj public_key,
                                 ___slogan_obj u8arr_input,
                                 ___slogan_obj i_input_len)
{
  return crypto_rsa_encdec(public_key, u8arr_input, i_input_len, 1);
}

___slogan_obj crypto_rsa_decrypt(___slogan_obj private_key,
                                 ___slogan_obj u8arr_input,
                                 ___slogan_obj i_input_len)
{
  return crypto_rsa_encdec(private_key, u8arr_input, i_input_len, 0);
}

