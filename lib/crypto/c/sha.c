/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include "sha.h"

static const int k[] = { 
  0x5a827999, // 0 <= t <= 19
  0x6ed9eba1, // 20 <= t <= 39
  0x8f1bbcdc, // 40 <= t <= 59
  0xca62c1d6 // 60 <= t <= 79
};

// ch is functions 0 - 19
unsigned int ch(unsigned int x, unsigned int y, unsigned int z)
{
  return (x & y) ^ (~x & z);
}

// parity is functions 20 - 39 & 60 - 79
unsigned int parity(unsigned int x, unsigned int y, unsigned int z)
{
  return x ^ y ^ z;
}

// maj is functions 40 - 59
unsigned int maj(unsigned int x, unsigned int y, unsigned int z)
{
  return (x & y) ^ (x & z) ^ (y & z);
}

unsigned int rotr(unsigned int x, unsigned int n)
{
  return (x >> n) | ((x) << (32 - n));
}

unsigned int shr(unsigned int x, unsigned int n)
{
  return x >> n;
}

unsigned int sigma_rot(unsigned int x, int i)
{
  return rotr(x, i ? 6 : 2) ^ rotr(x, i ? 11 : 13) ^ rotr(x, i ? 25 : 22);
}

unsigned int sigma_shr(unsigned int x, int i)
{
  return rotr(x, i ? 17 : 7) ^ rotr(x, i ? 19 : 18) ^ shr(x, i ? 10 : 3);
}

void sha1_block_operate(const unsigned char *block, unsigned int hash[SHA1_RESULT_SIZE])
{
  unsigned int W[80];
  unsigned int t = 0;
  unsigned int a, b, c, d, e, T;

  // First 16 blocks of W are the original 16 blocks of the input
  for (t = 0; t < 80; t++)
    {
      if (t < 16)
        {
          W[t] = (block[(t * 4)] << 24) | 
            (block[(t * 4) + 1] << 16) |
            (block[(t * 4) + 2] << 8) |
            (block[(t * 4) + 3]);
        }
      else
        {
          W[t] = W[t - 3] ^ 
            W[t - 8] ^
            W[t - 14] ^
            W[t - 16];
          // Rotate left operation, simulated in C
          W[t] = (W[t] << 1) | ((W[t] & 0x80000000) >> 31);
        }
    }
 
  hash[0] = ntohl(hash[0]);
  hash[1] = ntohl(hash[1]);
  hash[2] = ntohl(hash[2]);
  hash[3] = ntohl(hash[3]);
  hash[4] = ntohl(hash[4]);
  
  a = hash[0];
  b = hash[1];
  c = hash[2];
  d = hash[3];
  e = hash[4];

  for (t = 0; t < 80; t++)
    {
      T = ((a << 5) | (a >> 27)) + e + k[(t / 20)] + W[t];

      if (t <= 19)
        {
          T += ch(b, c, d);
        }
      else if (t <= 39)
        {
          T += parity(b, c, d);
        }
      else if (t <= 59)
        {
          T += maj(b, c, d);
        }
      else
        {
          T += parity(b, c, d);
        }

      e = d;
      d = c;
      c = ((b << 30) | (b >> 2));
      b = a;
      a = T;
    }

  hash[0] += a;
  hash[1] += b;
  hash[2] += c;
  hash[3] += d;
  hash[4] += e;

  hash[0] = htonl(hash[0]);
  hash[1] = htonl(hash[1]);
  hash[2] = htonl(hash[2]);
  hash[3] = htonl(hash[3]);
  hash[4] = htonl(hash[4]);
}

unsigned int sha256_initial_hash[] = {
  0x67e6096a,
  0x85ae67bb,
  0x72f36e3c,
  0x3af54fa5,
  0x7f520e51,
  0x8c68059b,
  0xabd9831f,
  0x19cde05b
};

void sha256_block_operate(const unsigned char *block,
                          unsigned int hash[8])
{
  unsigned int W[64];
  unsigned int a, b, c, d, e, f, g, h;
  unsigned int T1, T2;
  int t, i;

  /**
   * The first 32 bits of the fractional parts of the cube roots
   * of the first sixty-four prime numbers.
   */
  static const unsigned int k[] =
    {
      0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 
      0x923f82a4, 0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 
      0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 
      0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 
      0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 
      0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 
      0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
      0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 
      0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 
      0x5b9cca4f, 0x682e6ff3, 0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 
      0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
    };

  // deal with little-endian-ness
  for (i = 0; i < 8; i++)
    {
      hash[i] = ntohl(hash[i]);
    }

  for (t = 0; t < 64; t++)
    {
      if (t <= 15 )
        {
          W[t] = (block[( t * 4 )] << 24) |
            (block[( t * 4 ) + 1] << 16) |
            (block[( t * 4 ) + 2] << 8) |
            (block[( t * 4 ) + 3]);
        }
      else
        {
          W[t] = sigma_shr(W[t - 2], 1) +
            W[t - 7] +
            sigma_shr(W[t - 15], 0) +
            W[t - 16];
        }
    }
  
  a = hash[0];
  b = hash[1];
  c = hash[2];
  d = hash[3];
  e = hash[4];
  f = hash[5];
  g = hash[6];
  h = hash[7];

  for (t = 0; t < 64; t++)
    {
      T1 = h + sigma_rot(e, 1) + ch(e, f, g) + k[t] + W[t];
      T2 = sigma_rot(a, 0) + maj(a, b, c);
      h = g;
      g = f;
      f = e;
      e = d + T1;
      d = c;
      c = b;
      b = a;
      a = T1 + T2;
    }

  hash[0] = a + hash[0];
  hash[1] = b + hash[1];
  hash[2] = c + hash[2];
  hash[3] = d + hash[3];
  hash[4] = e + hash[4];
  hash[5] = f + hash[5];
  hash[6] = g + hash[6];
  hash[7] = h + hash[7];

  // deal with little-endian-ness
  for (i = 0; i < 8; i++)
    {
      hash[i] = htonl(hash[i]);
    }
} 

#define SHA1_INPUT_BLOCK_SIZE 56
#define SHA1_BLOCK_SIZE 64

unsigned int sha1_initial_hash[] = { 
  0x01234567,
  0x89abcdef,
  0xfedcba98,
  0x76543210,
  0xf0e1d2c3
};	

int sha1_hash(unsigned char *input, int len, 
              unsigned int hash[SHA1_RESULT_SIZE])
{
  unsigned char padded_block[SHA1_BLOCK_SIZE];
  int length_in_bits = len * 8;
  
  
  hash[0] = sha1_initial_hash[0];
  hash[1] = sha1_initial_hash[1];
  hash[2] = sha1_initial_hash[2];
  hash[3] = sha1_initial_hash[3];
  hash[4] = sha1_initial_hash[4];

  while (len >= SHA1_INPUT_BLOCK_SIZE)
    {
      if (len < SHA1_BLOCK_SIZE)
        {
          memset(padded_block, 0, sizeof(padded_block));
          memcpy(padded_block, input, len);
          padded_block[len] = 0x80;
          sha1_block_operate(padded_block, hash);

          input += len;
          len = -1;
        }
      else
        {
          sha1_block_operate(input, hash);

          input += SHA1_BLOCK_SIZE;
          len -= SHA1_BLOCK_SIZE;
        }
    }

  memset(padded_block, 0, sizeof(padded_block));
  if (len >= 0)
    {
      memcpy(padded_block, input, len);
      padded_block[len] = 0x80;
    }

  padded_block[SHA1_BLOCK_SIZE - 4] = (length_in_bits & 0xFF000000) >> 24;
  padded_block[SHA1_BLOCK_SIZE - 3] = (length_in_bits & 0x00FF0000) >> 16;
  padded_block[SHA1_BLOCK_SIZE - 2] = (length_in_bits & 0x0000FF00) >> 8;
  padded_block[SHA1_BLOCK_SIZE - 1] = (length_in_bits & 0x000000FF);

  sha1_block_operate(padded_block, hash);

  return 0;
}

void sha1_finalize(unsigned char *padded_block, int length_in_bits)
{
  padded_block[SHA1_BLOCK_SIZE - 4] = (length_in_bits & 0xFF000000) >> 24;
  padded_block[SHA1_BLOCK_SIZE - 3] = (length_in_bits & 0x00FF0000) >> 16;
  padded_block[SHA1_BLOCK_SIZE - 2] = (length_in_bits & 0x0000FF00) >> 8;
  padded_block[SHA1_BLOCK_SIZE - 1] = (length_in_bits & 0x000000FF);
}

void init_sha1_digest(digest_ctx *context)
{
  context->hash_len = 5;
  context->input_len = 0;
  context->block_len = 0;
  context->hash = (unsigned int *) 
    malloc(context->hash_len * sizeof(unsigned int));
  memcpy(context->hash, sha1_initial_hash, 
         context->hash_len * sizeof( unsigned int));
  memset(context->block, '\0', DIGEST_BLOCK_SIZE);
  context->block_operate = sha1_block_operate;
  context->block_finalize = sha1_finalize;
}

void init_sha256_digest(digest_ctx *context)
{
  context->hash_len = 8;
  context->input_len = 0;
  context->block_len = 0;
  context->hash = (unsigned int *) malloc(context->hash_len * sizeof(unsigned int));
  memcpy(context->hash, sha256_initial_hash, context->hash_len * sizeof(unsigned int));
  memset(context->block, '\0', DIGEST_BLOCK_SIZE);
  context->block_operate = sha256_block_operate;
  context->block_finalize = sha1_finalize;
}

void release_sha1_digest(digest_ctx *context)
{
  if (context != NULL && context->hash != NULL)
    free(context->hash);
}

void release_sha256_digest(digest_ctx *context)
{
  if (context != NULL && context->hash != NULL)
    free(context->hash);
}

int crypto_sha1_result_size()
{
  return SHA1_RESULT_SIZE;
}

int crypto_sha256_result_size()
{
  return SHA256_RESULT_SIZE;
}
