/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#ifndef DIGEST_H
#define DIGEST_H

int digest_hash(unsigned char *input,
                int len, 
                unsigned int *hash,
                void (*block_operate)(const unsigned char *input, unsigned int hash[]),
                void (*block_finalize)(unsigned char *block, int length));

#define DIGEST_BLOCK_SIZE 64
#define INPUT_BLOCK_SIZE 56

typedef struct
{
  unsigned int *hash;
  int hash_len;
  unsigned int input_len;

  void (*block_operate)(const unsigned char *input, unsigned int hash[]);
  void (*block_finalize)(unsigned char *block, int length);

  // Temporary storage
  unsigned char block[DIGEST_BLOCK_SIZE];
  int block_len;
}
digest_ctx;

void update_digest(digest_ctx *context, const unsigned char *input, int input_len);
void finalize_digest(digest_ctx *context);

#endif
