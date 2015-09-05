/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#ifndef SHA_H
#define SHA_H

#define SHA1_RESULT_SIZE 5
#define SHA1_BYTE_SIZE SHA1_RESULT_SIZE * sizeof(int)

#define SHA256_RESULT_SIZE 8
#define SHA256_BYTE_SIZE SHA256_RESULT_SIZE * sizeof(int)

#include "digest.h"

unsigned int sha1_initial_hash[SHA1_RESULT_SIZE];
unsigned int sha256_initial_hash[SHA256_RESULT_SIZE];
void sha1_block_operate(const unsigned char *block, unsigned int hash[SHA1_RESULT_SIZE]);
void sha256_block_operate(const unsigned char *block, unsigned int hash[SHA256_RESULT_SIZE]);
void sha1_finalize(unsigned char *padded_block, int length_in_bits);
void init_sha1_digest(digest_ctx *context);
void init_sha256_digest(digest_ctx *context);
void release_sha1_digest(digest_ctx *context);
void release_sha256_digest(digest_ctx *context);

#endif
