/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#ifndef MD5_H
#define MD5_H

// Size of MD5 hash in ints (128 bits)
#define MD5_RESULT_SIZE 4
#define MD5_BYTE_SIZE MD5_RESULT_SIZE * sizeof( int )

#include "digest.h"

unsigned int md5_initial_hash[ MD5_RESULT_SIZE ];

int md5_hash( const unsigned char *input, 
       int len, 
       unsigned int hash[ MD5_RESULT_SIZE ] );

void md5_block_operate( const unsigned char *input, 
                unsigned int hash[ MD5_RESULT_SIZE ] );
void md5_finalize( unsigned char *padded_block, int length_in_bits );
void new_md5_digest( digest_ctx *context );

#endif
