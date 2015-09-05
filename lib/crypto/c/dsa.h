/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#ifndef DSA_H
#define DSA_H

#include "huge.h"

typedef struct
{ 
  huge g;
  huge p;
  huge q;
} dsa_params;

typedef struct
{
  huge r;
  huge s;
} dsa_signature;

void dsa_sign(dsa_params *params,
              huge *private_key,
              unsigned int *hash,
              int hash_len, 
              dsa_signature *signature);
int dsa_verify(dsa_params *params,
               huge *public_key,
               unsigned int *hash,
               int hash_len,
               dsa_signature *signature);

#endif
