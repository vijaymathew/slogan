/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#ifndef ECDSA_H
#define ECDSA_H

#include "ecc.h"
#include "dsa.h"

void ecdsa_sign(elliptic_curve *params, 
         huge *private_key,
         unsigned int *hash, 
         int hash_len, 
         dsa_signature *signature);
int ecdsa_verify(elliptic_curve *params,
         point *public_key,
         unsigned int *hash,
         int hash_len,
         dsa_signature *signature);

#endif
