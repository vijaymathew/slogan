/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#ifndef HMAC_H
#define HMAC_H

void hmac( const unsigned char *key, 
     int key_length, 
     const unsigned char *text, 
     int text_length,
    digest_ctx *digest );

#endif
