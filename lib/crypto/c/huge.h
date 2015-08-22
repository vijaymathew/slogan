/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#ifndef HUGE_H
#define HUGE_H

typedef struct
{
  int sign;
  unsigned int size;
  unsigned char *rep;
}
huge;

void copy_huge( huge *tgt, huge *src );
void set_huge( huge *h, unsigned int val );
int compare( huge *h1, huge *h2 );
void add( huge *h1, huge *h2 );
void subtract( huge *h1, huge *h2 );
void multiply( huge *h1, huge *h2 );
void divide( huge *dividend, huge *divisor, huge *quotient );
void load_huge( huge *h, const unsigned char *bytes, int length );
void unload_huge( const huge *h, unsigned char *bytes, int length );
void free_huge( huge *h );
void mod_pow( huge *h1, huge *exp, huge *n, huge *h2 );
void inv( huge *z, huge *a );
void contract( huge *h );

#endif
