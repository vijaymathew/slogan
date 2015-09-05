#ifndef BASE64_H
#define BASE64_H

void base64_encode(const unsigned char *input, int len, unsigned char *output);
int base64_decode(const unsigned char *input, int len, unsigned char *output);

#endif
