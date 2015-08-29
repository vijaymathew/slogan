#ifndef ASN1_H
#define ASN1_H

struct asn1struct
{
 int constructed;  // bit 6 of the identifier byte
 int tag_class;   // bits 7-8 of the identifier byte
 int tag;      // bits 1-5 of the identifier byte
 int length;
 const unsigned char *data;
 struct asn1struct *children;
 struct asn1struct *next;
};

#define ASN1_CLASS_UNIVERSAL 0
#define ASN1_CLASS_APPLICATION 1
#define ASN1_CONTEXT_SPECIFIC 2
#define ASN1_PRIVATE 3

#define ASN1_BER 0
#define ASN1_BOOLEAN 1
#define ASN1_INTEGER 2
#define ASN1_BIT_STRING 3
#define ASN1_OCTET_STRING 4
#define ASN1_NULL 5
#define ASN1_OBJECT_IDENTIFIER 6
#define ASN1_OBJECT_DESCRIPTOR 7
#define ASN1_INSTANCE_OF_EXTERNAL 8
#define ASN1_REAL 9
#define ASN1_ENUMERATED 10
#define ASN1_EMBEDDED_PPV 11
#define ASN1_UTF8_STRING 12
#define ASN1_RELATIVE_OID 13
// 14 & 15 undefined
#define ASN1_SEQUENCE 16
#define ASN1_SET 17
#define ASN1_NUMERIC_STRING 18
#define ASN1_PRINTABLE_STRING 19
#define ASN1_TELETEX_STRING 20
#define ASN1_T61_STRING 20
#define ASN1_VIDEOTEX_STRING 21
#define ASN1_IA5_STRING 22
#define ASN1_UTC_TIME 23
#define ASN1_GENERALIZED_TIME 24
#define ASN1_GRAPHIC_STRING 25
#define ASN1_VISIBLE_STRING 26
#define ASN1_ISO64_STRING 26
#define ASN1_GENERAL_STRING 27
#define ASN1_UNIVERSAL_STRING 28
#define ASN1_CHARACTER_STRING 29
#define ASN1_BMP_STRING 30

int asn1parse( const unsigned char *buffer, 
               int length, 
               struct asn1struct *top_level_token );
void asn1free( struct asn1struct *node );
int pem_decode( unsigned char *pem_buffer, unsigned char *der_buffer );

#endif
