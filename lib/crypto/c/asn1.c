/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <assert.h>
#include "base64.h"
#include "asn1.h"
#include "slogan.h"

int asn1parse( const unsigned char *buffer, 
               int length, 
               struct asn1struct *top_level_token )
{  
  unsigned int tag;
  unsigned char tag_length_byte;
  unsigned long tag_length;
  const unsigned char *ptr;
  const unsigned char *ptr_begin;
  struct asn1struct *token;

  ptr = buffer;
  token = top_level_token;

  while ( length )
  {
   ptr_begin = ptr;
   tag = *ptr;
   ptr++;
   length--;

   // High tag # form (bits 5-1 all == "1"), to encode tags > 31. Not used
   // in X.509
   if ( ( tag & 0x1F ) == 0x1F )
   {
     tag = 0;
     while ( *ptr & 0x80 )
     {
       tag <<= 8;
       tag |= *ptr & 0x7F;
     }
    }

    tag_length_byte = *ptr;
    ptr++;
    length--;

    // TODO this doesn't handle indefinite-length encodings (according to
    // ITU-T X.690, this never occurs in DER, only in BER, which X.509 doesn't
    // use)
    if ( tag_length_byte & 0x80 )
    {
      const unsigned char *len_ptr = ptr;
      tag_length = 0;
      while ( ( len_ptr - ptr ) < ( tag_length_byte & 0x7F ) )
      {
        tag_length <<= 8;
        tag_length |= *(len_ptr++);
        length--;
      }
      ptr = len_ptr;
    }
    else
    {
      tag_length = tag_length_byte;
    }
  
    // TODO deal with "high tag numbers"
    token->constructed = tag & 0x20;
    token->tag_class = ( tag & 0xC0 ) >> 6;
    token->tag = tag & 0x1F;
    token->length = tag_length;
    token->data = ptr;
    token->children = NULL;
    token->next = NULL;

    if ( tag & 0x20 )
    {
      token->length = tag_length + ( ptr - ptr_begin );

      token->data = ptr_begin;

      // Append a child to this tag and recurse into it
      token->children = ( struct asn1struct * ) 
        malloc( sizeof( struct asn1struct ) );
      asn1parse( ptr, tag_length, token->children );
    }

    ptr += tag_length;
    length -= tag_length;

    // At this point, we're pointed at the tag for the next token in the buffer.
    if ( length )
    {
      token->next = ( struct asn1struct * ) malloc( sizeof( struct asn1struct ) ); 
      token = token->next;
    }
  }
  
  return 0;
}

/**
 * Recurse through the given node and free all of the memory that was allocated
 * by asn1parse. Don't free the "data" pointers, since that points to memory that
 * was not allocated by asn1parse.
 */
void asn1free( struct asn1struct *node )
{
  if ( !node )
  {
    return;
  }

  asn1free( node->children );
  free( node->children );
  asn1free( node->next );
  free( node->next );
}

static char *tag_names[] = { 
 "BER",          // 0
 "BOOLEAN",        // 1
 "INTEGER",        // 2
 "BIT STRING",       // 3
 "OCTET STRING",      // 4
 "NULL",          // 5
 "OBJECT IDENTIFIER",   // 6
 "ObjectDescriptor",    // 7
 "INSTANCE OF, EXTERNAL", // 8
 "REAL",          // 9
 "ENUMERATED",       // 10
 "EMBEDDED PPV",      // 11
 "UTF8String",       // 12
 "RELATIVE-OID",      // 13
 "undefined(14)",     // 14
 "undefined(15)",     // 15
 "SEQUENCE, SEQUENCE OF", // 16
 "SET, SET OF",      // 17
 "NumericString",     // 18
 "PrintableString",    // 19
 "TeletexString, T61String", // 20
 "VideotexString",     // 21
 "IA5String",       // 22
 "UTCTime",        // 23
 "GeneralizedTime",    // 24
 "GraphicString",     // 25
 "VisibleString, ISO64String", // 26
 "GeneralString",     // 27
 "UniversalString",    // 28
 "CHARACTER STRING",    // 29
 "BMPString"        // 30
};

void asn1show( int depth, struct asn1struct *certificate )
{
  struct asn1struct *token;
  int i; 
 
  token = certificate;
 
  while ( token )
  {
    for ( i = 0; i < depth; i++ )
    {
      printf( " " );
    }
    switch ( token->tag_class )
    {
      case ASN1_CLASS_UNIVERSAL:
        printf( "%s", tag_names[ token->tag ] );
        break;
      case ASN1_CLASS_APPLICATION:
        printf( "application" );
        break;
      case ASN1_CONTEXT_SPECIFIC:
        printf( "context" );
        break;
      case ASN1_PRIVATE:
        printf( "private" );
        break;
    }
    printf( " (%d:%d) ", token->tag, token->length );
  
    if ( token->tag_class == ASN1_CLASS_UNIVERSAL )
    {
      switch ( token->tag )
      {
        case ASN1_INTEGER:
          break;
        case ASN1_BIT_STRING:
        case ASN1_OCTET_STRING:
        case ASN1_OBJECT_IDENTIFIER:
          {
            int i;

            for ( i = 0; i < token->length; i++ )
            {
              printf( "%.02x ", token->data[ i ] );
            }
          }
          break;
      case ASN1_NUMERIC_STRING:
      case ASN1_PRINTABLE_STRING:
      case ASN1_TELETEX_STRING:
      case ASN1_VIDEOTEX_STRING:
      case ASN1_IA5_STRING:
      case ASN1_UTC_TIME:
      case ASN1_GENERALIZED_TIME:
      case ASN1_GRAPHIC_STRING:
      case ASN1_VISIBLE_STRING:
      case ASN1_GENERAL_STRING:
      case ASN1_UNIVERSAL_STRING:
      case ASN1_CHARACTER_STRING:
      case ASN1_BMP_STRING:
      case ASN1_UTF8_STRING:
      {
        char *str_val = ( char * ) malloc( token->length + 1 );
        strncpy( str_val, ( char * ) token->data, token->length );
         str_val[ token->length ] = 0;
        printf( " %s", str_val ); 
        free( str_val );
      }
        break;
      default:
        break;
    }
  }
  
  printf( "\n" );
  if ( token->children )
  {
    asn1show( depth + 1, token->children );
  }
  token = token->next;
 } 
} 

int pem_decode( unsigned char *pem_buffer, unsigned char *der_buffer )
{
  unsigned char *pem_buffer_end, *pem_buffer_begin;
  unsigned char *bufptr = der_buffer;
  int buffer_size;
  // Skip first line, which is always "-----BEGIN CERTIFICATE-----".

  if ( strncmp( pem_buffer, "-----BEGIN", 10 ) )
  {
    fprintf( stderr, 
       "This does not appear to be a PEM-encoded certificate file\n" );
    exit( 0 );
  }

  pem_buffer_begin = pem_buffer;
  pem_buffer= pem_buffer_end = strchr( pem_buffer, '\n' ) + 1;

  while ( strncmp( pem_buffer, "-----END", 8 ) )
  {
    // Find end of line
    pem_buffer_end = strchr( pem_buffer, '\n' );
    // Decode one line out of pem_buffer into buffer
    bufptr += base64_decode( pem_buffer, 
      ( pem_buffer_end - pem_buffer ) - 
      ( ( *( pem_buffer_end - 1 ) == '\r' ) ? 1 : 0 ), 
      bufptr );
    pem_buffer = pem_buffer_end + 1;
  } 
 
  buffer_size = bufptr - der_buffer;
 
  return buffer_size;
} 

void *crypto_asn1_parse(___slogan_obj u8arr,
                        ___slogan_obj iarrlen)
{
  int len;
  struct asn1struct *certificate;

  certificate = (struct asn1struct *)malloc(sizeof(struct asn1struct));
  assert(certificate != NULL);
  ___slogan_obj_to_int(iarrlen, &len);
  asn1parse((unsigned char *)___BODY(u8arr), len, certificate);
  return (void *)certificate;
}

void crypto_asn1_cert_free(void *p)
{
  if (p != NULL)
    {
      struct asn1struct *certificate = (struct asn1struct *)p;
      asn1free(certificate);
    }
}

void crypto_pem_decode(___slogan_obj u8arr_pem,
                       ___slogan_obj u8arr_der)
{
  pem_decode((unsigned char *)___BODY(u8arr_pem),
             (unsigned char *)___BODY(u8arr_der));
}

___slogan_obj crypto_asn1_cert_constructed(void *p)
{
  struct asn1struct *certificate = (struct asn1struct *)p;
  return certificate->constructed ? ___TRU : ___FAL;
}

___slogan_obj crypto_asn1_cert_tagclass(void *p)
{
  struct asn1struct *certificate = (struct asn1struct *)p;
  return ___fix(certificate->tag_class);
}

___slogan_obj crypto_asn1_cert_tag(void *p)
{
  struct asn1struct *certificate = (struct asn1struct *)p;
  return ___fix(certificate->tag);
}

___slogan_obj crypto_asn1_cert_length(void *p)
{
  struct asn1struct *certificate = (struct asn1struct *)p;
  return ___fix(certificate->length);
}

void crypto_asn1_cert_data(void *p,  ___slogan_obj u8arr_out)
{
  struct asn1struct *certificate = (struct asn1struct *)p;
  memcpy(___BODY(u8arr_out), certificate->data, certificate->length);
}

void *crypto_asn1_cert_children(void *p)
{
  struct asn1struct *certificate = (struct asn1struct *)p;
  return (void *)certificate->children;
}

void *crypto_asn1_cert_next(void *p)
{
  struct asn1struct *certificate = (struct asn1struct *)p;
  return (void *)certificate->next;
}

