/**
 * C Crypto functions derived from "Implementing SSL / TLS Using Cryptography and PKI" by Joshua Davies.
 * (http://as.wiley.com/WileyCDA/WileyTitle/productCd-0470920416.html)
 **/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <arpa/inet.h>
#include "slogan.h"
#include "sha.h"
#include "digest.h"
#include "huge.h"
#include "ecdsa.h"

void ecdsa_sign(elliptic_curve *params, 
                huge *private_key,
                unsigned int *hash, 
                int hash_len, 
                dsa_signature *signature)
{
  unsigned char K[] = {
    0x9E, 0x56, 0xF5, 0x09, 0x19, 0x67, 0x84, 0xD9, 0x63, 0xD1, 0xC0, 
    0xA4, 0x01, 0x51, 0x0E, 0xE7, 0xAD, 0xA3, 0xDC, 0xC5, 0xDE, 0xE0, 
    0x4B, 0x15, 0x4B, 0xF6, 0x1A, 0xF1, 0xD5, 0xA6, 0xDE, 0xCE
  };
  huge k;
  point X;
  huge z;

  init_huge(&k);
  init_huge(&X.x);
  init_huge(&X.y);
  init_huge(&z);
  
  // This should be a random number between 0 and n-1
  load_huge(&k, (unsigned char *) K, sizeof(K));

  set_huge(&X.x, 0);
  set_huge(&X.y, 0);
  copy_huge(&X.x, &params->G.x);
  copy_huge(&X.y, &params->G.y);

  multiply_point(&X, &k, &params->a, &params->p);

  set_huge(&signature->r, 0);
  copy_huge(&signature->r, &X.x);
  divide(&signature->r, &params->n, NULL); // r = x1 % n

  // z is the L_n leftmost bits of hash - cannot be longer than n
  load_huge(&z, (unsigned char *) hash,
            ((hash_len * 4) < params->n.size) ? (hash_len * 4) : params->n.size);

  // s = k^-1 (z + r d_a) % n
  inv(&k, &params->n);
  set_huge(&signature->s, 0);
  copy_huge(&signature->s, private_key);
  multiply(&signature->s, &signature->r);
  add(&signature->s, &z);
  multiply(&signature->s, &k);
  divide(&signature->s, &params->n, NULL);

  release_huge(&k);
  release_huge(&z);
  release_huge(&X.x);
  release_huge(&X.y);
}

int ecdsa_verify(elliptic_curve *params,
                 point *public_key,
                 unsigned int *hash,
                 int hash_len,
                 dsa_signature *signature)
{
  huge z;
  huge w;
  point G;
  point Q;
  int match;

  init_huge(&z);
  init_huge(&w);
  init_huge(&G.x);
  init_huge(&G.y);
  init_huge(&Q.x);
  init_huge(&Q.y);

  // w = s^-1 % n
  set_huge(&w, 0);
  copy_huge(&w, &signature->s);
  inv(&w, &params->n);

  // z is the L_n leftmost bits of hash - cannot be longer than n
  load_huge(&z, (unsigned char *) hash, 
            ((hash_len * 4) < params->n.size) ? (hash_len * 4) : params->n.size);

  // u1 = zw % n
  multiply(&z, &w);
  divide(&z, &params->n, NULL);  // u1 = z

  // u2 = (rw) % q
  multiply(&w, &signature->r);
  divide(&w, &params->n, NULL); // u2 = w

  // (x1,y1) = u1 * G + u2 * Q
  set_huge(&G.x, 0);
  set_huge(&G.y, 0);
  set_huge(&Q.x, 0);
  set_huge(&Q.y, 0);
  copy_huge(&G.x, &params->G.x);
  copy_huge(&G.y, &params->G.y);
  copy_huge(&Q.x, &public_key->x);
  copy_huge(&Q.y, &public_key->y); 

  multiply_point(&G, &z, &params->a, &params->p);
  multiply_point(&Q, &w, &params->a, &params->p);
  add_points(&G, &Q, &params->p);
 
  // r = x1 % n
  divide(&G.x, &params->n, NULL);

  match = !compare(&G.x, &signature->r);

  release_huge(&z);
  release_huge(&w);
  release_huge(&G.x);
  release_huge(&G.y);
  release_huge(&Q.x);
  release_huge(&Q.y);

  return match;
}

___slogan_obj crypto_ecdsa_sign(___slogan_obj args,
                                ___slogan_obj u8arr_msg,
                                ___slogan_obj imsg_len)
{
  ___slogan_obj h;
  ___slogan_obj P;
  ___slogan_obj b;
  ___slogan_obj q;
  ___slogan_obj gx;
  ___slogan_obj gy;
  ___slogan_obj w;
  int mlen;
  int hlen;
  int Plen;
  int blen;
  int qlen;
  int gxlen;
  int gylen;
  int wlen;

  elliptic_curve curve;
  ecc_key A;
  dsa_signature signature;

  digest_ctx ctx;
  ___slogan_obj rr;
  ___slogan_obj rs;
  ___slogan_obj result;

  h = ___head(args);
  P = ___head(h);
  ___slogan_obj_to_int(___tail(h), &Plen);
  args = ___tail(args);

  h = ___head(args);
  b = ___head(h);
  ___slogan_obj_to_int(___tail(h), &blen);
  args = ___tail(args);

  h = ___head(args);
  q = ___head(h);
  ___slogan_obj_to_int(___tail(h), &qlen);
  args = ___tail(args);

  h = ___head(args);
  gx = ___head(h);
  ___slogan_obj_to_int(___tail(h), &gxlen);
  args = ___tail(args);

  h = ___head(args);
  gy = ___head(h);
  ___slogan_obj_to_int(___tail(h), &gylen);
  args = ___tail(args);

  h = ___head(args);
  w = ___head(h);
  ___slogan_obj_to_int(___tail(h), &wlen);

  ___slogan_obj_to_int(imsg_len, &mlen);

  init_huge(&curve.p);
  init_huge(&curve.a);
  init_huge(&curve.b);
  init_huge(&curve.G.x);
  init_huge(&curve.G.y);
  init_huge(&curve.n);
  init_huge(&curve.h);
  init_huge(&A.d);
  init_huge(&A.Q.x);
  init_huge(&A.Q.y);
  init_huge(&signature.r);
  init_huge(&signature.s);

  load_huge(&curve.p, (unsigned char *) ___BODY(P), Plen);
  set_huge(&curve.a, 3);
  curve.a.sign = 1;
  load_huge(&curve.b, (unsigned char *)___BODY(b), blen);
  load_huge(&curve.G.x, (unsigned char *)___BODY(gx), gxlen);
  load_huge(&curve.G.y, (unsigned char *)___BODY(gy), gylen);
  load_huge(&curve.n, (unsigned char *)___BODY(q), qlen);

  load_huge(&A.d, (unsigned char *)___BODY(w), wlen);
  set_huge(&A.Q.x, 0);
  set_huge(&A.Q.y, 0);
  copy_huge(&A.Q.x, &curve.G.x);
  copy_huge(&A.Q.y, &curve.G.y);
  multiply_point(&A.Q, &A.d, &curve.a, &curve.p);

  init_sha256_digest(&ctx);
  update_digest(&ctx, (unsigned char *)___BODY(u8arr_msg), mlen);
  finalize_digest(&ctx);

  ecdsa_sign(&curve, &A.d, ctx.hash, ctx.hash_len, &signature);
  rr = ___alloc_u8array(signature.r.size);
  memcpy(___BODY(rr), signature.r.rep, signature.r.size);
  rs = ___alloc_u8array(signature.s.size);
  memcpy(___BODY(rs), signature.s.rep, signature.s.size);
  result = ___pair(rr, rs);
  
  release_sha256_digest(&ctx);
  release_huge(&curve.p);
  release_huge(&curve.a);
  release_huge(&curve.b);
  release_huge(&curve.G.x);
  release_huge(&curve.G.y);
  release_huge(&curve.n);
  release_huge(&curve.h);
  release_huge(&A.d);
  release_huge(&A.Q.x);
  release_huge(&A.Q.y);
  release_huge(&signature.r);
  release_huge(&signature.s);
  
  return result;
}

___slogan_obj crypto_ecdsa_verify(___slogan_obj args,
                                  ___slogan_obj u8arr_rr,
                                  ___slogan_obj irr_len,
                                  ___slogan_obj u8arr_rs,
                                  ___slogan_obj irs_len,
                                  ___slogan_obj u8arr_msg,
                                  ___slogan_obj imsg_len)
{
  ___slogan_obj h;
  ___slogan_obj P;
  ___slogan_obj b;
  ___slogan_obj q;
  ___slogan_obj gx;
  ___slogan_obj gy;
  ___slogan_obj w;
  int mlen;
  int hlen;
  int Plen;
  int blen;
  int qlen;
  int gxlen;
  int gylen;
  int wlen;
  int rrlen;
  int rslen;

  elliptic_curve curve;
  ecc_key A;
  dsa_signature signature;

  digest_ctx ctx;
  ___slogan_obj result;

  h = ___head(args);
  P = ___head(h);
  ___slogan_obj_to_int(___tail(h), &Plen);
  args = ___tail(args);

  h = ___head(args);
  b = ___head(h);
  ___slogan_obj_to_int(___tail(h), &blen);
  args = ___tail(args);

  h = ___head(args);
  q = ___head(h);
  ___slogan_obj_to_int(___tail(h), &qlen);
  args = ___tail(args);

  h = ___head(args);
  gx = ___head(h);
  ___slogan_obj_to_int(___tail(h), &gxlen);
  args = ___tail(args);

  h = ___head(args);
  gy = ___head(h);
  ___slogan_obj_to_int(___tail(h), &gylen);
  args = ___tail(args);

  h = ___head(args);
  w = ___head(h);
  ___slogan_obj_to_int(___tail(h), &wlen);

  ___slogan_obj_to_int(imsg_len, &mlen);
  ___slogan_obj_to_int(irr_len, &rrlen);
  ___slogan_obj_to_int(irs_len, &rslen);

  init_huge(&curve.p);
  init_huge(&curve.a);
  init_huge(&curve.b);
  init_huge(&curve.G.x);
  init_huge(&curve.G.y);
  init_huge(&curve.n);
  init_huge(&curve.h);
  init_huge(&A.d);
  init_huge(&A.Q.x);
  init_huge(&A.Q.y);
  init_huge(&signature.r);
  init_huge(&signature.s);

  load_huge(&curve.p, (unsigned char *) ___BODY(P), Plen);
  set_huge(&curve.a, 3);
  curve.a.sign = 1;
  load_huge(&curve.b, (unsigned char *)___BODY(b), blen);
  load_huge(&curve.G.x, (unsigned char *)___BODY(gx), gxlen);
  load_huge(&curve.G.y, (unsigned char *)___BODY(gy), gylen);
  load_huge(&curve.n, (unsigned char *)___BODY(q), qlen);

  load_huge(&A.d, (unsigned char *)___BODY(w), wlen);
  set_huge(&A.Q.x, 0);
  set_huge(&A.Q.y, 0);
  copy_huge(&A.Q.x, &curve.G.x);
  copy_huge(&A.Q.y, &curve.G.y);
  multiply_point(&A.Q, &A.d, &curve.a, &curve.p);

  init_sha256_digest(&ctx);
  update_digest(&ctx, (unsigned char *)___BODY(u8arr_msg), mlen);
  finalize_digest(&ctx);

  load_huge(&signature.r, (unsigned char *)___BODY(u8arr_rr), rrlen);
  load_huge(&signature.s, (unsigned char *)___BODY(u8arr_rs), rslen);

  result = ecdsa_verify(&curve, &A.Q, ctx.hash, ctx.hash_len, &signature) ? ___TRU : ___FAL;

  release_sha256_digest(&ctx);
  release_huge(&curve.p);
  release_huge(&curve.a);
  release_huge(&curve.b);
  release_huge(&curve.G.x);
  release_huge(&curve.G.y);
  release_huge(&curve.n);
  release_huge(&curve.h);
  release_huge(&A.d);
  release_huge(&A.Q.x);
  release_huge(&A.Q.y);
  release_huge(&signature.r);
  release_huge(&signature.s);
  
  return result;
}
