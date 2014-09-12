#ifndef SLOGAN_H_
#define SLOGAN_H_

#include "gambit.h" 

#define ___slogan_obj ___SCMOBJ
#define ___int ___INT
#define ___fix ___FIX

#define ___SLOGAN_OBJ_to_INT ___SCMOBJ_to_INT
#define ___slogan_obj_to_int(s, i) ((___SCMOBJ_to_INT (s, i, 0)))

#define ___SLOGAN_OBJ_to_INT64 ___SCMOBJ_to_INT64
#define ___slogan_obj_to_int64(s, i) ((___SCMOBJ_to_INT64 (s, i, 0)))

#define ___SLOGAN_OBJ_to_UINT ___SCMOBJ_to_UINT
#define ___slogan_obj_to_uint(s, u) ((___SCMOBJ_to_UINT (s, u, 0)))

#define ___SLOGAN_OBJ_to_FLOAT ___SCMOBJ_to_FLOAT
#define ___slogan_obj_to_float(s, f) ((___SCMOBJ_to_FLOAT (s, f, 0)))

#define ___SLOGAN_OBJ_to_DOUBLE ___SCMOBJ_to_DOUBLE
#define ___slogan_obj_to_double(s, d) ((___SCMOBJ_to_DOUBLE (s, d, 0)))

#define ___SLOGAN_OBJ_to_CHARSTRING ___SCMOBJ_to_CHARSTRING
#define ___slogan_obj_to_charstring(s, c) ((___SCMOBJ_to_CHARSTRING (s, c, 0)))

#define ___SLOGAN_OBJ_to_NONNULL_CHARSTRING ___SCMOBJ_to_NONNULLCHARSTRING
#define ___slogan_obj_to_nonnull_charstring(s, c) ((___SCMOBJ_to_NONNULLCHARSTRING (s, c, 0)))

#define ___CHARSTRING_to_SLOGAN_OBJ ___CHARSTRING_to_SCMOBJ
#define ___charstring_to_slogan_obj(c, s) ((___CHARSTRING_to_SCMOBJ (___PSTATE, c, s, 0)))

#define ___NONNULLCHARSTRING_to_SLOGAN_OBJ ___NONNULLCHARSTRING_to_SCMOBJ
#define ___nonnullcharstring_to_slogan_obj(c, s) ((___NONNULLCHARSTRING_to_SCMOBJ (___PSTATE, c, s, 0)))

#define ___SLOGAN_OBJ_to_POINTER ___SCMOBJ_to_POINTER
#define ___POINTER_to_SLOGAN_OBJ ___POINTER_to_SCMOBJ

#define ___PAIR ___make_pair
#define ___pair(a, b) ((___make_pair (___PSTATE, a, b)))
#define ___head ___CAR
#define ___tail ___CDR
#define ___is_empty ___NULLP

#define ___body ___BODY

#define ___release_slogan_obj ___release_scmobj

#endif
