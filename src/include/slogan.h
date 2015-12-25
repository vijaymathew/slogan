#ifndef SLOGAN_H_
#define SLOGAN_H_

#include "gambit.h" 

#define SLOGAN_LIBFFI_ARGC 32
#define SLOGAN_LIBFFI_STRUCT_DEFS 32

#define ___slogan_obj ___SCMOBJ
#define ___int ___INT
#define ___fix ___FIX

#define ___SLOGAN_OBJ_to_INT ___SCMOBJ_to_INT
#define ___slogan_obj_to_int(s, i) ((___SCMOBJ_to_INT (s, i, 0)))

#define ___SLOGAN_OBJ_to_UINT ___SCMOBJ_to_UINT
#define ___slogan_obj_to_uint(s, i) ((___SCMOBJ_to_UINT (s, i, 0)))

#define ___SLOGAN_OBJ_to_LONG ___SCMOBJ_to_LONG
#define ___slogan_obj_to_long(s, i) ((___SCMOBJ_to_LONG (s, i, 0)))

#define ___SLOGAN_OBJ_to_ULONG ___SCMOBJ_to_ULONG
#define ___slogan_obj_to_ulong(s, i) ((___SCMOBJ_to_ULONG (s, i, 0)))

#define ___SLOGAN_OBJ_to_LONGLONG ___SCMOBJ_to_LONGLONG
#define ___slogan_obj_to_longlong(s, i) ((___SCMOBJ_to_LONGLONG (s, i, 0)))

#define ___SLOGAN_OBJ_to_ULONGLONG ___SCMOBJ_to_ULONGLONG
#define ___slogan_obj_to_ulonglong(s, i) ((___SCMOBJ_to_ULONGLONG (s, i, 0)))

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

#define ___UINT_to_SLOGAN_OBJ ___UINT_to_SCMOBJ
#define ___uint_to_slogan_obj(c, s) ((___UINT_to_SCMOBJ (___PSTATE, c, s, 0)))

#define ___ULONG_to_SLOGAN_OBJ ___ULONG_to_SCMOBJ
#define ___ulong_to_slogan_obj(c, s) ((___ULONG_to_SCMOBJ (___PSTATE, c, s, 0)))

#define ___LONG_to_SLOGAN_OBJ ___LONG_to_SCMOBJ
#define ___long_to_slogan_obj(c, s) ((___LONG_to_SCMOBJ (___PSTATE, c, s, 0)))

#define ___LONGLONG_to_SLOGAN_OBJ ___LONGLONG_to_SCMOBJ
#define ___longlong_to_slogan_obj(c, s) ((___LONGLONG_to_SCMOBJ (___PSTATE, c, s, 0)))

#define ___ULONGLONG_to_SLOGAN_OBJ ___ULONGLONG_to_SCMOBJ
#define ___ulonglong_to_slogan_obj(c, s) ((___ULONGLONG_to_SCMOBJ (___PSTATE, c, s, 0)))

#define ___FLOAT_to_SLOGAN_OBJ ___FLOAT_to_SCMOBJ
#define ___float_to_slogan_obj(c, s) ((___FLOAT_to_SCMOBJ (___PSTATE, c, s, 0)))

#define ___DOUBLE_to_SLOGAN_OBJ ___DOUBLE_to_SCMOBJ
#define ___double_to_slogan_obj(c, s) ((___DOUBLE_to_SCMOBJ (___PSTATE, c, s, 0)))

#define ___SLOGAN_OBJ_to_POINTER ___SCMOBJ_to_POINTER
#define ___slogan_obj_to_void_pointer(obj, p) ___SCMOBJ_to_POINTER(obj, p, ___FAL, 0);

#define ___POINTER_to_SLOGAN_OBJ ___POINTER_to_SCMOBJ
#define ___void_pointer_to_slogan_obj(c, s) ((___POINTER_to_SCMOBJ (___PSTATE, c, ___FAL, NULL, s, 0)))

#define ___PAIR ___make_pair
#define ___pair(a, b) ((___make_pair (___PSTATE, a, b)))
#define ___head ___CAR
#define ___tail ___CDR
#define ___is_empty ___NULLP

#define ___body ___BODY

#define ___release_slogan_obj ___release_scmobj

extern  void ___setup_fn_call();

/* Allocates space for arrays whose pointer is not relocated by the GC. */
extern ___slogan_obj ___alloc_u8array(size_t size);
extern ___slogan_obj ___alloc_array(size_t size);

#endif
