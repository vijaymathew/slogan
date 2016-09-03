#ifndef SLOGAN_H_
#define SLOGAN_H_

#include "gambit.h" 

#define SLOGAN_LIBFFI_ARGC 32
#define SLOGAN_LIBFFI_STRUCT_DEFS 32
#define SLOGAN_LIBFFI_STRUCT_SIZE 1024

#define ___slogan_obj ___SCMOBJ
#define ___int ___INT
extern ___slogan_obj ___fix(int);

#define ___SLOGAN_OBJ_to_CHAR ___SCMOBJ_to_CHAR
#define ___slogan_obj_to_char(s, i) ((___SCMOBJ_to_CHAR (s, i, 0)))

#define ___SLOGAN_OBJ_to_UCHAR ___SCMOBJ_to_UCHAR
#define ___slogan_obj_to_uchar(s, i) ((___SCMOBJ_to_UCHAR (s, i, 0)))

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

#define ___SLOGAN_OBJ_to_POINTER ___SCMOBJ_to_POINTER
#define ___slogan_obj_to_void_pointer(obj, p) ___SCMOBJ_to_POINTER(obj, p, ___FAL, 0)

#define ___CHARSTRING_to_SLOGAN_OBJ ___CHARSTRING_to_SCMOBJ
#define ___charstring_to_slogan_obj(c, s) ((___CHARSTRING_to_SCMOBJ (___PSTATE, c, s, 0)));___EXT(___release_scmobj)(*s)

#define ___NONNULLCHARSTRING_to_SLOGAN_OBJ ___NONNULLCHARSTRING_to_SCMOBJ
#define ___nonnullcharstring_to_slogan_obj(c, s) ((___NONNULLCHARSTRING_to_SCMOBJ (___PSTATE, c, s, 0)));___EXT(___release_scmobj)(*s)

#define ___CHAR_to_SLOGAN_OBJ ___CHAR_to_SCMOBJ
#define ___char_to_slogan_obj(c, s) ((___CHAR_to_SCMOBJ (___PSTATE, c, s, 0)));___EXT(___release_scmobj)(*s)

#define ___UCHAR_to_SLOGAN_OBJ ___UCHAR_to_SCMOBJ
#define ___uchar_to_slogan_obj(c, s) ((___UCHAR_to_SCMOBJ (___PSTATE, c, s, 0)));___EXT(___release_scmobj)(*s)

#define ___UINT_to_SLOGAN_OBJ ___UINT_to_SCMOBJ
#define ___uint_to_slogan_obj(c, s) ((___UINT_to_SCMOBJ (___PSTATE, c, s, 0)));___EXT(___release_scmobj)(*s)

#define ___ULONG_to_SLOGAN_OBJ ___ULONG_to_SCMOBJ
#define ___ulong_to_slogan_obj(c, s) ((___ULONG_to_SCMOBJ (___PSTATE, c, s, 0)));___EXT(___release_scmobj)(*s)

#define ___LONG_to_SLOGAN_OBJ ___LONG_to_SCMOBJ
#define ___long_to_slogan_obj(c, s) ((___LONG_to_SCMOBJ (___PSTATE, c, s, 0)));___EXT(___release_scmobj)(*s)

#define ___LONGLONG_to_SLOGAN_OBJ ___LONGLONG_to_SCMOBJ
#define ___longlong_to_slogan_obj(c, s) ((___LONGLONG_to_SCMOBJ (___PSTATE, c, s, 0)));___EXT(___release_scmobj)(*s)

#define ___ULONGLONG_to_SLOGAN_OBJ ___ULONGLONG_to_SCMOBJ
#define ___ulonglong_to_slogan_obj(c, s) ((___ULONGLONG_to_SCMOBJ (___PSTATE, c, s, 0)));___EXT(___release_scmobj)(*s)

#define ___FLOAT_to_SLOGAN_OBJ ___FLOAT_to_SCMOBJ
#define ___float_to_slogan_obj(c, s) ((___FLOAT_to_SCMOBJ (___PSTATE, c, s, 0)));___EXT(___release_scmobj)(*s)

#define ___DOUBLE_to_SLOGAN_OBJ ___DOUBLE_to_SCMOBJ
#define ___double_to_slogan_obj(c, s) ((___DOUBLE_to_SCMOBJ (___PSTATE, c, s, 0)));___EXT(___release_scmobj)(*s)

#define ___POINTER_to_SLOGAN_OBJ ___POINTER_to_SCMOBJ
#define ___void_pointer_to_slogan_obj(c, s) ((___POINTER_to_SCMOBJ (___PSTATE, c, ___FAL, NULL, s, 0)));___EXT(___release_scmobj)(*s)

#define ___PAIR ___make_pair
extern ___slogan_obj ___pair(___slogan_obj, ___slogan_obj);
#define ___head ___CAR
#define ___tail ___CDR
#define ___is_empty ___NULLP

#define ___body ___BODY

#define ___array_length(x) ___int( ___VECTORLENGTH(x))
#define ___array_at ___VECTORREF 
#define ___array_set ___VECTORSET
#define ___array_shrink ___VECTORSHRINK 

#define ___s8array_length(x) ___int(___S8VECTORLENGTH(x))
#define ___s8array_at ___S8VECTORREF 
#define ___s8array_set ___S8VECTORSET 
#define ___s8array_shrink ___S8VECTORSHRINK 

#define ___u8array_length(x) ___int(___U8VECTORLENGTH(x))
#define ___u8array_at ___U8VECTORREF 
#define ___u8array_set ___U8VECTORSET
#define ___u8array_shrink ___U8VECTORSHRINK

#define ___s16array_length(x) ___int(___S16VECTORLENGTH(x))
#define ___s16array_at ___S16VECTORREF
#define ___s16array_set ___S16VECTORSET
#define ___s16array_shrink ___S16VECTORSHRINK

#define ___u16array_length(x) ___int(___U16VECTORLENGTH(x))
#define ___u16array_at ___U16VECTORREF
#define ___u16array_set ___U16VECTORSET
#define ___u16array_shrink ___U16VECTORSHRINK

#define ___s32array_length(x) ___int(___S32VECTORLENGTH(x))
#define ___s32array_at ___S32VECTORREF
#define ___s32array_set ___S32VECTORSET
#define ___s32array_shrink ___S32VECTORSHRINK

#define ___u32array_length(x) ___int(___U32VECTORLENGTH(x))
#define ___u32array_at ___U32VECTORREF
#define ___u32array_set ___U32VECTORSET
#define ___u32array_shrink ___U32VECTORSHRINK

#define ___s64array_length(x) ___int(___S64VECTORLENGTH(x))
#define ___s64array_at ___S64VECTORREF
#define ___s64array_set ___S64VECTORSET
#define ___s64array_shrink ___S64VECTORSHRINK

#define ___u64array_length(x) ___int(___U64VECTORLENGTH(x))
#define ___u64array_at ___U64VECTORREF
#define ___u64array_set ___U64VECTORSET
#define ___u64array_shrink ___U64VECTORSHRINK

#define ___f32array_length(x) ___int(___F32VECTORLENGTH(x))
#define ___f32array_at ___F32VECTORREF
#define ___f32array_set ___F32VECTORSET
#define ___f32array_shrink ___F32VECTORSHRINK

#define ___f64array_length(x) ___int(___F64VECTORLENGTH(x))
#define ___f64array_at ___F64VECTORREF
#define ___f64array_set ___F64VECTORSET
#define ___f64array_shrink ___F64VECTORSHRINK

#define ___string_length(x) ___int(___STRINGLENGTH(x))
#define ___string_at ___STRINGREF
#define ___string_set ___STRINGSET
#define ___string_shrink ___STRINGSHRINK

#define ___release_slogan_obj ___release_scmobj

extern  void ___setup_fn_call();
extern ___SCMOBJ ___call_fn(___SCMOBJ, ___SCMOBJ);

/* Allocates space for arrays whose pointer is not relocated by the GC. */
extern ___slogan_obj ___alloc_u8array(size_t size);
extern ___slogan_obj ___alloc_array(size_t size);

extern int _peek_refcount(___SCMOBJ s); /* A debug tool to check the reference count of an object. */

#endif
