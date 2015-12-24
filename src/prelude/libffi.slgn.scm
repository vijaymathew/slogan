;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.
;; Linkage to libffi to provide a higher level of abstraction to C library calls.

(c-declare #<<c-declare-end

#include <ffi.h>
#include "../include/slogan.h"

 enum libffi_type {
  libffi_type_uint8,
  libffi_type_sint8,
  libffi_type_uint16,
  libffi_type_sint16,
  libffi_type_uint32,
  libffi_type_sint32,
  libffi_type_uchar,
  libffi_type_schar,
  libffi_type_ushort,
  libffi_type_sshort,
  libffi_type_uint,
  libffi_type_sint,
  libffi_type_ulong,
  libffi_type_slong,
  libffi_type_uint64,
  libffi_type_sint64,  
  libffi_type_float,
  libffi_type_double,  
  libffi_type_longdouble,
  libffi_type_charstr,
  libffi_type_pointer,
  libffi_type_void
};

 ffi_type *FFI_TYPE_MAP[22] = {
   &ffi_type_uint8,
     &ffi_type_sint8,
     &ffi_type_uint16,
     &ffi_type_sint16,
     &ffi_type_uint32,
     &ffi_type_sint32,
     &ffi_type_uchar,
     &ffi_type_schar,
     &ffi_type_ushort,
     &ffi_type_sshort,
     &ffi_type_uint,
     &ffi_type_sint,
     &ffi_type_ulong,
     &ffi_type_slong,
     &ffi_type_uint64,
     &ffi_type_sint64,  
     &ffi_type_float,
     &ffi_type_double,  
     &ffi_type_longdouble,
     &ffi_type_pointer,
     &ffi_type_pointer,
     &ffi_type_void
     };

 struct fncall_param
 {
   int argc;
   ffi_type *args[LIBFFI_MAX_ARGC];
   void *values[LIBFFI_MAX_ARGC];
   int iargs[LIBFFI_MAX_ARGC];
   long largs[LIBFFI_MAX_ARGC];
   long long llargs[LIBFFI_MAX_ARGC];
   float fargs[LIBFFI_MAX_ARGC];
   double dargs[LIBFFI_MAX_ARGC];
   long double ldargs[LIBFFI_MAX_ARGC];
   char *sargs[LIBFFI_MAX_ARGC];
   void *pargs[LIBFFI_MAX_ARGC];
   int iargs_count;
   int llargs_count;
   int largs_count;
   int fargs_count;
   int ldargs_count;
   int dargs_count;
   int sargs_count;
   int pargs_count;
 };

 static void fpinit(struct fncall_param *fp)
 {
   fp->argc = fp->iargs_count = fp->llargs_count = 0;
   fp->largs_count = fp->fargs_count = fp->ldargs_count = 0;
   fp->dargs_count = fp->sargs_count = fp->pargs_count = 0;
 }

 ___SCMOBJ libffi_fncall(void *fn,
                         ___SCMOBJ arg_types_vals,
                         ___SCMOBJ i_argc,
                         ___SCMOBJ i_ret_type)
 {
   ffi_cif cif;
   struct fncall_param fp;
   int ret_type;
   int i;

   ___SCMOBJ retval = ___TRU;

   fpinit(&fp);
   ___slogan_obj_to_int(i_argc, &fp.argc);

   for (i = 0; i < fp.argc; ++i)
     {
       ffi_type t;
       int it;
       ___SCMOBJ arg = ___CAR(arg_types_vals);
   
       ___slogan_obj_to_int(___CAR(arg), &it);
       fp.args[i] = FFI_TYPE_MAP[it];
       if (fp.args[i] == &ffi_type_uint8
           || fp.args[i] == &ffi_type_sint8
           || fp.args[i] == &ffi_type_uint16
           || fp.args[i] == &ffi_type_sint16
           || fp.args[i] == &ffi_type_uint32
           || fp.args[i] == &ffi_type_sint32
           || fp.args[i] == &ffi_type_uchar
           || fp.args[i] == &ffi_type_schar
           || fp.args[i] == &ffi_type_ushort
           || fp.args[i] == &ffi_type_sshort
           || fp.args[i] == &ffi_type_uint
           || fp.args[i] == &ffi_type_sint)
         {
           ___slogan_obj_to_int(___CDR(arg), &fp.iargs[fp.iargs_count]);
           fp.values[i] = &fp.iargs[fp.iargs_count++];
         }
       else if (fp.args[i] == &ffi_type_ulong || fp.args[i] == &ffi_type_slong)
         {
           long r;
           ___slogan_obj_to_long(___CDR(arg), &r);
           fp.largs[fp.largs_count] = r;
           fp.values[i] = &fp.largs[fp.largs_count++];
         }
       else if (fp.args[i] == &ffi_type_uint64 || fp.args[i] == &ffi_type_sint64)
         {
           ___slogan_obj_to_longlong(___CDR(arg), &fp.llargs[fp.llargs_count]);
           fp.values[i] = &fp.llargs[fp.llargs_count++];
         }
       else if (fp.args[i] == &ffi_type_float)
         {
           ___slogan_obj_to_float(___CDR(arg), &fp.fargs[fp.fargs_count]);
           fp.values[i] = &fp.fargs[fp.fargs_count++];
         }
       else if (fp.args[i] == &ffi_type_double)
         {
           ___slogan_obj_to_double(___CDR(arg), &fp.dargs[fp.dargs_count]);
           fp.values[i] = &fp.dargs[fp.dargs_count++];
         }
       else if (fp.args[i] == &ffi_type_longdouble)
         {
           double d;
           ___slogan_obj_to_double(___CDR(arg), &d);
           fp.ldargs[fp.ldargs_count] = (long double)d;
           fp.values[i] = &fp.ldargs[fp.ldargs_count++];
         }
       else
         {
           int t = 0;
           ___slogan_obj_to_int(___CAR(arg), &t);
           if (t == libffi_type_charstr)
             {
               ___slogan_obj_to_charstring(___CDR(arg), &fp.sargs[fp.sargs_count]);
               fp.values[i] = &fp.sargs[fp.sargs_count++];
             }
           else
             {
               ___slogan_obj_to_void_pointer(___CDR(arg), &fp.pargs[fp.pargs_count]);
               fp.values[i] = &fp.pargs[fp.pargs_count++];
             }
           break;
         }
       arg_types_vals = ___CDR(arg_types_vals);
     }

   ___slogan_obj_to_int(i_ret_type, &ret_type);
   if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, fp.argc,
                    FFI_TYPE_MAP[ret_type], fp.args) == FFI_OK)
     {
       if (ret_type <= libffi_type_sint)
         {
           int rc;
           ffi_call(&cif, fn, &rc, fp.values);
           retval =  ___fix(rc);
         }
       else if (ret_type <= libffi_type_slong)
         {
           long rc;
           ffi_call(&cif, fn, &rc, fp.values);
           retval = ___fix(rc);
         }
       else if (ret_type <=  libffi_type_sint64)
         {
           long long rc;
           ffi_call(&cif, fn, &rc, fp.values);
           retval = ___fix(rc);
         }
       else if (ret_type == libffi_type_float)
         {
           float rc;
           ffi_call(&cif, fn, &rc, fp.values);
           ___float_to_slogan_obj(rc, &retval);
         }
       else if (ret_type == libffi_type_double)
         {
           double rc;
           ffi_call(&cif, fn, &rc, fp.values);
           ___double_to_slogan_obj(rc, &retval);
         }
       else if (ret_type == libffi_type_longdouble)
         {
           long double rc;
           ffi_call(&cif, fn, &rc, fp.values);
           ___double_to_slogan_obj(rc, &retval);
         }
       else if (ret_type == libffi_type_charstr)
         {
           char *rc;
           ffi_call(&cif, fn, &rc, fp.values);
           ___charstring_to_slogan_obj(rc, &retval);
         }
       else if (ret_type == libffi_type_void)
         {
           ffi_call(&cif, fn, NULL, fp.values);
           retval = ___NUL;
         }
       else
         {
           void *rc;
           ffi_call(&cif, fn, &rc, fp.values);
           ___void_pointer_to_slogan_obj(rc, &retval);
         }
     }
   else retval = ___FAL;
   return retval;
 }

c-declare-end
)

(c-define-type void-pointer (pointer void))

(define libffi-fncall (c-lambda (void-pointer scheme-object scheme-object scheme-object)
                                scheme-object
                                "libffi_fncall"))
;; Sample:
;; define clib = ffi_open("./demo_lib.so"); 
;; define f = ffi_fn(clib, "add");
;; `libffi-fncall`(f, [5:10, 5:20], 2, 5);
;; define f = ffi_fn(clib, "say_hello");
;; `libffi-fncall`(f, [19:"hey there!"], 1, 19);
;;  define f = ffi_fn(clib, "make_point");
;; define p = `libffi-fncall`(f, [5:3 5:12], 2, 20);
;; define f = ffi_fn(clib, "point_x");
;; `libffi-fncall`(f, [20:p], 1, 5);
;; define f = ffi_fn(clib, "point_y");
;; `libffi-fncall`(f, [20:p], 1, 5);
;; define f = ffi_fn(clib, "free_point");
;; `libffi-fncall`(f, [20:p], 1, 21);
