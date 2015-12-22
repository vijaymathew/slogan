;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.
;; Linkage to libffi to provide a higher level of abstraction to C library calls.

(c-declare #<<c-declare-end

#include <ffi.h>
#include <stdlib.h>
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
 
 ___SCMOBJ libffi_fncall(void *fn,
                         ___SCMOBJ arg_types_vals,
                         ___SCMOBJ i_argc,
                         ___SCMOBJ i_ret_type)
 {
   ffi_cif cif;
   ffi_type **args = NULL;
   void **values = NULL;
   int argc;
   int ret_type;
   int i;

   int *iargs = NULL;
   long *largs = NULL;
   long long *llargs = NULL;
   float *fargs = NULL;
   double *dargs = NULL;
   long double *ldargs = NULL;
   char **sargs = NULL;
   void **pargs = NULL;
   int iargs_count = 0;
   int llargs_count = 0;
   int largs_count = 0;
   int fargs_count = 0;
   int ldargs_count = 0;
   int dargs_count = 0;
   int sargs_count = 0;
   int pargs_count = 0;
   
   ___SCMOBJ tmp;
   ___SCMOBJ retval = ___TRU;
   
   ___slogan_obj_to_int(i_argc, &argc);
   
   args = (ffi_type **)malloc(sizeof(ffi_type *) * argc);
   values = (void **)malloc(sizeof(void *) * argc);

   tmp = arg_types_vals;
   for (i = 0; i < argc; ++i)
     {
       int it;
       ___SCMOBJ arg = ___CAR(tmp);
       
       ___slogan_obj_to_int(___CAR(arg), &it);
       if (it <=  libffi_type_sint) ++iargs_count;
       else if (it <= libffi_type_slong) ++largs_count;
       else if (it <=  libffi_type_sint64) ++llargs_count;
       else if (it == libffi_type_float) ++fargs_count;
       else if (it == libffi_type_double) ++dargs_count;
       else if (it == libffi_type_longdouble) ++ldargs_count;
       else if (it == libffi_type_charstr) ++sargs_count;
       else if (it == libffi_type_pointer) ++pargs_count;

       args[i] = FFI_TYPE_MAP[it];
       tmp = ___CDR(tmp);
     }

   if (iargs_count > 0) iargs = (int *)malloc(sizeof(int) * iargs_count);
   if (largs_count > 0) largs = (long *)malloc(sizeof(long) * largs_count);
   if (llargs_count > 0) llargs = (long long *)malloc(sizeof(long long) * llargs_count);
   if (fargs_count > 0) fargs = (float *)malloc(sizeof(float) * fargs_count);
   if (dargs_count > 0) dargs = (double *)malloc(sizeof(double) * dargs_count);
   if (ldargs_count > 0) ldargs = (long double *)malloc(sizeof(long double) * ldargs_count);
   if (pargs_count > 0) pargs = (void **)malloc(sizeof(void *) * pargs_count);
   if (sargs_count > 0) sargs = (char **)malloc(sizeof(char *) * sargs_count);

   iargs_count = largs_count = llargs_count = fargs_count = dargs_count = ldargs_count = pargs_count = sargs_count = 0;

   tmp = arg_types_vals;
   for (i = 0; i < argc; ++i)
     {
       ffi_type t;
       int it;
       ___SCMOBJ arg = ___CAR(tmp);
   
       if (args[i] == &ffi_type_uint8
           || args[i] == &ffi_type_sint8
           || args[i] == &ffi_type_uint16
           || args[i] == &ffi_type_sint16
           || args[i] == &ffi_type_uint32
           || args[i] == &ffi_type_sint32
           || args[i] == &ffi_type_uchar
           || args[i] == &ffi_type_schar
           || args[i] == &ffi_type_ushort
           || args[i] == &ffi_type_sshort
           || args[i] == &ffi_type_uint
           || args[i] == &ffi_type_sint)
         {
           ___slogan_obj_to_int(___CDR(arg), &iargs[iargs_count]);
           values[i] = &iargs[iargs_count++];
         }
       else if (args[i] == &ffi_type_ulong || args[i] == &ffi_type_slong)
         {
           int r;
           ___slogan_obj_to_int(___CDR(arg), &r);
           largs[largs_count] = (long)r;
           values[i] = &largs[largs_count++];
         }
       else if (args[i] == &ffi_type_uint64 || args[i] == &ffi_type_sint64)
         {
           ___slogan_obj_to_int64(___CDR(arg), &llargs[llargs_count]);
           values[i] = &llargs[llargs_count++];
         }
       else if (args[i] == &ffi_type_float)
         {
           ___slogan_obj_to_float(___CDR(arg), &fargs[fargs_count]);
           values[i] = &fargs[fargs_count++];
         }
       else if (args[i] == &ffi_type_double)
         {
           ___slogan_obj_to_double(___CDR(arg), &dargs[dargs_count]);
           values[i] = &dargs[dargs_count++];
         }
       else if (args[i] == &ffi_type_longdouble)
         {
           double d;
           ___slogan_obj_to_double(___CDR(arg), &d);
           ldargs[ldargs_count] = (long double)d;
           values[i] = &ldargs[ldargs_count++];
         }
       else
         {
           int t = 0;
           ___slogan_obj_to_int(___CAR(arg), &t);
           if (t == libffi_type_charstr)
             {
               ___slogan_obj_to_charstring(___CDR(arg), &sargs[sargs_count]);
               values[i] = &sargs[sargs_count++];
             }
           else
             {
               ___slogan_obj_to_void_pointer(___CDR(arg), &pargs[pargs_count]);
               values[i] = &pargs[pargs_count++];
             }
           break;
         }
       tmp = ___CDR(tmp);
     }

   ___slogan_obj_to_int(i_ret_type, &ret_type);
   if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, argc,
                    FFI_TYPE_MAP[ret_type], args) == FFI_OK)
     {
       if (ret_type <= libffi_type_sint)
         {
           int rc;
           ffi_call(&cif, fn, &rc, values);
           retval =  ___fix(rc);
         }
       else if (ret_type <= libffi_type_slong)
         {
           long rc;
           ffi_call(&cif, fn, &rc, values);
           retval = ___fix(rc);
         }
       else if (ret_type <=  libffi_type_sint64)
         {
           long long rc;
           ffi_call(&cif, fn, &rc, values);
           retval = ___fix(rc);
         }
       else if (ret_type == libffi_type_float)
         {
           float rc;
           ffi_call(&cif, fn, &rc, values);
           ___float_to_slogan_obj(rc, &retval);
         }
       else if (ret_type == libffi_type_double)
         {
           double rc;
           ffi_call(&cif, fn, &rc, values);
           ___double_to_slogan_obj(rc, &retval);
         }
       else if (ret_type == libffi_type_longdouble)
         {
           long double rc;
           ffi_call(&cif, fn, &rc, values);
           ___double_to_slogan_obj(rc, &retval);
         }
       else if (ret_type == libffi_type_charstr)
         {
           char *rc;
           ffi_call(&cif, fn, &rc, values);
           ___charstring_to_slogan_obj(rc, &retval);
         }
       else if (ret_type == libffi_type_void)
         {
           ffi_call(&cif, fn, NULL, values);
           retval = ___NUL;
         }
       else
         {
           void *rc;
           ffi_call(&cif, fn, &rc, values);
           ___void_pointer_to_slogan_obj(rc, &retval);
         }
     }
   else retval = ___FAL;

   if (args != NULL) free(args);
   if (values != NULL) free(values);
   if (iargs != NULL) free(iargs);
   if (largs != NULL) free(largs);
   if (llargs != NULL) free(llargs);
   if (fargs != NULL) free(fargs);
   if (dargs != NULL) free(dargs);
   if (ldargs != NULL) free(ldargs);
   if (sargs != NULL) free(sargs);
   if (pargs != NULL) free(pargs);

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
