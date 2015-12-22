;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(c-declare #<<c-declare-end
 
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <dlfcn.h>
#include "../include/slogan.h"

 typedef void (*fn_void)();
 typedef void* (*fn_void_pointer)();
 typedef char* (*fn_char_string)();
 typedef int (*fn_int)();
#ifdef INT64_MAX
 typedef int64_t (*fn_int64)();
#else
 typedef int (*fn_int64)();
 typedef int int64_t;
#endif
 typedef unsigned int (*fn_uint)();
 typedef float (*fn_float)();
 typedef double (*fn_double)();
 typedef ___SCMOBJ (*fn_obj)();

 #define SCHEME_LIBRARY_LINKER ____20_ffi_2e_slgn
 
 ___BEGIN_C_LINKAGE
 extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state);
 ___END_C_LINKAGE

 void ___setup_fn_call()
 {
   ___setup_params_struct setup_params;
   
   ___setup_params_reset (&setup_params);
   
   setup_params.version = ___VERSION;
   setup_params.linker  = SCHEME_LIBRARY_LINKER;
   
   ___setup (&setup_params);
 }

 static void assert_fn_pointer (void *ptr, const char *name) 
 {
   if (ptr == NULL)
     {
       fprintf (stderr, "failed to load function %s.\n", name);
       exit (1);
     }
 }

 void *dlopen_lazy (const char *libname)
 {
   return dlopen (libname, RTLD_LAZY);
 }

 void ffi_call_void_0 (void *handle, const char *fnname)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   (*fn) ();
 }

 void ffi_call_void_1 (void *handle, const char *fnname, 
		       ___SCMOBJ arg)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   (*fn) (arg);
 }

 void ffi_call_void_2 (void *handle, const char *fnname, 
		       ___SCMOBJ arg1, ___SCMOBJ arg2)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   (*fn) (arg1, arg2);
 }

 void ffi_call_void_3 (void *handle, const char *fnname, 
		       ___SCMOBJ arg1, ___SCMOBJ arg2,
                       ___SCMOBJ arg3)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   (*fn) (arg1, arg2, arg3);
 }

 void ffi_call_void_4 (void *handle, const char *fnname, 
		       ___SCMOBJ arg1, ___SCMOBJ arg2,
                       ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   (*fn) (arg1, arg2, arg3, arg4);
 }

 void ffi_call_void_5 (void *handle, const char *fnname, 
		       ___SCMOBJ arg1, ___SCMOBJ arg2,
                       ___SCMOBJ arg3, ___SCMOBJ arg4,
                       ___SCMOBJ arg5)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   (*fn) (arg1, arg2, arg3, arg4, arg5);
 }

 void ffi_call_void_6 (void *handle, const char *fnname, 
		       ___SCMOBJ arg1, ___SCMOBJ arg2,
                       ___SCMOBJ arg3, ___SCMOBJ arg4,
                       ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   (*fn) (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 void ffi_call_void_7 (void *handle, const char *fnname, 
		       ___SCMOBJ arg1, ___SCMOBJ arg2,
                       ___SCMOBJ arg3, ___SCMOBJ arg4,
                       ___SCMOBJ arg5, ___SCMOBJ arg6,
                       ___SCMOBJ arg7)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 void ffi_call_void_8 (void *handle, const char *fnname, 
		       ___SCMOBJ arg1, ___SCMOBJ arg2,
                       ___SCMOBJ arg3, ___SCMOBJ arg4,
                       ___SCMOBJ arg5, ___SCMOBJ arg6,
                       ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 void *ffi_call_void_pointer_0 (void *handle, const char *fnname)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) ();
 }

 void *ffi_call_void_pointer_1 (void *handle, const char *fnname, 
				___SCMOBJ arg)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg);
 }

 void *ffi_call_void_pointer_2 (void *handle, const char *fnname, 
				___SCMOBJ arg1, ___SCMOBJ arg2)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2);
 }

 void *ffi_call_void_pointer_3 (void *handle, const char *fnname, 
				___SCMOBJ arg1, ___SCMOBJ arg2,
                                ___SCMOBJ arg3)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3);
 }

 void *ffi_call_void_pointer_4 (void *handle, const char *fnname, 
				___SCMOBJ arg1, ___SCMOBJ arg2,
                                ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4);
 }

 void *ffi_call_void_pointer_5 (void *handle, const char *fnname, 
				___SCMOBJ arg1, ___SCMOBJ arg2,
                                ___SCMOBJ arg3, ___SCMOBJ arg4,
                                ___SCMOBJ arg5)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5);
 }

 void *ffi_call_void_pointer_6 (void *handle, const char *fnname, 
				___SCMOBJ arg1, ___SCMOBJ arg2,
                                ___SCMOBJ arg3, ___SCMOBJ arg4,
                                ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 void *ffi_call_void_pointer_7 (void *handle, const char *fnname, 
				___SCMOBJ arg1, ___SCMOBJ arg2,
                                ___SCMOBJ arg3, ___SCMOBJ arg4,
                                ___SCMOBJ arg5, ___SCMOBJ arg6,
                                ___SCMOBJ arg7)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 void *ffi_call_void_pointer_8 (void *handle, const char *fnname, 
				___SCMOBJ arg1, ___SCMOBJ arg2,
                                ___SCMOBJ arg3, ___SCMOBJ arg4,
                                ___SCMOBJ arg5, ___SCMOBJ arg6,
                                ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 void *ffi_call_char_string_0 (void *handle, const char *fnname)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) ();
 }

 void *ffi_call_char_string_1 (void *handle, const char *fnname, 
                               ___SCMOBJ arg)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg);
 }

 void *ffi_call_char_string_2 (void *handle, const char *fnname, 
                               ___SCMOBJ arg1, ___SCMOBJ arg2)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2);
 }

 void *ffi_call_char_string_3 (void *handle, const char *fnname, 
                               ___SCMOBJ arg1, ___SCMOBJ arg2,
                               ___SCMOBJ arg3)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3);
 }

 void *ffi_call_char_string_4 (void *handle, const char *fnname, 
                               ___SCMOBJ arg1, ___SCMOBJ arg2,
                               ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4);
 }

 void *ffi_call_char_string_5 (void *handle, const char *fnname, 
                               ___SCMOBJ arg1, ___SCMOBJ arg2,
                               ___SCMOBJ arg3, ___SCMOBJ arg4,
                               ___SCMOBJ arg5)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5);
 }

 void *ffi_call_char_string_6 (void *handle, const char *fnname, 
                               ___SCMOBJ arg1, ___SCMOBJ arg2,
                               ___SCMOBJ arg3, ___SCMOBJ arg4,
                               ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 void *ffi_call_char_string_7 (void *handle, const char *fnname, 
                               ___SCMOBJ arg1, ___SCMOBJ arg2,
                               ___SCMOBJ arg3, ___SCMOBJ arg4,
                               ___SCMOBJ arg5, ___SCMOBJ arg6,
                               ___SCMOBJ arg7)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 void *ffi_call_char_string_8 (void *handle, const char *fnname, 
                               ___SCMOBJ arg1, ___SCMOBJ arg2,
                               ___SCMOBJ arg3, ___SCMOBJ arg4,
                               ___SCMOBJ arg5, ___SCMOBJ arg6,
                               ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 int ffi_call_int_0 (void *handle, const char *fnname)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) ();
 }

 int ffi_call_int_1 (void *handle, const char *fnname, 
		     ___SCMOBJ arg)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg);
 }

 int ffi_call_int_2 (void *handle, const char *fnname, 
		     ___SCMOBJ arg1, ___SCMOBJ arg2)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2);
 }

 int ffi_call_int_3 (void *handle, const char *fnname, 
		     ___SCMOBJ arg1, ___SCMOBJ arg2,
                     ___SCMOBJ arg3)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3);
 }

 int ffi_call_int_4 (void *handle, const char *fnname, 
		     ___SCMOBJ arg1, ___SCMOBJ arg2,
                     ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4);
 }

 int ffi_call_int_5 (void *handle, const char *fnname, 
		     ___SCMOBJ arg1, ___SCMOBJ arg2,
                     ___SCMOBJ arg3, ___SCMOBJ arg4,
                     ___SCMOBJ arg5)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5);
 }

 int ffi_call_int_6 (void *handle, const char *fnname, 
		     ___SCMOBJ arg1, ___SCMOBJ arg2,
                     ___SCMOBJ arg3, ___SCMOBJ arg4,
                     ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 int ffi_call_int_7 (void *handle, const char *fnname, 
		     ___SCMOBJ arg1, ___SCMOBJ arg2,
                     ___SCMOBJ arg3, ___SCMOBJ arg4,
                     ___SCMOBJ arg5, ___SCMOBJ arg6,
                     ___SCMOBJ arg7)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 int ffi_call_int_8 (void *handle, const char *fnname, 
		     ___SCMOBJ arg1, ___SCMOBJ arg2,
                     ___SCMOBJ arg3, ___SCMOBJ arg4,
                     ___SCMOBJ arg5, ___SCMOBJ arg6,
                     ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }


 int64_t ffi_call_int64_0 (void *handle, const char *fnname)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) ();
 }

 int64_t ffi_call_int64_1 (void *handle, const char *fnname, 
                         ___SCMOBJ arg)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg);
 }

 int64_t ffi_call_int64_2 (void *handle, const char *fnname, 
                         ___SCMOBJ arg1, ___SCMOBJ arg2)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2);
 }

 int64_t ffi_call_int64_3 (void *handle, const char *fnname, 
                         ___SCMOBJ arg1, ___SCMOBJ arg2,
                         ___SCMOBJ arg3)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3);
 }

 int64_t ffi_call_int64_4 (void *handle, const char *fnname, 
                         ___SCMOBJ arg1, ___SCMOBJ arg2,
                         ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4);
 }

 int64_t ffi_call_int64_5 (void *handle, const char *fnname, 
                         ___SCMOBJ arg1, ___SCMOBJ arg2,
                         ___SCMOBJ arg3, ___SCMOBJ arg4,
                         ___SCMOBJ arg5)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5);
 }

 int64_t ffi_call_int64_6 (void *handle, const char *fnname, 
                         ___SCMOBJ arg1, ___SCMOBJ arg2,
                         ___SCMOBJ arg3, ___SCMOBJ arg4,
                         ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 int64_t ffi_call_int64_7 (void *handle, const char *fnname, 
                         ___SCMOBJ arg1, ___SCMOBJ arg2,
                         ___SCMOBJ arg3, ___SCMOBJ arg4,
                         ___SCMOBJ arg5, ___SCMOBJ arg6,
                         ___SCMOBJ arg7)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 int64_t ffi_call_int64_8 (void *handle, const char *fnname, 
                         ___SCMOBJ arg1, ___SCMOBJ arg2,
                         ___SCMOBJ arg3, ___SCMOBJ arg4,
                         ___SCMOBJ arg5, ___SCMOBJ arg6,
                         ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 unsigned int ffi_call_uint_0 (void *handle, const char *fnname)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) ();
 }

 unsigned int ffi_call_uint_1 (void *handle, const char *fnname, 
                               ___SCMOBJ arg)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg);
 }

 unsigned int ffi_call_uint_2 (void *handle, const char *fnname, 
                               ___SCMOBJ arg1, ___SCMOBJ arg2)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2);
 }

 unsigned int ffi_call_uint_3 (void *handle, const char *fnname, 
                               ___SCMOBJ arg1, ___SCMOBJ arg2,
                               ___SCMOBJ arg3)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3);
 }

 unsigned int ffi_call_uint_4 (void *handle, const char *fnname, 
                               ___SCMOBJ arg1, ___SCMOBJ arg2,
                               ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4);
 }

 unsigned int ffi_call_uint_5 (void *handle, const char *fnname, 
                               ___SCMOBJ arg1, ___SCMOBJ arg2,
                               ___SCMOBJ arg3, ___SCMOBJ arg4,
                               ___SCMOBJ arg5)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5);
 }

 unsigned int ffi_call_uint_6 (void *handle, const char *fnname, 
                               ___SCMOBJ arg1, ___SCMOBJ arg2,
                               ___SCMOBJ arg3, ___SCMOBJ arg4,
                               ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6);
 }


 unsigned int ffi_call_uint_7 (void *handle, const char *fnname, 
                               ___SCMOBJ arg1, ___SCMOBJ arg2,
                               ___SCMOBJ arg3, ___SCMOBJ arg4,
                               ___SCMOBJ arg5, ___SCMOBJ arg6,
                               ___SCMOBJ arg7)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }


 unsigned int ffi_call_uint_8 (void *handle, const char *fnname, 
                               ___SCMOBJ arg1, ___SCMOBJ arg2,
                               ___SCMOBJ arg3, ___SCMOBJ arg4,
                               ___SCMOBJ arg5, ___SCMOBJ arg6,
                               ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 float ffi_call_float_0 (void *handle, const char *fnname)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) ();
 }

 float ffi_call_float_1 (void *handle, const char *fnname, 
                         ___SCMOBJ arg)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg);
 }

 float ffi_call_float_2 (void *handle, const char *fnname, 
                         ___SCMOBJ arg1, ___SCMOBJ arg2)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2);
 }

 float ffi_call_float_3 (void *handle, const char *fnname, 
                         ___SCMOBJ arg1, ___SCMOBJ arg2,
                         ___SCMOBJ arg3)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3);
 }

 float ffi_call_float_4 (void *handle, const char *fnname, 
                         ___SCMOBJ arg1, ___SCMOBJ arg2,
                         ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4);
 }

 float ffi_call_float_5 (void *handle, const char *fnname, 
                         ___SCMOBJ arg1, ___SCMOBJ arg2,
                         ___SCMOBJ arg3, ___SCMOBJ arg4,
                         ___SCMOBJ arg5)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5);
 }


 float ffi_call_float_6 (void *handle, const char *fnname, 
                         ___SCMOBJ arg1, ___SCMOBJ arg2,
                         ___SCMOBJ arg3, ___SCMOBJ arg4,
                         ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 float ffi_call_float_7 (void *handle, const char *fnname, 
                         ___SCMOBJ arg1, ___SCMOBJ arg2,
                         ___SCMOBJ arg3, ___SCMOBJ arg4,
                         ___SCMOBJ arg5, ___SCMOBJ arg6,
                         ___SCMOBJ arg7)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 float ffi_call_float_8 (void *handle, const char *fnname, 
                         ___SCMOBJ arg1, ___SCMOBJ arg2,
                         ___SCMOBJ arg3, ___SCMOBJ arg4,
                         ___SCMOBJ arg5, ___SCMOBJ arg6,
                         ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 double ffi_call_double_0 (void *handle, const char *fnname)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) ();
 }

 double ffi_call_double_1 (void *handle, const char *fnname, 
                           ___SCMOBJ arg)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg);
 }

 double ffi_call_double_2 (void *handle, const char *fnname, 
                           ___SCMOBJ arg1, ___SCMOBJ arg2)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2);
 }

 double ffi_call_double_3 (void *handle, const char *fnname, 
                           ___SCMOBJ arg1, ___SCMOBJ arg2,
                           ___SCMOBJ arg3)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3);
 }

 double ffi_call_double_4 (void *handle, const char *fnname, 
                           ___SCMOBJ arg1, ___SCMOBJ arg2,
                           ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4);
 }

 double ffi_call_double_5 (void *handle, const char *fnname, 
                           ___SCMOBJ arg1, ___SCMOBJ arg2,
                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                           ___SCMOBJ arg5)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5);
 }

 double ffi_call_double_6 (void *handle, const char *fnname, 
                           ___SCMOBJ arg1, ___SCMOBJ arg2,
                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                           ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 double ffi_call_double_7 (void *handle, const char *fnname, 
                           ___SCMOBJ arg1, ___SCMOBJ arg2,
                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                           ___SCMOBJ arg5, ___SCMOBJ arg6,
                           ___SCMOBJ arg7)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 double ffi_call_double_8 (void *handle, const char *fnname, 
                           ___SCMOBJ arg1, ___SCMOBJ arg2,
                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                           ___SCMOBJ arg5, ___SCMOBJ arg6,
                           ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 ___SCMOBJ ffi_call_obj_0 (void *handle, const char *fnname)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) ();
 }

 ___SCMOBJ ffi_call_obj_1 (void *handle, const char *fnname, 
			   ___SCMOBJ arg)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg);
 }

 ___SCMOBJ ffi_call_obj_2 (void *handle, const char *fnname, 
			   ___SCMOBJ arg1, ___SCMOBJ arg2)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2);
 }

 ___SCMOBJ ffi_call_obj_3 (void *handle, const char *fnname, 
			   ___SCMOBJ arg1, ___SCMOBJ arg2,
                           ___SCMOBJ arg3)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3);
 }

 ___SCMOBJ ffi_call_obj_4 (void *handle, const char *fnname, 
			   ___SCMOBJ arg1, ___SCMOBJ arg2,
                           ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4);
 }

 ___SCMOBJ ffi_call_obj_5 (void *handle, const char *fnname, 
			   ___SCMOBJ arg1, ___SCMOBJ arg2,
                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                           ___SCMOBJ arg5)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5);
 }

 ___SCMOBJ ffi_call_obj_6 (void *handle, const char *fnname, 
			   ___SCMOBJ arg1, ___SCMOBJ arg2,
                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                           ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 ___SCMOBJ ffi_call_obj_7 (void *handle, const char *fnname, 
			   ___SCMOBJ arg1, ___SCMOBJ arg2,
                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                           ___SCMOBJ arg5, ___SCMOBJ arg6,
                           ___SCMOBJ arg7)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 ___SCMOBJ ffi_call_obj_8 (void *handle, const char *fnname, 
			   ___SCMOBJ arg1, ___SCMOBJ arg2,
                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                           ___SCMOBJ arg5, ___SCMOBJ arg6,
                           ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 void ffi_call_with_void_pointer_1 (void *handle, const char *fnname,
                                    void *arg)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   fn (arg);
 }

 void ffi_call_with_void_pointer_2 (void *handle, const char *fnname,
                                    void *arg1, ___SCMOBJ arg2)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   fn (arg1, arg2);
 }

 void ffi_call_with_void_pointer_3 (void *handle, const char *fnname,
                                    void *arg1, ___SCMOBJ arg2,
                                    ___SCMOBJ arg3)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   fn (arg1, arg2, arg3);
 }

 void ffi_call_with_void_pointer_4 (void *handle, const char *fnname,
                                    void *arg1, ___SCMOBJ arg2,
                                    ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   fn (arg1, arg2, arg3, arg4);
 }

 void ffi_call_with_void_pointer_5 (void *handle, const char *fnname,
                                    void *arg1, ___SCMOBJ arg2,
                                    ___SCMOBJ arg3, ___SCMOBJ arg4,
                                    ___SCMOBJ arg5)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   fn (arg1, arg2, arg3, arg4, arg5);
 }

 void ffi_call_with_void_pointer_6 (void *handle, const char *fnname,
                                    void *arg1, ___SCMOBJ arg2,
                                    ___SCMOBJ arg3, ___SCMOBJ arg4,
                                    ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   fn (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 void ffi_call_with_void_pointer_7 (void *handle, const char *fnname,
                                    void *arg1, ___SCMOBJ arg2,
                                    ___SCMOBJ arg3, ___SCMOBJ arg4,
                                    ___SCMOBJ arg5, ___SCMOBJ arg6,
                                    ___SCMOBJ arg7)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 void ffi_call_with_void_pointer_8 (void *handle, const char *fnname,
                                    void *arg1, ___SCMOBJ arg2,
                                    ___SCMOBJ arg3, ___SCMOBJ arg4,
                                    ___SCMOBJ arg5, ___SCMOBJ arg6,
                                    ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_void fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 void *ffi_call_void_pointer_with_void_pointer_1 (void *handle, const char *fnname,
                                                  void *arg)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg);
 }

 void *ffi_call_void_pointer_with_void_pointer_2 (void *handle, const char *fnname,
                                                  void *arg1, ___SCMOBJ arg2)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2);
 }

 void *ffi_call_void_pointer_with_void_pointer_3 (void *handle, const char *fnname,
                                                  void *arg1, ___SCMOBJ arg2,
                                                  ___SCMOBJ arg3)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3);
 }

 void *ffi_call_void_pointer_with_void_pointer_4 (void *handle, const char *fnname,
                                                  void *arg1, ___SCMOBJ arg2,
                                                  ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4);
 }

 void *ffi_call_void_pointer_with_void_pointer_5 (void *handle, const char *fnname,
                                                  void *arg1, ___SCMOBJ arg2,
                                                  ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                  ___SCMOBJ arg5)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5);
 }

 void *ffi_call_void_pointer_with_void_pointer_6 (void *handle, const char *fnname,
                                                  void *arg1, ___SCMOBJ arg2,
                                                  ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                  ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 void *ffi_call_void_pointer_with_void_pointer_7 (void *handle, const char *fnname,
                                                  void *arg1, ___SCMOBJ arg2,
                                                  ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                  ___SCMOBJ arg5, ___SCMOBJ arg6,
                                                  ___SCMOBJ arg7)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 void *ffi_call_void_pointer_with_void_pointer_8 (void *handle, const char *fnname,
                                                  void *arg1, ___SCMOBJ arg2,
                                                  ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                  ___SCMOBJ arg5, ___SCMOBJ arg6,
                                                  ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_void_pointer fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 int ffi_call_int_with_void_pointer_1 (void *handle, const char *fnname,
                                       void *arg)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg);
 }

 int ffi_call_int_with_void_pointer_2 (void *handle, const char *fnname,
                                       void *arg1, ___SCMOBJ arg2)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2);
 }

 int ffi_call_int_with_void_pointer_3 (void *handle, const char *fnname,
                                       void *arg1, ___SCMOBJ arg2,
                                       ___SCMOBJ arg3)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3);
 }

 int ffi_call_int_with_void_pointer_4 (void *handle, const char *fnname,
                                       void *arg1, ___SCMOBJ arg2,
                                       ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4);
 }

 int ffi_call_int_with_void_pointer_5 (void *handle, const char *fnname,
                                       void *arg1, ___SCMOBJ arg2,
                                       ___SCMOBJ arg3, ___SCMOBJ arg4,
                                       ___SCMOBJ arg5)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5);
 }

 int ffi_call_int_with_void_pointer_6 (void *handle, const char *fnname,
                                       void *arg1, ___SCMOBJ arg2,
                                       ___SCMOBJ arg3, ___SCMOBJ arg4,
                                       ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 int ffi_call_int_with_void_pointer_7 (void *handle, const char *fnname,
                                       void *arg1, ___SCMOBJ arg2,
                                       ___SCMOBJ arg3, ___SCMOBJ arg4,
                                       ___SCMOBJ arg5, ___SCMOBJ arg6,
                                       ___SCMOBJ arg7)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 int ffi_call_int_with_void_pointer_8 (void *handle, const char *fnname,
                                       void *arg1, ___SCMOBJ arg2,
                                       ___SCMOBJ arg3, ___SCMOBJ arg4,
                                       ___SCMOBJ arg5, ___SCMOBJ arg6,
                                       ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_int fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 double ffi_call_double_with_void_pointer_1 (void *handle, const char *fnname,
                                             void *arg)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg);
 }

 double ffi_call_double_with_void_pointer_2 (void *handle, const char *fnname,
                                             void *arg1, ___SCMOBJ arg2)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2);
 }

 double ffi_call_double_with_void_pointer_3 (void *handle, const char *fnname,
                                             void *arg1, ___SCMOBJ arg2,
                                             ___SCMOBJ arg3)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3);
 }

 double ffi_call_double_with_void_pointer_4 (void *handle, const char *fnname,
                                             void *arg1, ___SCMOBJ arg2,
                                             ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4);
 }

 double ffi_call_double_with_void_pointer_5 (void *handle, const char *fnname,
                                             void *arg1, ___SCMOBJ arg2,
                                             ___SCMOBJ arg3, ___SCMOBJ arg4,
                                             ___SCMOBJ arg5)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5);
 }

 double ffi_call_double_with_void_pointer_6 (void *handle, const char *fnname,
                                             void *arg1, ___SCMOBJ arg2,
                                             ___SCMOBJ arg3, ___SCMOBJ arg4,
                                             ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 double ffi_call_double_with_void_pointer_7 (void *handle, const char *fnname,
                                             void *arg1, ___SCMOBJ arg2,
                                             ___SCMOBJ arg3, ___SCMOBJ arg4,
                                             ___SCMOBJ arg5, ___SCMOBJ arg6,
                                             ___SCMOBJ arg7)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 double ffi_call_double_with_void_pointer_8 (void *handle, const char *fnname,
                                             void *arg1, ___SCMOBJ arg2,
                                             ___SCMOBJ arg3, ___SCMOBJ arg4,
                                             ___SCMOBJ arg5, ___SCMOBJ arg6,
                                             ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_double fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 float ffi_call_float_with_void_pointer_1 (void *handle, const char *fnname,
                                           void *arg)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg);
 }

 float ffi_call_float_with_void_pointer_2 (void *handle, const char *fnname,
                                           void *arg1, ___SCMOBJ arg2)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2);
 }

 float ffi_call_float_with_void_pointer_3 (void *handle, const char *fnname,
                                           void *arg1, ___SCMOBJ arg2,
                                           ___SCMOBJ arg3)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3);
 }

 float ffi_call_float_with_void_pointer_4 (void *handle, const char *fnname,
                                           void *arg1, ___SCMOBJ arg2,
                                           ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4);
 }

 float ffi_call_float_with_void_pointer_5 (void *handle, const char *fnname,
                                           void *arg1, ___SCMOBJ arg2,
                                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                                           ___SCMOBJ arg5)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5);
 }

 float ffi_call_float_with_void_pointer_6 (void *handle, const char *fnname,
                                           void *arg1, ___SCMOBJ arg2,
                                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                                           ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 float ffi_call_float_with_void_pointer_7 (void *handle, const char *fnname,
                                           void *arg1, ___SCMOBJ arg2,
                                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                                           ___SCMOBJ arg5, ___SCMOBJ arg6,
                                           ___SCMOBJ arg7)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 float ffi_call_float_with_void_pointer_8 (void *handle, const char *fnname,
                                           void *arg1, ___SCMOBJ arg2,
                                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                                           ___SCMOBJ arg5, ___SCMOBJ arg6,
                                           ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_float fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 int64_t ffi_call_int64_with_void_pointer_1 (void *handle, const char *fnname,
                                           void *arg)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg);
 }

 int64_t ffi_call_int64_with_void_pointer_2 (void *handle, const char *fnname,
                                           void *arg1, ___SCMOBJ arg2)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2);
 }

 int64_t ffi_call_int64_with_void_pointer_3 (void *handle, const char *fnname,
                                           void *arg1, ___SCMOBJ arg2,
                                           ___SCMOBJ arg3)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3);
 }

 int64_t ffi_call_int64_with_void_pointer_4 (void *handle, const char *fnname,
                                           void *arg1, ___SCMOBJ arg2,
                                           ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4);
 }

 int64_t ffi_call_int64_with_void_pointer_5 (void *handle, const char *fnname,
                                           void *arg1, ___SCMOBJ arg2,
                                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                                           ___SCMOBJ arg5)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5);
 }

 int64_t ffi_call_int64_with_void_pointer_6 (void *handle, const char *fnname,
                                           void *arg1, ___SCMOBJ arg2,
                                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                                           ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 int64_t ffi_call_int64_with_void_pointer_7 (void *handle, const char *fnname,
                                           void *arg1, ___SCMOBJ arg2,
                                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                                           ___SCMOBJ arg5, ___SCMOBJ arg6,
                                           ___SCMOBJ arg7)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 int64_t ffi_call_int64_with_void_pointer_8 (void *handle, const char *fnname,
                                           void *arg1, ___SCMOBJ arg2,
                                           ___SCMOBJ arg3, ___SCMOBJ arg4,
                                           ___SCMOBJ arg5, ___SCMOBJ arg6,
                                           ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_int64 fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 unsigned int ffi_call_uint_with_void_pointer_1 (void *handle, const char *fnname,
                                                 void *arg)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg);
 }

 unsigned int ffi_call_uint_with_void_pointer_2 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2);
 }

 unsigned int ffi_call_uint_with_void_pointer_3 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3);
 }

 unsigned int ffi_call_uint_with_void_pointer_4 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4);
 }

 unsigned int ffi_call_uint_with_void_pointer_5 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                 ___SCMOBJ arg5)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5);
 }

 unsigned int ffi_call_uint_with_void_pointer_6 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                 ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 unsigned int ffi_call_uint_with_void_pointer_7 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                 ___SCMOBJ arg5, ___SCMOBJ arg6,
                                                 ___SCMOBJ arg7)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 unsigned int ffi_call_uint_with_void_pointer_8 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                 ___SCMOBJ arg5, ___SCMOBJ arg6,
                                                 ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 void *ffi_call_char_string_with_void_pointer_1 (void *handle, const char *fnname,
                                                 void *arg)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg);
 }

 void *ffi_call_char_string_with_void_pointer_2 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2);
 }

 void *ffi_call_char_string_with_void_pointer_3 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3);
 }

 void *ffi_call_char_string_with_void_pointer_4 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4);
 }

 void *ffi_call_char_string_with_void_pointer_5 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                 ___SCMOBJ arg5)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5);
 }

 void *ffi_call_char_string_with_void_pointer_6 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                 ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 void *ffi_call_char_string_with_void_pointer_7 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                 ___SCMOBJ arg5, ___SCMOBJ arg6,
                                                 ___SCMOBJ arg7)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 void *ffi_call_char_string_with_void_pointer_8 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                 ___SCMOBJ arg5, ___SCMOBJ arg6,
                                                 ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_char_string fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 ___SCMOBJ ffi_call_obj_with_void_pointer_1 (void *handle, const char *fnname,
                                                 void *arg)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg);
 }

 ___SCMOBJ ffi_call_obj_with_void_pointer_2 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2);
 }

 ___SCMOBJ ffi_call_obj_with_void_pointer_3 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3);
 }

 ___SCMOBJ ffi_call_obj_with_void_pointer_4 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4);
 }

 ___SCMOBJ ffi_call_obj_with_void_pointer_5 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                 ___SCMOBJ arg5)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5);
 }

 ___SCMOBJ ffi_call_obj_with_void_pointer_6 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                 ___SCMOBJ arg5, ___SCMOBJ arg6)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6);
 }

 ___SCMOBJ ffi_call_obj_with_void_pointer_7 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                 ___SCMOBJ arg5, ___SCMOBJ arg6,
                                                 ___SCMOBJ arg7)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7);
 }

 ___SCMOBJ ffi_call_obj_with_void_pointer_8 (void *handle, const char *fnname,
                                                 void *arg1, ___SCMOBJ arg2,
                                                 ___SCMOBJ arg3, ___SCMOBJ arg4,
                                                 ___SCMOBJ arg5, ___SCMOBJ arg6,
                                                 ___SCMOBJ arg7, ___SCMOBJ arg8)
 {
   fn_obj fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return fn (arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
 }

 /* Implementation of functions declared in slogan.h */
 
 ___slogan_obj ___alloc_u8array(size_t size)
 {
   ___slogan_obj result = (___slogan_obj)___EXT(___alloc_scmobj)(___PSTATE, ___sU8VECTOR, size);
   ___EXT(___release_scmobj)(result);
   return result;
 }
 
 ___slogan_obj ___alloc_array(size_t size)
 {
   ___SCMOBJ init = ___FAL;
   ___SCMOBJ result = ___EXT(___make_vector)(___PSTATE, size, init);
   return result;
 }

c-declare-end
)

(c-define-type void-pointer (pointer void))

(define _ffi_open (c-lambda (char-string) void-pointer "dlopen_lazy"))

(define (ffi_open libname)
  (if (not (file-exists? libname))
      (let ((ld-paths (getenv "LD_LIBRARY_PATH")))
        (if ld-paths
            (set! ld-paths (string_split ld-paths '(#\:))))
        (let loop ((ld-paths ld-paths))
          (if (null? ld-paths) 
              #f
              (let ((libname (string-append (car ld-paths) "/" libname)))
                (if (file-exists? libname)
                    (_ffi_open libname)
                    (loop (cdr ld-paths)))))))
      (_ffi_open libname)))

(define _ffi_close (c-lambda (void-pointer) int "dlclose"))

(define (ffi_close lib) 
  (zero? (_ffi_close lib)))

(define ffi_fn (c-lambda (void-pointer char-string) void-pointer "dlsym"))

(define ffi_call_void_0 (c-lambda (void-pointer char-string) 
                                  void 
                                  "ffi_call_void_0"))
(define ffi_call_void_1 (c-lambda (void-pointer char-string 
                                                scheme-object) 
                                  void 
                                  "ffi_call_void_1"))
(define ffi_call_void_2 (c-lambda (void-pointer char-string 
                                                scheme-object scheme-object) 
                                  void 
                                  "ffi_call_void_2"))
(define ffi_call_void_3 (c-lambda (void-pointer char-string 
                                                scheme-object scheme-object 
                                                scheme-object) 
                                  void 
                                  "ffi_call_void_3"))
(define ffi_call_void_4 (c-lambda (void-pointer char-string 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object) 
                                  void 
                                  "ffi_call_void_4"))
(define ffi_call_void_5 (c-lambda (void-pointer char-string 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object
                                                scheme-object) 
                                  void 
                                  "ffi_call_void_5"))
(define ffi_call_void_6 (c-lambda (void-pointer char-string 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object) 
                                  void 
                                  "ffi_call_void_6"))
(define ffi_call_void_7 (c-lambda (void-pointer char-string 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object 
                                                scheme-object) 
                                  void 
                                  "ffi_call_void_7"))
(define ffi_call_void_8 (c-lambda (void-pointer char-string 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object) 
                                  void 
                                  "ffi_call_void_8"))

(define ffi_call_void_pointer_0 (c-lambda (void-pointer char-string) 
                                          void-pointer 
                                          "ffi_call_void_pointer_0"))
(define ffi_call_void_pointer_1 (c-lambda (void-pointer char-string 
                                                        scheme-object) 
                                          void-pointer 
                                          "ffi_call_void_pointer_1"))
(define ffi_call_void_pointer_2 (c-lambda (void-pointer char-string 
                                                        scheme-object scheme-object) 
                                          void-pointer 
                                          "ffi_call_void_pointer_2"))
(define ffi_call_void_pointer_3 (c-lambda (void-pointer char-string 
                                                        scheme-object scheme-object 
                                                        scheme-object) 
                                          void-pointer
                                          "ffi_call_void_pointer_3"))
(define ffi_call_void_pointer_4 (c-lambda (void-pointer char-string 
                                                        scheme-object scheme-object 
                                                        scheme-object scheme-object) 
                                          void-pointer 
                                          "ffi_call_void_pointer_4"))
(define ffi_call_void_pointer_5 (c-lambda (void-pointer char-string 
                                                        scheme-object scheme-object 
                                                        scheme-object scheme-object
                                                        scheme-object) 
                                          void-pointer 
                                          "ffi_call_void_pointer_5"))
(define ffi_call_void_pointer_6 (c-lambda (void-pointer char-string 
                                                        scheme-object scheme-object 
                                                        scheme-object scheme-object 
                                                        scheme-object scheme-object) 
                                          void-pointer 
                                          "ffi_call_void_pointer_6"))
(define ffi_call_void_pointer_7 (c-lambda (void-pointer char-string 
                                                        scheme-object scheme-object 
                                                        scheme-object scheme-object 
                                                        scheme-object scheme-object 
                                                        scheme-object) 
                                          void-pointer 
                                          "ffi_call_void_pointer_7"))
(define ffi_call_void_pointer_8 (c-lambda (void-pointer char-string 
                                                        scheme-object scheme-object 
                                                        scheme-object scheme-object 
                                                        scheme-object scheme-object 
                                                        scheme-object scheme-object) 
                                          void-pointer
                                          "ffi_call_void_pointer_8"))

(define ffi_call_char_string_0 (c-lambda (void-pointer char-string) 
                                         char-string
                                         "ffi_call_char_string_0"))
(define ffi_call_char_string_1 (c-lambda (void-pointer char-string 
                                                       scheme-object) 
                                         char-string
                                         "ffi_call_char_string_1"))
(define ffi_call_char_string_2 (c-lambda (void-pointer char-string 
                                                       scheme-object scheme-object) 
                                         char-string
                                         "ffi_call_char_string_2"))
(define ffi_call_char_string_3 (c-lambda (void-pointer char-string 
                                                       scheme-object scheme-object 
                                                       scheme-object) 
                                         char-string
                                         "ffi_call_char_string_3"))
(define ffi_call_char_string_4 (c-lambda (void-pointer char-string 
                                                       scheme-object scheme-object 
                                                       scheme-object scheme-object) 
                                         char-string
                                         "ffi_call_char_string_4"))
(define ffi_call_char_string_5 (c-lambda (void-pointer char-string 
                                                       scheme-object scheme-object 
                                                       scheme-object scheme-object
                                                       scheme-object) 
                                         char-string
                                         "ffi_call_char_string_5"))
(define ffi_call_char_string_6 (c-lambda (void-pointer char-string 
                                                       scheme-object scheme-object 
                                                       scheme-object scheme-object 
                                                       scheme-object scheme-object) 
                                         char-string
                                         "ffi_call_char_string_6"))
(define ffi_call_char_string_7 (c-lambda (void-pointer char-string 
                                                       scheme-object scheme-object 
                                                       scheme-object scheme-object 
                                                       scheme-object scheme-object 
                                                       scheme-object) 
                                         char-string
                                         "ffi_call_char_string_7"))
(define ffi_call_char_string_8 (c-lambda (void-pointer char-string 
                                                       scheme-object scheme-object 
                                                       scheme-object scheme-object 
                                                       scheme-object scheme-object 
                                                       scheme-object scheme-object) 
                                         char-string
                                         "ffi_call_char_string_8"))

(define ffi_call_int_0 (c-lambda (void-pointer char-string) 
                                 int 
                                 "ffi_call_int_0"))
(define ffi_call_int_1 (c-lambda (void-pointer char-string 
                                               scheme-object) 
                                 int 
                                 "ffi_call_int_1"))
(define ffi_call_int_2 (c-lambda (void-pointer char-string 
                                               scheme-object scheme-object) 
                                 int 
                                 "ffi_call_int_2"))
(define ffi_call_int_3 (c-lambda (void-pointer char-string 
                                               scheme-object scheme-object 
                                               scheme-object) 
                                 int
                                 "ffi_call_int_3"))
(define ffi_call_int_4 (c-lambda (void-pointer char-string 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object) 
                                 int 
                                 "ffi_call_int_4"))
(define ffi_call_int_5 (c-lambda (void-pointer char-string 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object
                                               scheme-object) 
                                 int 
                                 "ffi_call_int_5"))
(define ffi_call_int_6 (c-lambda (void-pointer char-string 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object) 
                                 int 
                                 "ffi_call_int_6"))
(define ffi_call_int_7 (c-lambda (void-pointer char-string 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object 
                                               scheme-object) 
                                 int 
                                 "ffi_call_int_7"))
(define ffi_call_int_8 (c-lambda (void-pointer char-string 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object) 
                                 int
                                 "ffi_call_int_8"))

(define ffi_call_int64_0 (c-lambda (void-pointer char-string) 
                                   int64 
                                   "ffi_call_int64_0"))
(define ffi_call_int64_1 (c-lambda (void-pointer char-string 
                                                 scheme-object) 
                                   int64 
                                   "ffi_call_int64_1"))
(define ffi_call_int64_2 (c-lambda (void-pointer char-string 
                                                 scheme-object scheme-object) 
                                   int64 
                                   "ffi_call_int64_2"))
(define ffi_call_int64_3 (c-lambda (void-pointer char-string 
                                                 scheme-object scheme-object 
                                                 scheme-object) 
                                   int64
                                   "ffi_call_int64_3"))
(define ffi_call_int64_4 (c-lambda (void-pointer char-string 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object) 
                                   int64
                                   "ffi_call_int64_4"))
(define ffi_call_int64_5 (c-lambda (void-pointer char-string 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object
                                                 scheme-object) 
                                   int64 
                                   "ffi_call_int64_5"))
(define ffi_call_int64_6 (c-lambda (void-pointer char-string 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object) 
                                   int64 
                                   "ffi_call_int64_6"))
(define ffi_call_int64_7 (c-lambda (void-pointer char-string 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object
                                                 scheme-object) 
                                   int64 
                                   "ffi_call_int64_7"))
(define ffi_call_int64_8 (c-lambda (void-pointer char-string 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object) 
                                   int64
                                   "ffi_call_int64_8"))

(define ffi_call_uint_0 (c-lambda (void-pointer char-string) 
                                  unsigned-int 
                                  "ffi_call_uint_0"))
(define ffi_call_uint_1 (c-lambda (void-pointer char-string 
                                                scheme-object) 
                                  unsigned-int 
                                  "ffi_call_uint_1"))
(define ffi_call_uint_2 (c-lambda (void-pointer char-string 
                                                scheme-object scheme-object) 
                                  unsigned-int 
                                  "ffi_call_uint_2"))
(define ffi_call_uint_3 (c-lambda (void-pointer char-string 
                                                scheme-object scheme-object 
                                                scheme-object) 
                                  unsigned-int
                                  "ffi_call_uint_3"))
(define ffi_call_uint_4 (c-lambda (void-pointer char-string 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object) 
                                  unsigned-int
                                  "ffi_call_uint_4"))
(define ffi_call_uint_5 (c-lambda (void-pointer char-string 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object
                                                scheme-object) 
                                  unsigned-int 
                                  "ffi_call_uint_5"))
(define ffi_call_uint_6 (c-lambda (void-pointer char-string 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object) 
                                  unsigned-int 
                                  "ffi_call_uint_6"))
(define ffi_call_uint_7 (c-lambda (void-pointer char-string 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object 
                                                scheme-object) 
                                  unsigned-int 
                                  "ffi_call_uint_7"))
(define ffi_call_uint_8 (c-lambda (void-pointer char-string 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object 
                                                scheme-object scheme-object) 
                                  unsigned-int
                                  "ffi_call_uint_8"))

(define ffi_call_float_0 (c-lambda (void-pointer char-string) 
                                   float 
                                   "ffi_call_float_0"))
(define ffi_call_float_1 (c-lambda (void-pointer char-string 
                                                 scheme-object) 
                                   float 
                                   "ffi_call_float_1"))
(define ffi_call_float_2 (c-lambda (void-pointer char-string 
                                                 scheme-object scheme-object) 
                                   float 
                                   "ffi_call_float_2"))
(define ffi_call_float_3 (c-lambda (void-pointer char-string 
                                                 scheme-object scheme-object 
                                                 scheme-object) 
                                   float
                                   "ffi_call_float_3"))
(define ffi_call_float_4 (c-lambda (void-pointer char-string 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object) 
                                   float
                                   "ffi_call_float_4"))
(define ffi_call_float_5 (c-lambda (void-pointer char-string 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object
                                                 scheme-object) 
                                   float 
                                   "ffi_call_float_5"))
(define ffi_call_float_6 (c-lambda (void-pointer char-string 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object) 
                                   float 
                                   "ffi_call_float_6"))
(define ffi_call_float_7 (c-lambda (void-pointer char-string 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object 
                                                 scheme-object) 
                                   float 
                                   "ffi_call_float_7"))
(define ffi_call_float_8 (c-lambda (void-pointer char-string 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object 
                                                 scheme-object scheme-object) 
                                   float
                                   "ffi_call_float_8"))

(define ffi_call_double_0 (c-lambda (void-pointer char-string) 
                                    double 
                                    "ffi_call_double_0"))
(define ffi_call_double_1 (c-lambda (void-pointer char-string 
                                                  scheme-object) 
                                    double 
                                    "ffi_call_double_1"))
(define ffi_call_double_2 (c-lambda (void-pointer char-string 
                                                  scheme-object scheme-object) 
                                    double 
                                    "ffi_call_double_2"))
(define ffi_call_double_3 (c-lambda (void-pointer char-string 
                                                  scheme-object scheme-object 
                                                  scheme-object) 
                                    double
                                    "ffi_call_double_3"))
(define ffi_call_double_4 (c-lambda (void-pointer char-string 
                                                  scheme-object scheme-object 
                                                  scheme-object scheme-object) 
                                    double
                                    "ffi_call_double_4"))
(define ffi_call_double_5 (c-lambda (void-pointer char-string 
                                                  scheme-object scheme-object 
                                                  scheme-object scheme-object
                                                  scheme-object) 
                                    double 
                                    "ffi_call_double_5"))
(define ffi_call_double_6 (c-lambda (void-pointer char-string 
                                                  scheme-object scheme-object 
                                                  scheme-object scheme-object 
                                                  scheme-object scheme-object) 
                                    double 
                                    "ffi_call_double_6"))
(define ffi_call_double_7 (c-lambda (void-pointer char-string 
                                                  scheme-object scheme-object 
                                                  scheme-object scheme-object 
                                                  scheme-object scheme-object 
                                                  scheme-object) 
                                    double 
                                    "ffi_call_double_7"))
(define ffi_call_double_8 (c-lambda (void-pointer char-string 
                                                  scheme-object scheme-object 
                                                  scheme-object scheme-object 
                                                  scheme-object scheme-object 
                                                  scheme-object scheme-object) 
                                    double
                                    "ffi_call_double_8"))

(define ffi_call_obj_0 (c-lambda (void-pointer char-string) 
                                 scheme-object 
                                 "ffi_call_obj_0"))
(define ffi_call_obj_1 (c-lambda (void-pointer char-string 
                                               scheme-object) 
                                 scheme-object 
                                 "ffi_call_obj_1"))
(define ffi_call_obj_2 (c-lambda (void-pointer char-string 
                                               scheme-object scheme-object) 
                                 scheme-object 
                                 "ffi_call_obj_2"))
(define ffi_call_obj_3 (c-lambda (void-pointer char-string 
                                               scheme-object scheme-object 
                                               scheme-object) 
                                 scheme-object
                                 "ffi_call_obj_3"))
(define ffi_call_obj_4 (c-lambda (void-pointer char-string 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object) 
                                 scheme-object
                                 "ffi_call_obj_4"))
(define ffi_call_obj_5 (c-lambda (void-pointer char-string 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object
                                               scheme-object) 
                                 scheme-object 
                                 "ffi_call_obj_5"))
(define ffi_call_obj_6 (c-lambda (void-pointer char-string 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object) 
                                 scheme-object 
                                 "ffi_call_obj_6"))
(define ffi_call_obj_7 (c-lambda (void-pointer char-string 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object 
                                               scheme-object) 
                                 scheme-object 
                                 "ffi_call_obj_7"))
(define ffi_call_obj_8 (c-lambda (void-pointer char-string 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object 
                                               scheme-object scheme-object) 
                                 scheme-object
                                 "ffi_call_obj_8"))

(define ffi_call_void_with_void_pointer_1 (c-lambda (void-pointer char-string 
                                                                  void-pointer) 
                                                    void
                                                    "ffi_call_with_void_pointer_1"))

(define ffi_call_void_with_void_pointer_2 (c-lambda (void-pointer char-string 
                                                                  void-pointer scheme-object) 
                                                    void
                                                    "ffi_call_with_void_pointer_2"))

(define ffi_call_void_with_void_pointer_3 (c-lambda (void-pointer char-string 
                                                                  void-pointer scheme-object 
                                                                  scheme-object) 
                                                    void
                                                    "ffi_call_with_void_pointer_3"))

(define ffi_call_void_with_void_pointer_4 (c-lambda (void-pointer char-string 
                                                                  void-pointer scheme-object 
                                                                  scheme-object scheme-object)
                                                    void
                                                    "ffi_call_with_void_pointer_4"))

(define ffi_call_void_with_void_pointer_5 (c-lambda (void-pointer char-string 
                                                                  void-pointer scheme-object 
                                                                  scheme-object scheme-object
                                                                  scheme-object) 
                                                    void
                                                    "ffi_call_with_void_pointer_5"))

(define ffi_call_void_with_void_pointer_6 (c-lambda (void-pointer char-string 
                                                                  void-pointer scheme-object 
                                                                  scheme-object scheme-object
                                                                  scheme-object scheme-object)
                                                    void
                                                    "ffi_call_with_void_pointer_6"))

(define ffi_call_void_with_void_pointer_7 (c-lambda (void-pointer char-string 
                                                                  void-pointer scheme-object 
                                                                  scheme-object scheme-object
                                                                  scheme-object scheme-object
                                                                  scheme-object) 
                                                    void
                                                    "ffi_call_with_void_pointer_7"))

(define ffi_call_void_with_void_pointer_8 (c-lambda (void-pointer char-string 
                                                                  void-pointer scheme-object 
                                                                  scheme-object scheme-object
                                                                  scheme-object scheme-object
                                                                  scheme-object scheme-object) 
                                                    void
                                                    "ffi_call_with_void_pointer_8"))


(define ffi_call_void_pointer_with_void_pointer_1 (c-lambda (void-pointer char-string void-pointer) 
                                                            void-pointer
                                                            "ffi_call_void_pointer_with_void_pointer_1"))

(define ffi_call_void_pointer_with_void_pointer_2 (c-lambda (void-pointer char-string 
                                                                          void-pointer scheme-object) 
                                                            void-pointer
                                                            "ffi_call_void_pointer_with_void_pointer_2"))

(define ffi_call_void_pointer_with_void_pointer_3 (c-lambda (void-pointer char-string 
                                                                          void-pointer scheme-object 
                                                                          scheme-object) 
                                                            void-pointer
                                                            "ffi_call_void_pointer_with_void_pointer_3"))

(define ffi_call_void_pointer_with_void_pointer_4 (c-lambda (void-pointer char-string 
                                                                          void-pointer scheme-object 
                                                                          scheme-object scheme-object)
                                                            void-pointer
                                                            "ffi_call_void_pointer_with_void_pointer_4"))

(define ffi_call_void_pointer_with_void_pointer_5 (c-lambda (void-pointer char-string 
                                                                          void-pointer scheme-object 
                                                                          scheme-object scheme-object
                                                                          scheme-object) 
                                                            void-pointer
                                                            "ffi_call_void_pointer_with_void_pointer_5"))

(define ffi_call_void_pointer_with_void_pointer_6 (c-lambda (void-pointer char-string 
                                                                          void-pointer scheme-object 
                                                                          scheme-object scheme-object
                                                                          scheme-object scheme-object)
                                                            void-pointer
                                                            "ffi_call_void_pointer_with_void_pointer_6"))

(define ffi_call_void_pointer_with_void_pointer_7 (c-lambda (void-pointer char-string 
                                                                          void-pointer scheme-object 
                                                                          scheme-object scheme-object
                                                                          scheme-object scheme-object
                                                                          scheme-object) 
                                                            void-pointer
                                                            "ffi_call_void_pointer_with_void_pointer_7"))

(define ffi_call_void_pointer_with_void_pointer_8 (c-lambda (void-pointer char-string 
                                                                          void-pointer scheme-object 
                                                                          scheme-object scheme-object
                                                                          scheme-object scheme-object
                                                                          scheme-object scheme-object) 
                                                            void-pointer
                                                            "ffi_call_void_pointer_with_void_pointer_8"))

(define ffi_call_obj_with_void_pointer_1 (c-lambda (void-pointer char-string void-pointer) 
                                                   scheme-object
                                                   "ffi_call_obj_with_void_pointer_1"))

(define ffi_call_obj_with_void_pointer_2 (c-lambda (void-pointer char-string 
                                                                 void-pointer scheme-object) 
                                                   scheme-object
                                                   "ffi_call_obj_with_void_pointer_2"))

(define ffi_call_obj_with_void_pointer_3 (c-lambda (void-pointer char-string 
                                                                 void-pointer scheme-object 
                                                                 scheme-object) 
                                                   scheme-object
                                                   "ffi_call_obj_with_void_pointer_3"))

(define ffi_call_obj_with_void_pointer_4 (c-lambda (void-pointer char-string 
                                                                 void-pointer scheme-object 
                                                                 scheme-object scheme-object)
                                                   scheme-object
                                                   "ffi_call_obj_with_void_pointer_4"))

(define ffi_call_obj_with_void_pointer_5 (c-lambda (void-pointer char-string 
                                                                 void-pointer scheme-object 
                                                                 scheme-object scheme-object
                                                                 scheme-object) 
                                                   scheme-object
                                                   "ffi_call_obj_with_void_pointer_5"))

(define ffi_call_obj_with_void_pointer_6 (c-lambda (void-pointer char-string 
                                                                 void-pointer scheme-object 
                                                                 scheme-object scheme-object
                                                                 scheme-object scheme-object)
                                                   scheme-object
                                                   "ffi_call_obj_with_void_pointer_6"))

(define ffi_call_obj_with_void_pointer_7 (c-lambda (void-pointer char-string 
                                                                 void-pointer scheme-object 
                                                                 scheme-object scheme-object
                                                                 scheme-object scheme-object
                                                                 scheme-object) 
                                                   scheme-object
                                                   "ffi_call_obj_with_void_pointer_7"))

(define ffi_call_obj_with_void_pointer_8 (c-lambda (void-pointer char-string 
                                                                 void-pointer scheme-object 
                                                                 scheme-object scheme-object
                                                                 scheme-object scheme-object
                                                                 scheme-object scheme-object) 
                                                   scheme-object
                                                   "ffi_call_obj_with_void_pointer_8"))

(define ffi_call_double_with_void_pointer_1 (c-lambda (void-pointer char-string void-pointer) 
                                                      double
                                                      "ffi_call_double_with_void_pointer_1"))

(define ffi_call_double_with_void_pointer_2 (c-lambda (void-pointer char-string 
                                                                    void-pointer scheme-object) 
                                                      double
                                                      "ffi_call_double_with_void_pointer_2"))

(define ffi_call_double_with_void_pointer_3 (c-lambda (void-pointer char-string 
                                                                    void-pointer scheme-object 
                                                                    scheme-object) 
                                                      double
                                                      "ffi_call_double_with_void_pointer_3"))

(define ffi_call_double_with_void_pointer_4 (c-lambda (void-pointer char-string 
                                                                    void-pointer scheme-object 
                                                                    scheme-object scheme-object)
                                                      double
                                                      "ffi_call_double_with_void_pointer_4"))

(define ffi_call_double_with_void_pointer_5 (c-lambda (void-pointer char-string 
                                                                    void-pointer scheme-object 
                                                                    scheme-object scheme-object
                                                                    scheme-object) 
                                                      double
                                                      "ffi_call_double_with_void_pointer_5"))

(define ffi_call_double_with_void_pointer_6 (c-lambda (void-pointer char-string 
                                                                    void-pointer scheme-object 
                                                                    scheme-object scheme-object
                                                                    scheme-object scheme-object)
                                                      double
                                                      "ffi_call_double_with_void_pointer_6"))

(define ffi_call_double_with_void_pointer_7 (c-lambda (void-pointer char-string 
                                                                    void-pointer scheme-object 
                                                                    scheme-object scheme-object
                                                                    scheme-object scheme-object
                                                                    scheme-object) 
                                                      double
                                                      "ffi_call_double_with_void_pointer_7"))

(define ffi_call_double_with_void_pointer_8 (c-lambda (void-pointer char-string 
                                                                    void-pointer scheme-object 
                                                                    scheme-object scheme-object
                                                                    scheme-object scheme-object
                                                                    scheme-object scheme-object) 
                                                      double
                                                      "ffi_call_double_with_void_pointer_8"))

(define ffi_call_float_with_void_pointer_1 (c-lambda (void-pointer char-string void-pointer) 
                                                     float
                                                     "ffi_call_float_with_void_pointer_1"))

(define ffi_call_float_with_void_pointer_2 (c-lambda (void-pointer char-string 
                                                                   void-pointer scheme-object) 
                                                     float
                                                     "ffi_call_float_with_void_pointer_2"))

(define ffi_call_float_with_void_pointer_3 (c-lambda (void-pointer char-string 
                                                                   void-pointer scheme-object 
                                                                   scheme-object) 
                                                     float
                                                     "ffi_call_float_with_void_pointer_3"))

(define ffi_call_float_with_void_pointer_4 (c-lambda (void-pointer char-string 
                                                                   void-pointer scheme-object 
                                                                   scheme-object scheme-object)
                                                     float
                                                     "ffi_call_float_with_void_pointer_4"))

(define ffi_call_float_with_void_pointer_5 (c-lambda (void-pointer char-string 
                                                                   void-pointer scheme-object 
                                                                   scheme-object scheme-object
                                                                   scheme-object) 
                                                     float
                                                     "ffi_call_float_with_void_pointer_5"))

(define ffi_call_float_with_void_pointer_6 (c-lambda (void-pointer char-string 
                                                                   void-pointer scheme-object 
                                                                   scheme-object scheme-object
                                                                   scheme-object scheme-object)
                                                     float
                                                     "ffi_call_float_with_void_pointer_6"))

(define ffi_call_float_with_void_pointer_7 (c-lambda (void-pointer char-string 
                                                                   void-pointer scheme-object 
                                                                   scheme-object scheme-object
                                                                   scheme-object scheme-object
                                                                   scheme-object) 
                                                     float
                                                     "ffi_call_float_with_void_pointer_7"))

(define ffi_call_float_with_void_pointer_8 (c-lambda (void-pointer char-string 
                                                                   void-pointer scheme-object 
                                                                   scheme-object scheme-object
                                                                   scheme-object scheme-object
                                                                   scheme-object scheme-object) 
                                                     float
                                                     "ffi_call_float_with_void_pointer_8"))


(define ffi_call_int_with_void_pointer_1 (c-lambda (void-pointer char-string void-pointer) 
                                                   int
                                                   "ffi_call_int_with_void_pointer_1"))

(define ffi_call_int_with_void_pointer_2 (c-lambda (void-pointer char-string 
                                                                 void-pointer scheme-object) 
                                                   int
                                                   "ffi_call_int_with_void_pointer_2"))

(define ffi_call_int_with_void_pointer_3 (c-lambda (void-pointer char-string 
                                                                 void-pointer scheme-object 
                                                                 scheme-object) 
                                                   int
                                                   "ffi_call_int_with_void_pointer_3"))

(define ffi_call_int_with_void_pointer_4 (c-lambda (void-pointer char-string 
                                                                 void-pointer scheme-object 
                                                                 scheme-object scheme-object)
                                                   int
                                                   "ffi_call_int_with_void_pointer_4"))

(define ffi_call_int_with_void_pointer_5 (c-lambda (void-pointer char-string 
                                                                 void-pointer scheme-object 
                                                                 scheme-object scheme-object
                                                                 scheme-object) 
                                                   int
                                                   "ffi_call_int_with_void_pointer_5"))

(define ffi_call_int_with_void_pointer_6 (c-lambda (void-pointer char-string 
                                                                 void-pointer scheme-object 
                                                                 scheme-object scheme-object
                                                                 scheme-object scheme-object)
                                                   int
                                                   "ffi_call_int_with_void_pointer_6"))

(define ffi_call_int_with_void_pointer_7 (c-lambda (void-pointer char-string 
                                                                 void-pointer scheme-object 
                                                                 scheme-object scheme-object
                                                                 scheme-object scheme-object
                                                                 scheme-object) 
                                                   int
                                                   "ffi_call_int_with_void_pointer_7"))

(define ffi_call_int_with_void_pointer_8 (c-lambda (void-pointer char-string 
                                                                 void-pointer scheme-object 
                                                                 scheme-object scheme-object
                                                                 scheme-object scheme-object
                                                                 scheme-object scheme-object) 
                                                   int
                                                   "ffi_call_int_with_void_pointer_8"))

(define ffi_call_int64_with_void_pointer_1 (c-lambda (void-pointer char-string void-pointer) 
                                                     int64
                                                     "ffi_call_int64_with_void_pointer_1"))

(define ffi_call_int64_with_void_pointer_2 (c-lambda (void-pointer char-string 
                                                                   void-pointer scheme-object) 
                                                     int64
                                                     "ffi_call_int64_with_void_pointer_2"))

(define ffi_call_int64_with_void_pointer_3 (c-lambda (void-pointer char-string 
                                                                   void-pointer scheme-object 
                                                                   scheme-object) 
                                                     int64
                                                     "ffi_call_int64_with_void_pointer_3"))

(define ffi_call_int64_with_void_pointer_4 (c-lambda (void-pointer char-string 
                                                                   void-pointer scheme-object 
                                                                   scheme-object scheme-object)
                                                     int64
                                                     "ffi_call_int64_with_void_pointer_4"))

(define ffi_call_int64_with_void_pointer_5 (c-lambda (void-pointer char-string 
                                                                   void-pointer scheme-object 
                                                                   scheme-object scheme-object
                                                                   scheme-object) 
                                                     int64
                                                     "ffi_call_int64_with_void_pointer_5"))

(define ffi_call_int64_with_void_pointer_6 (c-lambda (void-pointer char-string 
                                                                   void-pointer scheme-object 
                                                                   scheme-object scheme-object
                                                                   scheme-object scheme-object)
                                                     int64
                                                     "ffi_call_int64_with_void_pointer_6"))

(define ffi_call_int64_with_void_pointer_7 (c-lambda (void-pointer char-string 
                                                                   void-pointer scheme-object 
                                                                   scheme-object scheme-object
                                                                   scheme-object scheme-object
                                                                   scheme-object) 
                                                     int64
                                                     "ffi_call_int64_with_void_pointer_7"))

(define ffi_call_int64_with_void_pointer_8 (c-lambda (void-pointer char-string 
                                                                   void-pointer scheme-object 
                                                                   scheme-object scheme-object
                                                                   scheme-object scheme-object
                                                                   scheme-object scheme-object) 
                                                     int64
                                                     "ffi_call_int64_with_void_pointer_8"))


(define ffi_call_uint_with_void_pointer_1 (c-lambda (void-pointer char-string void-pointer) 
                                                    unsigned-int
                                                    "ffi_call_uint_with_void_pointer_1"))

(define ffi_call_uint_with_void_pointer_2 (c-lambda (void-pointer char-string 
                                                                  void-pointer scheme-object) 
                                                    unsigned-int
                                                    "ffi_call_uint_with_void_pointer_2"))

(define ffi_call_uint_with_void_pointer_3 (c-lambda (void-pointer char-string 
                                                                  void-pointer scheme-object 
                                                                  scheme-object) 
                                                    unsigned-int
                                                    "ffi_call_uint_with_void_pointer_3"))

(define ffi_call_uint_with_void_pointer_4 (c-lambda (void-pointer char-string 
                                                                  void-pointer scheme-object 
                                                                  scheme-object scheme-object)
                                                    unsigned-int
                                                    "ffi_call_uint_with_void_pointer_4"))

(define ffi_call_uint_with_void_pointer_5 (c-lambda (void-pointer char-string 
                                                                  void-pointer scheme-object 
                                                                  scheme-object scheme-object
                                                                  scheme-object) 
                                                    unsigned-int
                                                    "ffi_call_uint_with_void_pointer_5"))

(define ffi_call_uint_with_void_pointer_6 (c-lambda (void-pointer char-string 
                                                                  void-pointer scheme-object 
                                                                  scheme-object scheme-object
                                                                  scheme-object scheme-object)
                                                    unsigned-int
                                                    "ffi_call_uint_with_void_pointer_6"))

(define ffi_call_uint_with_void_pointer_7 (c-lambda (void-pointer char-string 
                                                                  void-pointer scheme-object 
                                                                  scheme-object scheme-object
                                                                  scheme-object scheme-object
                                                                  scheme-object) 
                                                    unsigned-int
                                                    "ffi_call_uint_with_void_pointer_7"))

(define ffi_call_uint_with_void_pointer_8 (c-lambda (void-pointer char-string 
                                                                  void-pointer scheme-object 
                                                                  scheme-object scheme-object
                                                                  scheme-object scheme-object
                                                                  scheme-object scheme-object) 
                                                    unsigned-int
                                                    "ffi_call_uint_with_void_pointer_8"))


(define ffi_call_char_string_with_void_pointer_1 (c-lambda (void-pointer char-string void-pointer) 
                                                           char-string
                                                           "ffi_call_char_string_with_void_pointer_1"))

(define ffi_call_char_string_with_void_pointer_2 (c-lambda (void-pointer char-string 
                                                                         void-pointer scheme-object) 
                                                           char-string
                                                           "ffi_call_char_string_with_void_pointer_2"))

(define ffi_call_char_string_with_void_pointer_3 (c-lambda (void-pointer char-string 
                                                                         void-pointer scheme-object 
                                                                         scheme-object) 
                                                           char-string
                                                           "ffi_call_char_string_with_void_pointer_3"))

(define ffi_call_char_string_with_void_pointer_4 (c-lambda (void-pointer char-string 
                                                                         void-pointer scheme-object 
                                                                         scheme-object scheme-object)
                                                           char-string
                                                           "ffi_call_char_string_with_void_pointer_4"))

(define ffi_call_char_string_with_void_pointer_5 (c-lambda (void-pointer char-string 
                                                                         void-pointer scheme-object 
                                                                         scheme-object scheme-object
                                                                         scheme-object) 
                                                           char-string
                                                           "ffi_call_char_string_with_void_pointer_5"))

(define ffi_call_char_string_with_void_pointer_6 (c-lambda (void-pointer char-string 
                                                                         void-pointer scheme-object 
                                                                         scheme-object scheme-object
                                                                         scheme-object scheme-object)
                                                           char-string
                                                           "ffi_call_char_string_with_void_pointer_6"))

(define ffi_call_char_string_with_void_pointer_7 (c-lambda (void-pointer char-string 
                                                                         void-pointer scheme-object 
                                                                         scheme-object scheme-object
                                                                         scheme-object scheme-object
                                                                         scheme-object) 
                                                           char-string
                                                           "ffi_call_char_string_with_void_pointer_7"))

(define ffi_call_char_string_with_void_pointer_8 (c-lambda (void-pointer char-string 
                                                                         void-pointer scheme-object 
                                                                         scheme-object scheme-object
                                                                         scheme-object scheme-object
                                                                         scheme-object scheme-object) 
                                                           char-string
                                                           "ffi_call_char_string_with_void_pointer_8"))

(c-define (___call-fn f args) (scheme-object scheme-object) scheme-object "___call_fn" ""
          (apply f args))
