;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.

(c-declare #<<c-declare-end
 
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <dlfcn.h>
#include "../include/slogan.h"

 typedef void (*fn_void)(___SCMOBJ*);
 typedef void* (*fn_void_pointer)(___SCMOBJ*);
 typedef char* (*fn_char_string)(___SCMOBJ*);
 typedef int (*fn_int)(___SCMOBJ*);
#ifdef INT64_MAX
 typedef int64_t (*fn_int64)(___SCMOBJ*);
#else
 typedef int (*fn_int64)(___SCMOBJ*);
 typedef int int64_t;
#endif
 typedef unsigned int (*fn_uint)(___SCMOBJ*);
 typedef float (*fn_float)(___SCMOBJ*);
 typedef double (*fn_double)(___SCMOBJ*);
 typedef ___SCMOBJ (*fn_obj)(___SCMOBJ*);

 typedef void (*fn_voidp_void)(void*, ___SCMOBJ*);
 typedef void* (*fn_voidp_void_pointer)(void*, ___SCMOBJ*);
 typedef char* (*fn_voidp_char_string)(void*, ___SCMOBJ*);
 typedef int (*fn_voidp_int)(void*, ___SCMOBJ*);
#ifdef INT64_MAX
 typedef int64_t (*fn_voidp_int64)(void*, ___SCMOBJ*);
#else
 typedef int (*fn_voidp_int64)(void*, ___SCMOBJ*);
 typedef int int64_t;
#endif
 typedef unsigned int (*fn_voidp_uint)(void*, ___SCMOBJ*);
 typedef float (*fn_voidp_float)(void*, ___SCMOBJ*);
 typedef double (*fn_voidp_double)(void*, ___SCMOBJ*);
 typedef ___SCMOBJ (*fn_voidp_obj)(void*, ___SCMOBJ*);

 #define SCHEME_LIBRARY_LINKER ____20_ffi_2e_slgn
 
 ___BEGIN_C_LINKAGE
 extern ___mod_or_lnk SCHEME_LIBRARY_LINKER (___global_state);
 ___END_C_LINKAGE

 void ___setup_fn_call()
 {
   ___setup_params_struct setup_params;
   
   ___setup_params_reset(&setup_params);
   
   setup_params.version = ___VERSION;
   setup_params.linker  = SCHEME_LIBRARY_LINKER;
   
   ___setup(&setup_params);
 }

 static void assert_fn_pointer(void *ptr, const char *name) 
 {
   if (ptr == NULL)
     {
       fprintf(stderr, "failed to load function %s.\n", name);
       exit(1);
     }
 }

 static void *dlopen_lazy(const char *libname)
 {
   return dlopen(libname, RTLD_LAZY);
 }
 
 static void ffi_call_void(void *fnptr,  ___SCMOBJ args)
 {
   fn_void fn = (fn_void)fnptr;
   fn(___body(args));
 }

 static void *ffi_call_void_pointer(void *fnptr, ___SCMOBJ args)
 {
   fn_void_pointer fn = (fn_void_pointer)fnptr;
   return fn(___body(args));
 }

 static void *ffi_call_char_string(void *fnptr, ___SCMOBJ args)
 {
   fn_char_string fn = (fn_char_string)fnptr;
   return fn(___body(args));
 }

 static int ffi_call_int(void *fnptr, ___SCMOBJ args)
 {
   fn_int fn = (fn_int)fnptr;
   return fn(___body(args));
 }

 static int64_t ffi_call_int64(void *fnptr, ___SCMOBJ args)
 {
   fn_int64 fn = (fn_int64)fnptr;
   return fn(___body(args));
 }

 static unsigned int ffi_call_uint(void *fnptr, ___SCMOBJ args)
 {
   fn_uint fn = (fn_uint)fnptr;
   return fn(___body(args));
 }

 static float ffi_call_float(void *fnptr, ___SCMOBJ args)
 {
   fn_float fn = (fn_float)fnptr;
   return fn(___body(args));   
 }

 static double ffi_call_double(void *fnptr, ___SCMOBJ args)
 {
   fn_double fn = (fn_double)fnptr;
   return fn(___body(args));      
 }

 static ___SCMOBJ ffi_call_obj(void *fnptr, ___SCMOBJ args)
 {
   fn_obj fn = (fn_obj)fnptr;
   return fn(___body(args));
 }

 static void ffi_call_with_void_pointer(void *fnptr, void *argp, ___SCMOBJ args)
 {
   fn_voidp_void fn = (fn_voidp_void)fnptr;
   return fn(argp, ___body(args));
 }

 static void *ffi_call_void_pointer_with_void_pointer(void *fnptr, void *argp, ___SCMOBJ args)
 {
   fn_voidp_void_pointer fn = (fn_voidp_void_pointer)fnptr;
   return fn(argp, ___body(args));
 }

 static int ffi_call_int_with_void_pointer(void *fnptr, void *argp, ___SCMOBJ args)
 {
   fn_voidp_int fn = (fn_voidp_int)fnptr;
   return fn(argp, ___body(args));
 }

 static double ffi_call_double_with_void_pointer(void *fnptr, void *argp, ___SCMOBJ args)
 {
   fn_voidp_double fn = (fn_voidp_double)fnptr;
   return fn(argp, ___body(args));
 }
 
 static float ffi_call_float_with_void_pointer(void *fnptr, void *argp, ___SCMOBJ args)
 {
   fn_voidp_float fn = (fn_voidp_float)fnptr;
   return fn(argp, ___body(args));   
 }

 static int64_t ffi_call_int64_with_void_pointer(void *fnptr, void *argp, ___SCMOBJ args)
 {
   fn_voidp_int64 fn = (fn_voidp_int64)fnptr;
   return fn(argp, ___body(args));    
 }
 
 static unsigned int ffi_call_uint_with_void_pointer(void *fnptr, void *argp, ___SCMOBJ args)
 {
   fn_voidp_uint fn = (fn_voidp_uint)fnptr;
   return fn(argp, ___body(args));   
 }
 
 static void *ffi_call_char_string_with_void_pointer(void *fnptr, void *argp, ___SCMOBJ args)
 {
   fn_voidp_void_pointer fn = (fn_voidp_void_pointer)fnptr;
   return fn(argp, ___body(args));
 }

 static ___SCMOBJ ffi_call_obj_with_void_pointer(void *fnptr, void *argp, ___SCMOBJ args)
 {
   fn_voidp_obj fn = (fn_voidp_obj)fnptr;
   return fn(argp, ___body(args));
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
      (let ((ld-paths (getenv "LD_LIBRARY_PATH"))) ;; DYLD_LIBRARY_PATH in Darwin??
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

(define ffi_call_void (c-lambda (void-pointer scheme-object) 
                                void 
                                "ffi_call_void"))

(define ffi_call_void_pointer (c-lambda (void-pointer scheme-object) 
                                        void-pointer 
                                        "ffi_call_void_pointer"))

(define ffi_call_char_string (c-lambda (void-pointer scheme-object) 
                                       char-string
                                       "ffi_call_char_string"))

(define ffi_call_int (c-lambda (void-pointer scheme-object) 
                               int 
                               "ffi_call_int"))

(define ffi_call_int64 (c-lambda (void-pointer scheme-object) 
                                 int64 
                                 "ffi_call_int64"))

(define ffi_call_uint (c-lambda (void-pointer scheme-object) 
                                unsigned-int 
                                "ffi_call_uint"))

(define ffi_call_float (c-lambda (void-pointer scheme-object) 
                                   float 
                                   "ffi_call_float"))

(define ffi_call_double (c-lambda (void-pointer scheme-object) 
                                  double 
                                  "ffi_call_double"))

(define ffi_call_obj (c-lambda (void-pointer scheme-object) 
                               scheme-object 
                               "ffi_call_obj"))

(define ffi_call_void_with_void_pointer (c-lambda (void-pointer void-pointer scheme-object) 
                                                  void
                                                  "ffi_call_with_void_pointer"))

(define ffi_call_void_pointer_with_void_pointer (c-lambda (void-pointer void-pointer scheme-object) 
                                                          void-pointer
                                                          "ffi_call_void_pointer_with_void_pointer"))

(define ffi_call_obj_with_void_pointer (c-lambda (void-pointer void-pointer scheme-object) 
                                                 scheme-object
                                                 "ffi_call_obj_with_void_pointer"))

(define ffi_call_double_with_void_pointer (c-lambda (void-pointer void-pointer scheme-object) 
                                                    double
                                                    "ffi_call_double_with_void_pointer"))

(define ffi_call_float_with_void_pointer (c-lambda (void-pointer void-pointer scheme-object) 
                                                   float
                                                   "ffi_call_float_with_void_pointer"))

(define ffi_call_int_with_void_pointer (c-lambda (void-pointer void-pointer scheme-object) 
                                                 int
                                                 "ffi_call_int_with_void_pointer"))

(define ffi_call_int64_with_void_pointer (c-lambda (void-pointer void-pointer scheme-object) 
                                                   int64
                                                   "ffi_call_int64_with_void_pointer"))

(define ffi_call_uint_with_void_pointer (c-lambda (void-pointer void-pointer scheme-object) 
                                                  unsigned-int
                                                  "ffi_call_uint_with_void_pointer"))

(define ffi_call_char_string_with_void_pointer (c-lambda (void-pointer void-pointer scheme-object) 
                                                         char-string
                                                         "ffi_call_char_string_with_void_pointer"))

(c-define (___call-fn f args) (scheme-object scheme-object) scheme-object "___call_fn" ""
          (apply f args))
