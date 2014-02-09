;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(c-declare #<<c-declare-end
 
#include <dlfcn.h>

 typedef void (*fn_void)();
 typedef void* (*fn_void_pointer)();
 typedef int (*fn_int)();
 typedef size_t (*fn_uint)();

 void *dlopen_lazy (const char *libname)
 {
   return dlopen (libname, RTLD_LAZY);
 }

 void ffi_call_void_0 (void *handle, const char *fnname)
 {
   fn_void fn = dlsym (handle, fnname);
   if (fn) (*fn) ();
 }

 void ffi_call_void_1 (void *handle, const char *fnname, 
		       ___SCMOBJ arg)
 {
   fn_void fn = dlsym (handle, fnname);
   if (fn) (*fn) (arg);
 }

c-declare-end
)

(c-define-type void-pointer (pointer void))

(define dlopen_lazy (c-lambda (char-string) void-pointer "dlopen_lazy"))
(define dlclose (c-lambda (void-pointer) int "dlclose"))
(define ffi_call_void_0 (c-lambda (void-pointer char-string) void "ffi_call_void_0"))
(define ffi_call_void_1 (c-lambda (void-pointer char-string scheme-object) void "ffi_call_void_1"))
