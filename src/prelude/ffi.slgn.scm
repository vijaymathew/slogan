;; Copyright (c) 2013-2014 by Vijay Mathew Pandyalakal, All Rights Reserved.

(c-declare #<<c-declare-end
 
#include <stdio.h>
#include <stdlib.h>
#include <dlfcn.h>

 typedef void (*fn_void)();
 typedef void* (*fn_void_pointer)();
 typedef int (*fn_int)();
 typedef size_t (*fn_uint)();
 typedef ___SCMOBJ (*fn_obj)();

 static void assert_fn_pointer (void *ptr, const char *name) 
 {
   if (ptr == NULL)
     {
       printf ("failed to load function %s.\n", name);
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

 size_t ffi_call_uint_0 (void *handle, const char *fnname)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) ();
 }

 size_t ffi_call_uint_1 (void *handle, const char *fnname, 
			 ___SCMOBJ arg)
 {
   fn_uint fn = dlsym (handle, fnname);
   assert_fn_pointer (fn, fnname);
   return (*fn) (arg);
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

c-declare-end
)

(c-define-type void-pointer (pointer void))

(define ffi_open (c-lambda (char-string) void-pointer "dlopen_lazy"))
(define ffi_close (c-lambda (void-pointer) int "dlclose"))
(define ffi_call_void_0 (c-lambda (void-pointer char-string) void "ffi_call_void_0"))
(define ffi_call_void_1 (c-lambda (void-pointer char-string scheme-object) void "ffi_call_void_1"))
