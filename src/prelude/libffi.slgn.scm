;; Copyright (c) 2013-2016 by Vijay Mathew Pandyalakal, All Rights Reserved.
;; Linkage to libffi to provide a higher level of abstraction to C library calls.

(c-declare #<<c-declare-end

#include <stdio.h>
#include <ffi.h>
#include <stdlib.h>
#include <string.h>
#include "../include/slogan.h"

 #define SLOGAN_LIBFFI_TYPE_COUNT 22
 
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

 ffi_type *FFI_TYPE_MAP[SLOGAN_LIBFFI_TYPE_COUNT] = {
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
   ffi_type *args[SLOGAN_LIBFFI_ARGC];
   void *values[SLOGAN_LIBFFI_ARGC];
   int iargs[SLOGAN_LIBFFI_ARGC];
   long largs[SLOGAN_LIBFFI_ARGC];
   long long llargs[SLOGAN_LIBFFI_ARGC];
   unsigned int uiargs[SLOGAN_LIBFFI_ARGC];
   unsigned long ulargs[SLOGAN_LIBFFI_ARGC];
   unsigned long long ullargs[SLOGAN_LIBFFI_ARGC];   
   float fargs[SLOGAN_LIBFFI_ARGC];
   double dargs[SLOGAN_LIBFFI_ARGC];
   long double ldargs[SLOGAN_LIBFFI_ARGC];
   char *sargs[SLOGAN_LIBFFI_ARGC];
   void *pargs[SLOGAN_LIBFFI_ARGC];
   int iargs_count;
   int llargs_count;
   int largs_count;
   int uiargs_count;
   int ullargs_count;
   int ulargs_count;
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

 static ffi_type c_structs[SLOGAN_LIBFFI_STRUCT_DEFS];
 static int c_struct_count;

 static int c_struct_index(ffi_type *ft)
 {
   int i;
   for (i = 0; i < c_struct_count; ++i)
     {
       if (ft == &c_structs[i])
         return i;
     }
   return -1;
 }

 static void slogan_obj_to_c_struct(___SCMOBJ obj, void **out)
 {
   int struct_index;
   ffi_type *ft;
   ffi_type **s_type_elements;
   ffi_type *elem;
   int i;
   
   ___slogan_obj_to_int(___CAR(obj), &struct_index);
   obj = ___CDR(obj);
   ft = &c_structs[struct_index];
   s_type_elements = ft->elements;
   elem = s_type_elements[0];

   i = 0;
   while (elem != NULL)
     {
       // TODO fill *out
       obj = ___CDR(obj);
       elem = s_type_elements[++i];
     }   
 }
 
 static ___SCMOBJ c_struct_to_slogan_obj_(void **p, ffi_type *s_type, int struct_index)
 {
   ffi_type **s_type_elements = s_type->elements;
   ffi_type *elem = s_type_elements[0];
   int i = 0;
   ___SCMOBJ retval = ___NUL;
   ___SCMOBJ obj;

   while (elem != NULL)
     {          
       if (elem == &ffi_type_sint8
           || elem == &ffi_type_sint16
           || elem == &ffi_type_sint32
           || elem == &ffi_type_schar
           || elem == &ffi_type_sshort
           || elem == &ffi_type_sint)
         {
           int *si = (int *)*p;
           retval = ___pair(___fix(*si), retval);
           if (elem == &ffi_type_sint)
             *p += sizeof(int);
           else if (elem == &ffi_type_sshort)
             *p += sizeof(short);
           else if (elem == &ffi_type_schar)
             *p += sizeof(char);
           else if (elem == &ffi_type_sint32)
             *p += sizeof(int);
           else if (elem == &ffi_type_sint16)
             *p += sizeof(short);
           else if (elem == &ffi_type_sint8)
             *p += sizeof(char);
         }
       else if (elem == &ffi_type_uint8
                || elem == &ffi_type_uint16
                || elem == &ffi_type_uint32
                || elem == &ffi_type_uchar
                || elem == &ffi_type_ushort
                || elem == &ffi_type_uint)
         {
           unsigned int *ui = (unsigned int *)*p;
           ___uint_to_slogan_obj(*ui, &obj);
           retval = ___pair(obj, retval);
           if (elem == &ffi_type_uint)
             *p += sizeof(unsigned int);
           else if (elem == &ffi_type_ushort)
             *p += sizeof(unsigned short);
           else if (elem == &ffi_type_uchar)
             *p += sizeof(unsigned char);
           else if (elem == &ffi_type_uint32)
             *p += sizeof(unsigned int);
           else if (elem == &ffi_type_uint16)
             *p += sizeof(unsigned short);
           else if (elem == &ffi_type_uint8)
             *p += sizeof(unsigned char);
         }
       else if (elem == &ffi_type_slong)
         {
           long *r = (long *)*p;
           ___long_to_slogan_obj(*r, &obj);
           retval = ___pair(obj, retval);
           *p += sizeof(long);
         }
       else if (elem == &ffi_type_ulong)
         {
           unsigned long *r = (unsigned long *)*p;
           ___ulong_to_slogan_obj(*r, &obj);
           retval = ___pair(obj, retval);
           *p += sizeof(unsigned long);
         }
       else if (elem == &ffi_type_sint64)
         {
           long long *r = (long long *)*p;
           ___longlong_to_slogan_obj(*r, &obj);
           retval = ___pair(obj, retval);
           *p += sizeof(long long);
         }
       else if (elem == &ffi_type_uint64)
         {
           unsigned long long *r = (unsigned long long *)*p;
           ___ulonglong_to_slogan_obj(*r, &obj);
           retval = ___pair(obj, retval);
           *p += sizeof(unsigned long long);
         }
       else if (elem == &ffi_type_float)
         {
           float *r = (float *)*p;
           ___float_to_slogan_obj(*r, &obj);
           retval = ___pair(obj, retval);
           *p += sizeof(float);
         }
       else if (elem == &ffi_type_double)
         {
           double *r = (double *)*p;
           ___double_to_slogan_obj(*r, &obj);
           retval = ___pair(obj, retval);
           *p += sizeof(double);
         }
       else if (elem == &ffi_type_longdouble)
         {
           long double *r = (long double *)*p;
           ___double_to_slogan_obj(*r, &obj);
           retval = ___pair(obj, retval);
           *p += sizeof(long double);
         }
       else if (elem == &ffi_type_pointer)
         {
           ___void_pointer_to_slogan_obj(*p, &obj);
           retval = ___pair(obj, retval);
           *p += sizeof(void *);
         }
       else
         {
           int sindex = c_struct_index(elem);
           retval = ___pair(c_struct_to_slogan_obj_(p, elem, sindex), retval);
         }
       elem = s_type_elements[++i];
     }
   return ___pair(___fix(struct_index), retval);
 }
 
 static ___SCMOBJ c_struct_to_slogan_obj(void **p, int ret_type)
 {
   int i = ret_type - SLOGAN_LIBFFI_TYPE_COUNT;
   ffi_type *s_type = &c_structs[i];
   return c_struct_to_slogan_obj_(p, s_type, i);
 }

 struct placeholder
 {
   char c[SLOGAN_LIBFFI_STRUCT_SIZE];
 };
 
 static  ___SCMOBJ libffi_fncall(void *fn,
                                 ___SCMOBJ arg_types_vals,
                                 ___SCMOBJ i_argc,
                                 ___SCMOBJ i_ret_type)
 {
   ffi_cif cif;
   struct fncall_param fp;
   int ret_type;
   ffi_type *ret_ffi_type;
   int i;

   ___SCMOBJ retval = ___TRU;
   int dealloc_pargs[SLOGAN_LIBFFI_ARGC];
   int has_allocated_pargs = 0;
   
   for (i = 0; i < SLOGAN_LIBFFI_ARGC; ++i)
     dealloc_pargs[i] = 0;
   
   fpinit(&fp);
   ___slogan_obj_to_int(i_argc, &fp.argc);

   for (i = 0; i < fp.argc; ++i)
     {
       int it;
       ___SCMOBJ arg = ___CAR(arg_types_vals);
   
       ___slogan_obj_to_int(___CAR(arg), &it);
       if (it >= SLOGAN_LIBFFI_TYPE_COUNT) /* A user defined struct */
         {
           it -= SLOGAN_LIBFFI_TYPE_COUNT;
           fp.args[i] = &c_structs[it];
           fp.pargs[fp.pargs_count] = (char *)malloc(SLOGAN_LIBFFI_STRUCT_SIZE);
           if (fp.pargs[fp.pargs_count] == NULL)
             {
               fprintf(stderr, "failed to allocate memory for user struct object");
               return ___FAL;
             }
           slogan_obj_to_c_struct(___CDR(arg), &fp.pargs[fp.pargs_count]);
           dealloc_pargs[fp.pargs_count] = 1;
           ++has_allocated_pargs;
           fp.values[i] = &fp.pargs[fp.pargs_count++];
         }
       else
         {
           fp.args[i] = FFI_TYPE_MAP[it];
           if (fp.args[i] == &ffi_type_sint
               || fp.args[i] == &ffi_type_schar
               || fp.args[i] == &ffi_type_sshort
               || fp.args[i] == &ffi_type_sint8
               || fp.args[i] == &ffi_type_sint16
               || fp.args[i] == &ffi_type_sint32)
             {
               ___slogan_obj_to_int(___CDR(arg), &fp.iargs[fp.iargs_count]);
               fp.values[i] = &fp.iargs[fp.iargs_count++];
             }
           else if (fp.args[i] == &ffi_type_uint
                    || fp.args[i] == &ffi_type_uchar
                    || fp.args[i] == &ffi_type_ushort
                    || fp.args[i] == &ffi_type_uint8
                    || fp.args[i] == &ffi_type_uint16
                    || fp.args[i] == &ffi_type_uint32)
             {
               ___slogan_obj_to_uint(___CDR(arg), &fp.uiargs[fp.uiargs_count]);
               fp.values[i] = &fp.uiargs[fp.uiargs_count++];
             }
           else if (fp.args[i] == &ffi_type_slong)
             {
               long r;
               ___slogan_obj_to_long(___CDR(arg), &r);
               fp.largs[fp.largs_count] = r;
               fp.values[i] = &fp.largs[fp.largs_count++];
             }
           else if (fp.args[i] == &ffi_type_ulong)
             {
               unsigned long r;
               ___slogan_obj_to_ulong(___CDR(arg), &r);
               fp.ulargs[fp.ulargs_count] = r;
               fp.values[i] = &fp.ulargs[fp.ulargs_count++];
             }
           else if (fp.args[i] == &ffi_type_sint64)
             {
               ___slogan_obj_to_longlong(___CDR(arg), &fp.llargs[fp.llargs_count]);
               fp.values[i] = &fp.llargs[fp.llargs_count++];
             }
           else if (fp.args[i] == &ffi_type_uint64)
             {
               ___slogan_obj_to_ulonglong(___CDR(arg), &fp.ullargs[fp.ullargs_count]);
               fp.values[i] = &fp.ullargs[fp.ullargs_count++];
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
               if (it == libffi_type_charstr)
                 {
                   ___slogan_obj_to_charstring(___CDR(arg), &fp.sargs[fp.sargs_count]);
                   fp.values[i] = &fp.sargs[fp.sargs_count++];
                 }
               else
                 {
                   ___slogan_obj_to_void_pointer(___CDR(arg), &fp.pargs[fp.pargs_count]);
                   fp.values[i] = &fp.pargs[fp.pargs_count++];
                 }
             }
         }
       arg_types_vals = ___CDR(arg_types_vals);
     }

   ___slogan_obj_to_int(i_ret_type, &ret_type);
   if (ret_type >= SLOGAN_LIBFFI_TYPE_COUNT)
     ret_ffi_type = &c_structs[ret_type - SLOGAN_LIBFFI_TYPE_COUNT];
   else ret_ffi_type = FFI_TYPE_MAP[ret_type];

   if (ffi_prep_cif(&cif, FFI_DEFAULT_ABI, fp.argc,
                    ret_ffi_type, fp.args) == FFI_OK)
     {
       if (ret_type <= libffi_type_sint)
         {
           if (ret_type == libffi_type_sint
               || ret_type == libffi_type_schar
               || ret_type == libffi_type_sshort
               || ret_type == libffi_type_sint8
               || ret_type == libffi_type_sint16
               || ret_type == libffi_type_sint32)
             {
               int rc;
               ffi_call(&cif, fn, &rc, fp.values);
               retval =  ___fix(rc);
             }
           else
             {
               unsigned int rc;
               ffi_call(&cif, fn, &rc, fp.values);
               ___uint_to_slogan_obj(rc, &retval);
             }
         }
       else if (ret_type == libffi_type_slong)
         {
           long rc;
           ffi_call(&cif, fn, &rc, fp.values);
           ___long_to_slogan_obj(rc, &retval);
         }
       else if (ret_type == libffi_type_ulong)
         {
           unsigned long rc;
           ffi_call(&cif, fn, &rc, fp.values);
           ___ulong_to_slogan_obj(rc, &retval);
         }       
       else if (ret_type == libffi_type_sint64)
         {
           long long rc;
           ffi_call(&cif, fn, &rc, fp.values);
           ___longlong_to_slogan_obj(rc, &retval);
         }
       else if (ret_type == libffi_type_uint64)
         {
           unsigned long long rc;
           ffi_call(&cif, fn, &rc, fp.values);
           ___ulonglong_to_slogan_obj(rc, &retval);
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
           if (ret_type >= SLOGAN_LIBFFI_TYPE_COUNT)
             {
               struct placeholder pc;
               ffi_call(&cif, fn, &pc, fp.values);
               rc = &pc;
               retval = c_struct_to_slogan_obj(&rc, ret_type);
             }
           else
             {
               ffi_call(&cif, fn, &rc, fp.values);
               ___void_pointer_to_slogan_obj(rc, &retval);
             }
         }
     }
   else retval = ___FAL;
   if (has_allocated_pargs > 0)
     {
       for (i = 0; i < SLOGAN_LIBFFI_ARGC; ++i)
         {
           if (dealloc_pargs[i] == 1)
             {
               free(fp.pargs[i]);
               --has_allocated_pargs;
             }
           if (has_allocated_pargs <= 0) break;
         }
     }
   return retval;
 }

static  ___SCMOBJ libffi_defstruct(___SCMOBJ memtypes, ___SCMOBJ i_memcount)
 {
   if (c_struct_count >= SLOGAN_LIBFFI_STRUCT_DEFS)
     {
       fprintf(stderr, "user struct definitions limit exceeded - %d", c_struct_count);
       return ___FAL;
     }
   ffi_type s_type;
   ffi_type **s_type_elements = NULL;
   int memcount;
   int i;

   ___slogan_obj_to_int(i_memcount, &memcount);
   s_type_elements = (ffi_type**)malloc(sizeof(ffi_type*) * (memcount + 1));    /* this is never freed */
   s_type.size = s_type.alignment = 0;
   s_type.type = FFI_TYPE_STRUCT;
   s_type.elements = s_type_elements;
     
   for (i = 0; i < memcount; i++)
     {
       int it;
       ___slogan_obj_to_int(___CAR(memtypes), &it);
       if (it >= SLOGAN_LIBFFI_TYPE_COUNT) /* A user defined struct */
         {
           it -= SLOGAN_LIBFFI_TYPE_COUNT;
           s_type_elements[i] = &c_structs[it];
         }
       else s_type_elements[i] = FFI_TYPE_MAP[it];
       memtypes = ___CDR(memtypes);
     }
   s_type_elements[memcount] = NULL;
   c_structs[c_struct_count] = s_type;
   ++c_struct_count;
   return ___fix((c_struct_count + SLOGAN_LIBFFI_TYPE_COUNT) - 1);
 }
 
c-declare-end
)

(c-define-type void-pointer (pointer void))

(define libffi-fncall (c-lambda (void-pointer scheme-object scheme-object scheme-object)
                                scheme-object
                                "libffi_fncall"))
(define libffi-defstruct (c-lambda (scheme-object scheme-object)
                                   scheme-object
                                   "libffi_defstruct"))

(define *libffi-types*
  '((uint8 . 0) (int8 . 1)
    (uint16 . 2) (int16 . 3)
    (uint32 . 4) (int32 . 5)
    (uchar . 6) (char . 7)
    (ushort . 8) (short . 9)
    (uint . 10) (int . 11)
    (ulong . 12) (long . 13)
    (uint64 . 14) (int64 . 15)
    (float . 16) (double . 17)
    (longdouble . 18) (charstring . 19)
    (pointer . 20) (void . 21)))

(define (libffitype->int type)
  (let ((t (assq type *libffi-types*)))
    (if t (cdr t)
        (error "Invalid c type - " type))))

(define (def-c-struct name memtypes)
  (let ((sid (libffi-defstruct (map libffitype->int memtypes)
                               (length memtypes))))
    (cond (sid
           (set! *libffi-types* (cons (cons name sid) *libffi-types*))
           name)
          (else (error "Failed to define c structure - " name)))))

(define (mk-c-fn-param-names plen)
  (let loop ((i 0) (pnames '()))
    (if (< i plen)
        (loop (+ i 1) (cons (string->symbol
                             (string-append "p" (number->string i)))
                            pnames))
        (reverse pnames))))

(define (mk-c-fn-args ptypes pnames)
  (let loop ((ptypes ptypes)
             (pnames pnames)
             (expr '(list)))
    (if (null? ptypes)
        expr
        (loop (cdr ptypes) (cdr pnames)
              (append expr (list `(cons (libffitype->int ',(car ptypes)) ,(car pnames))))))))
       
(define (def-c-fn libhandle c-fn-name name paramtypes rettype)
  (let ((plen (length paramtypes)))
    (let ((pnames (mk-c-fn-param-names plen)))
      `(define ,name (let ((fhandle (ffi_fn ,libhandle ,(symbol->string c-fn-name))))
                       (lambda ,pnames
                         (libffi-fncall fhandle ,(mk-c-fn-args paramtypes pnames)
                                        ,plen (libffitype->int ',rettype))))))))
