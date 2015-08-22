#include "slogan.h"

___slogan_obj ___alloc_u8array(size_t size)
{
  ___slogan_obj result = (___slogan_obj)___EXT(___alloc_scmobj)(___PSTATE, ___sU8VECTOR, size);
  ___EXT(___release_scmobj)(result);
  return result;
}
