# import ctypes as _ctypes
# import atexit as _atexit

import weakref
# # Load the dynamic library
# _dll = _ctypes.CDLL("Ouroboros.so")

# # Register function signatures
# _dll.example.argtypes = [_ctypes.c_char_p]
# _dll.example.restype = _ctypes.c_char_p
# def example(string):
#   input = string.encode()
#   output = _dll.example(input)
#   return output.decode()

# _IntToIntFunction = _ctypes.CFUNCTYPE(_ctypes.c_int, _ctypes.c_int)
# _dll.mappy.argtypes = [_ctypes.POINTER(_ctypes.c_int), _IntToIntFunction]
# _dll.mappy.restype = _ctypes.POINTER(_ctypes.c_int)
# def mappy(elems, fun):
#   # NOTE: For now expect a 0-terminated array
#   # Passing in the size separately would be preferable
#   ArrType = _ctypes.c_int * (len(elems) + 1)
#   arr = ArrType(*elems, 0)

#   # NOTE: Callback functions ought to be wrapped to turn exceptions into values
#   # While 'ctypes' ignores and logs exceptions, in that case 
#   # nothing is written to the return value memory so Haskell receives garbage.

#   output = _dll.mappy(arr, _IntToIntFunction(fun))
#   return output[0:len(elems)]

from cffi import FFI
_ffi = FFI()
_ffi.cdef("""
  char* example(char* str);

  typedef struct {
      int x;
      int y;
  } intpair;

  int* mappy(int*, intpair *(*)(int));
""")
_dll = _ffi.dlopen("Ouroboros.so")

def example(string):
  input = string.encode()
  output = _dll.example(input)
  return _ffi.string(output).decode()

_weakkeydict = weakref.WeakKeyDictionary()


def mappy(elems, fun):
  def fun_inner(val):
    res = fun(val)
    print(res)
    output = _ffi.new("intpair*", res)
    _weakkeydict[fun] = output
    print(output)
    return output
  array = _ffi.new("int[]", elems + [0])
  funptr = _ffi.callback("intpair*(int)", fun_inner)
  output = _dll.mappy(array, funptr)
  return list(output[0:len(elems)])
