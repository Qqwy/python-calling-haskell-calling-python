import ctypes as _ctypes
import atexit as _atexit
import signal
import time
import errno as _errno

# Load the dynamic library. This will initialize the Haskell runtime system.
_dll = _ctypes.CDLL("Ouroboros.so")

# Register function signatures
_dll.example.argtypes = [_ctypes.c_char_p]
_dll.example.restype = _ctypes.c_char_p
def example(string):
  input = string.encode()
  output = _dll.example(input)
  return output.decode()

_dll.haskellRealloc.argtypes = [_ctypes.c_void_p, _ctypes.c_uint]
_dll.haskellRealloc.restype = _ctypes.c_void_p
def haskellRealloc(ptr, size):
  return _ctypes.cast(_dll.haskellRealloc(ptr, size), _ctypes.c_void_p)

def haskellMalloc(size):
  return haskellRealloc(None, size)

def haskellFree(ptr):
  haskellRealloc(ptr, 0)
  return None

def haskellBytestring(bytestring):
  ptr = _ctypes.cast(haskellMalloc(len(bytestring)), _ctypes.POINTER(_ctypes.c_char))
  _ctypes.memmove(ptr, bytestring, len(bytestring))
  return ptr

def CleverVec(elemType, *elemVals):
  VLA = elemType * len(elemVals)

  def __init__(self, *elemVals):
    self.size = len(elemVals)
    self.elems = VLA(*elemVals)
  
  def __len__(self):
    return self.size

  def __getitem__(self, key):
    return self.elems.__getitem__(key)
  klass = type(
    f"CleverVec[{elemType.__name__}]", 
    (_ctypes.Structure,),
    dict(
      _fields_= [('size', _ctypes.c_uint64), ('elems', VLA)],
      __init__ = __init__,
      __len__ = __len__,
      __getitem__ = __getitem__
    )
  )

  return klass(*elemVals)


# An array with a known size, stored as (size, elem*)-pair in memory.
def SizePrefixedArray(elemType):
  def __init__(self, *vals):
    size = len(vals)
    ArrType = elemType * size # Allocate large enough empty array
    self.elems = ArrType(*vals) # Fill array and convert into pointer
    self.size = size
  
  def __len__(self):
    return self.size

  def __getitem__(self, key):
    return self.elems[0:self.size].__getitem__(key)

  return type(
      f"SizePrefixedArray[{elemType.__name__}]", 
      (_ctypes.Structure,),
     {'_fields_':
          [("size", _ctypes.c_uint64), 
          ("elems", _ctypes.POINTER(elemType))]
      ,
      '__init__': __init__,
      '__len__': __len__,
      '__getitem__': __getitem__
      }
    )

def ResultTuple(resultType):
  return type(
      f"ResultTuple[{resultType.__name__}]", 
      (_ctypes.Structure,), 
     {'_fields_': 
          [("error", _ctypes.c_int), # NOTE: To be replaced by a bespoke error type later
          ("result", resultType)]
      }
    )

_UIntArray = SizePrefixedArray(_ctypes.c_int)
_IntToIntFunction = _ctypes.CFUNCTYPE(None, _ctypes.c_int, _ctypes.POINTER(ResultTuple(_ctypes.c_int)))
_dll.mappy.argtypes = [_ctypes.POINTER(_UIntArray), _IntToIntFunction]
_dll.mappy.restype = _ctypes.POINTER(_UIntArray)
def mappy(elems, fun):
  try:
    time.sleep(2)
    # NOTE: For now expect a 0-terminated array
    # Passing in the size separately would be preferable
    # ArrType = _ctypes.c_int * (len(elems) + 1)
    # arr = ArrType(*elems, 0)
    arr = _UIntArray(*elems)
    print(arr)
    print(list(arr))

    # NOTE: Callback functions ought to be wrapped to turn exceptions into values
    # While 'ctypes' ignores and logs exceptions, in that case 
    # nothing is written to the return value memory so Haskell receives garbage.
    def fun_wrapper(val, outParam):
      try:
        res = fun(val)
        outParam.contents.result = res
        outParam.contents.error = False
      except KeyboardInterrupt as e:
        print("KeyboardInterrupt received on the inside!")
        outParam.contents.error = True
        # outparam.contents.result remains unset

    output = _dll.mappy(arr, _IntToIntFunction(fun_wrapper)).contents
    # print(arr)
    return output[0:len(elems)]
  except KeyboardInterrupt as e:
    print("KeyboardInterrupt received on the outside")
    raise e

