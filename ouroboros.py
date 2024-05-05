import ctypes as _ctypes
import atexit as _atexit
import signal
import time
import errno as _errno
from typing import Any

# Load the dynamic library. This will initialize the Haskell runtime system.
_dll = _ctypes.CDLL("Ouroboros.dylib")

_dll.haskellRealloc.argtypes = [_ctypes.c_void_p, _ctypes.c_uint]
_dll.haskellRealloc.restype = _ctypes.c_void_p
def haskellRealloc(ptr, size):
  return _ctypes.cast(_dll.haskellRealloc(ptr, size), _ctypes.c_void_p)

def haskellMalloc(size):
  return haskellRealloc(None, size)

def haskellFree(ptr):
  haskellRealloc(ptr, 0)
  return None

def haskellMallocBytesCopy(string):
  if isinstance(string, bytes):
    bytestring = string
  elif isinstance(string, str):
    bytestring = string.encode()

  ptr = _ctypes.cast(haskellMalloc(len(bytestring)), _ctypes.c_char_p)
  _ctypes.memmove(ptr, bytestring, len(bytestring))
  return ptr

class ByteBox(_ctypes.Structure):
  _fields_ = [('elems', _ctypes.c_char_p), ("size", _ctypes.c_uint64)]
  def __init__(self, string = None):
    if string != None:
      self.fill_with(string)

  def fill_with(self, string):
    if isinstance(string, bytes) or isinstance(string, ByteBox):
      bytestring = bytes(string)
    elif isinstance(string, str):
      bytestring = string.encode()
    else:
      raise Exception("Cannot convert input object {string} to str or bytes")
    haskellFree(self.elems)
    self.elems = haskellMallocBytesCopy(bytestring)
    self.size = len(bytestring)

  def __len__(self):
    return self.size

  def __getitem__(self, key):
    return self.elems[0:self.size].__getitem__(key)
  def __bytes__(self):
    return self.elems[0:self.size]
  def __str__(self):
    return bytes(self).decode()

class HaskellException(Exception):
  def __init__(self, name, message, callstack, annotations = []):
    traceback = haskellCallstackToPythonTraceback(callstack)
    if traceback is not None:
      self.__traceback__ = traceback
    for annotation in annotations:
      self.add_note(annotation)
    super().__init__(name, message)

def haskellCallstackToPythonTraceback(callstack):
  # print(callstack)
  if callstack is None:
    return None
  import tblib
  tb_next = None
  for name, info in callstack:
    code = {'co_filename': info['file'], 'co_name': name}
    frame = {'f_lineno': info['line'], 'f_code': code, 'f_globals': {}}
    current = {'tb_frame': frame, 'tb_lineno': info['line'], 'tb_next': tb_next}
    tb_next = current
  if tb_next is not None:
    return tblib.Traceback.from_dict(tb_next).as_traceback()
  else:
    return None

def raiseHaskellExceptionAsPythonException(name, message, callstack, annotations = []):
  res = None
  # Async exceptions
  if name == "UserInterrupt":
    res = KeyboardInterrupt
  if name == "StackOverflow":
    res = RecursionError(message)
  if name == "HeapOverflow":
    res = MemoryError(message)
  if name == "ThreadKilled": # NOTE: This is not 1:1 the same, but the closest
    res = SystemExit(message)
  # Sync exceptions
  if name == "ArithException" and message == "divide by zero":
    res = ZeroDivisionError
  if name == "ArithException":
    res = ArithmeticError(message)
  # Haskell could not parse input passed to it:
  if name == "InputParseException":
    res = ValueError(message)
  
  if res is None:
    raise HaskellException(name, message, callstack, annotations)
  else:
    context = HaskellException(name, message, callstack, annotations)
    raise res from context

def infernoFun(name):
  lowerFun = getattr(_dll, name)
  lowerFun.argtypes = [_ctypes.POINTER(ByteBox), _ctypes.POINTER(ByteBox)]
  lowerFun.restype = None

  def fun(inBytes: bytes) -> bytes:
    inBox = ByteBox(inBytes)
    outBox = ByteBox()
    lowerFun(inBox, outBox)
    outBytes = bytes(outBox)
    return outBytes

  return fun

def jsonFun(name):
  lowerFun = infernoFun(name)
  def fun(*params: tuple) -> Any:
    import json
    inStr = json.dumps(params)
    outStr = lowerFun(inStr)
    outObj = json.loads(outStr)
    return outObj
  
  return fun

def throwingFun(name):
  lowerFun = jsonFun(name)
  def fun(*params: tuple) -> Any:
    outObject = lowerFun(*params)
    if 'Right' in outObject:
      return outObject['Right']
    elif 'Left' in outObject and 'name' in outObject['Left'] and 'message' in outObject['Left']:
      error = outObject['Left']
      raiseHaskellExceptionAsPythonException(error['name'], error['message'], error['callstack'], error['annotations'])
    else:
      raise Exception(f"JSON in unexpected format returned from Haskell FFI call: {outObject}")
  
  return fun
    
haskellDivImpl = throwingFun('haskellDiv')
def haskellDiv(num: int, denom: int) -> int:
  return haskellDivImpl(num, denom)


# Old examples:

def pythonFunToHaskellFun(fun):
  def wrapped_fun(in_ptr, out_ptr): 
    input = bytes(in_ptr.contents)
    try:
      output = fun(input)
      succeeded = True
    except:
      import sys
      output = repr(sys.exception()).encode()
      succeeded = False
    # NOTE: `ptr.contents = ByteBox(output)` does not work 
    # as (perhaps surprisingly) that changes where the pointer points 
    # rather than what it points to
    out_ptr.contents.fill_with(output) 
    return succeeded

  return _PurgatoryFun(wrapped_fun)


_PurgatoryFun = _ctypes.CFUNCTYPE(_ctypes.c_bool, _ctypes.POINTER(ByteBox), _ctypes.POINTER(ByteBox))
_dll.runpython.argtypes = [_PurgatoryFun]
_dll.runpython.restype = _ctypes.c_bool
def runpython(fun):
  succeeded = _dll.runpython(pythonFunToHaskellFun(fun))
  if succeeded:
    # print("Python: Haskell succeeded")
    None
  else:
    print("Python: Haskell threw an error, rethrowing")
    raise KeyboardInterrupt

_dll.runpython2.argtypes = [_PurgatoryFun]
_dll.runpython2.restype = None
def runpython2(fun):
  _dll.runpython2(pythonFunToHaskellFun(fun))

_dll.appendMessage.argtypes = [_ctypes.POINTER(ByteBox), _ctypes.POINTER(ByteBox)]
_dll.appendMessage.restype = None
def appendMessage(string):
  inBox = ByteBox(string)
  outBox = ByteBox()
  _dll.appendMessage(inBox, outBox)
  return bytes(outBox)

_dll.printJSON.argtypes = [_ctypes.POINTER(ByteBox), _ctypes.POINTER(ByteBox)]
_dll.printJSON.restype = None
def printJSON(inObject):
  import json
  inStr = json.dumps(inObject)
  inBox = ByteBox(inStr)
  outBox = ByteBox()
  _dll.printJSON(inBox, outBox)
  outStr = bytes(outBox)
  outObject = json.loads(outStr)
  return outObject


_dll.example.argtypes = [_ctypes.c_char_p]
_dll.example.restype = _ctypes.c_char_p
def example(string):
  input = string.encode()
  output = _dll.example(input)
  return output.decode()


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
_dll.mappy.argtypes = [_ctypes.c_void_p, _IntToIntFunction]
_dll.mappy.restype = _ctypes.c_void_p
def mappy(elems, fun):
  try:
    time.sleep(2)
    # NOTE: For now expect a 0-terminated array
    # Passing in the size separately would be preferable
    # ArrType = _ctypes.c_int * (len(elems) + 1)
    # arr = ArrType(*elems, 0)
    # arr = _UIntArray(len(elems), *elems)
    # print(arr)
    # print(list(arr))
    arr = CleverVec(_ctypes.c_int, *elems)

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

    output = _dll.mappy(_ctypes.pointer(arr), _IntToIntFunction(fun_wrapper))
    output = _ctypes.c_void_p(output)
    # print(arr)
    return output[0:len(elems)]
  except KeyboardInterrupt as e:
    print("KeyboardInterrupt received on the outside")
    raise e
