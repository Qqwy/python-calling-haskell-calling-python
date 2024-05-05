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
