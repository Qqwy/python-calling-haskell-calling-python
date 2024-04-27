import ctypes as _ctypes
import atexit as _atexit
import signal
import time
import errno as _errno

# Load the dynamic library. This will initialize the Haskell runtime system.
_dll = _ctypes.CDLL("Ouroboros.so")

# # The Haskell RTS overrides the `SIGINT` signal handler
# # but we want Python code to be able to react to it gracefully.
# # So, re-register it: 
# def signal_handler(sig, frame):
#     # print('You pressed Ctrl+C!')
#     raise KeyboardInterrupt

# signal.signal(signal.SIGINT, signal_handler)

# Register function signatures
_dll.example.argtypes = [_ctypes.c_char_p]
_dll.example.restype = _ctypes.c_char_p
def example(string):
  input = string.encode()
  output = _dll.example(input)
  return output.decode()

class IntPair(_ctypes.Structure):
    _fields_ = [("x", _ctypes.c_int),
                ("y", _ctypes.c_int)]

_IntToIntFunction = _ctypes.CFUNCTYPE(_ctypes.c_int, _ctypes.c_int, use_errno=True)
_dll.mappy.argtypes = [_ctypes.POINTER(_ctypes.c_int), _IntToIntFunction]
_dll.mappy.restype = _ctypes.POINTER(_ctypes.c_int)
def mappy(elems, fun):
  try:
    time.sleep(2)
    # NOTE: For now expect a 0-terminated array
    # Passing in the size separately would be preferable
    ArrType = _ctypes.c_int * (len(elems) + 1)
    arr = ArrType(*elems, 0)

    # NOTE: Callback functions ought to be wrapped to turn exceptions into values
    # While 'ctypes' ignores and logs exceptions, in that case 
    # nothing is written to the return value memory so Haskell receives garbage.
    def fun_wrapper(val):
      try:
        return fun(val)
        # return IntPair(0, fun(val))
      except KeyboardInterrupt as e:
        print("KeyboardInterrupt received on the inside!")
        _ctypes.set_errno(_errno.EINTR)
        # return IntPair(1, 0)
        return 0

    output = _dll.mappy(arr, _IntToIntFunction(fun_wrapper))
    if _ctypes.get_errno() == _errno.EINTR:
      raise KeyboardInterrupt
    return output[0:len(elems)]
  except KeyboardInterrupt as e:
    print("KeyboardInterrupt received on the outside")
    raise e

