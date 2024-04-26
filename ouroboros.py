import ctypes as _ctypes
import atexit as _atexit

# Load the dynamic library
_dll = _ctypes.CDLL("Ouroboros.so")

# Register function signatures
_dll.example.restype = _ctypes.c_char_p
_dll.example.argtypes = [_ctypes.c_char_p]

def example(string):
  input = string.encode()
  output = _dll.example(input)
  return output.decode()
