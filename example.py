import ouroboros
import time
from multiprocessing import Pool

def myfun(val):
    print(__name__, val)
    time.sleep(0.5)
    return val * val * val

if __name__== '__main__':
    with Pool(5) as p:
        # result = ouroboros.mappy([1,2,3,4,5,6,7,8,9,10], lambda val: p.apply(myfun, [val]))
        # print(result)
         result = ouroboros.mappy([1,2,3,4,5,6,7,8,9,10], lambda val: myfun(val))
         print(result)

