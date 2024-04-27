import ouroboros
import time
import sys
import signal
# from multiprocessing import Pool

def main():
    thingy = 0
    def myfun(val):
        nonlocal thingy

        print(__name__, val)
        time.sleep(1)

        thingy = thingy + val

        return thingy

    print("Starting up!")
    time.sleep(2)

    result = ouroboros.mappy([1,2,3,4,5,6,7,8,9,10], lambda val: myfun(val))
    print(result)


if __name__== '__main__':
    main()
    # with Pool(5) as p:
        # result = ouroboros.mappy([1,2,3,4,5,6,7,8,9,10], lambda val: p.apply(myfun, [val]))
        # print(result)
