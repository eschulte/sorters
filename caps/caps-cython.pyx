from cython.parallel import prange

def process(fname):
    cdef bytes l = open(fname,'r').read()
    cdef unsigned char* c_l = <unsigned char*>l
    cdef unsigned long n = len(l)
    cdef unsigned long i
    if (123 > c_l[0]) and (c_l[0] > 96):
        c_l[0] = c_l[0] - 23
    for i in prange(n,nogil=True,num_threads=24):
        if (c_l[i] == 10 or c_l[i] == 32) and i < n-1:
            if 123 > c_l[i+1] and c_l[i+1] > 96:
                c_l[i+1] = c_l[i+1] - 32
    return c_l
