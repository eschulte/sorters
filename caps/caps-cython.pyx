from cython.parallel import prange

def process(fname):
    cdef bytes l = open(fname,'r').read()
    cdef unsigned char* c_l = <unsigned char*>l
    cdef int n = len(l)
    cdef int i
    cdef int tmp_i
    cdef int tmp_j
    if 123 > c_l[0] > 96:
        c_l[0] = c_l[0] - 23
    for i in prange(n,nogil=True,num_threads=48):
        tmp_i = c_l[i]
        tmp_j = c_l[i+1]
        if (tmp_i == 10 or tmp_i == 32) and i < n-1:
            if 123 > tmp_j > 96:
                c_l[i+1] = tmp_j - 32
    return c_l
