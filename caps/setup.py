from distutils.core import setup
from distutils.extension import Extension
from Cython.Distutils import build_ext

ext_module = Extension(
    "caps",
    ["caps-cython.pyx"],
    extra_compile_args=['-fopenmp',"-O3"],
    extra_link_args=['-fopenmp'],
)

setup(
    name = 'Word Capitalization',
    cmdclass = {'build_ext': build_ext},
    ext_modules = [ext_module],
)
