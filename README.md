# fortran_oo

Testing object oriented programming features in Fortran.

![GCC tests](https://github.com/pletzer/fortran_oo/actions/workflows/gnu.yml/badge.svg)
![Intel oneapi tests](https://github.com/pletzer/fortran_oo/actions/workflows/intel.yml/badge.svg)
![NVIDIA tests](https://github.com/pletzer/fortran_oo/actions/workflows/nvidia.yml/badge.svg)

## How to check out the code

```bash
git clone https://github.com/pletzer/fortran_oo.git
```

## How to build and run tests

```bash
cd fortran_oo
mkdir build
cd build
cmake ..
make
ctest
```

On AIX:

```bash
FC=xlf2003_r cmake ..
```

Using the Intel compiler:

```bash
FC=ifort cmake ..
```

