# fortran_oo

Testing object oriented programming features in Fortran.

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

## Fortran support by compilers

| Compiler       | list  | list_poly |
|----------------|-------|-----------|
| gfortran 4.8.5 | yes   | yes       |
