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

c: compiles
C: DOES NOT COMPILE
r: runs
R: DOES NOT RUN

| Compiler            | list  | list_poly | file_object |
|---------------------|-------|-----------|--------------
| gfortran 4.8.5      | c,r   | c,r       |  C,R        |
| xlf2003_r 14.1.0.12 | c,r   | c,r       |             |
| gfortran 6.3        | c,r   | c,r       |  c,R        |
