FC = gfortran
FCFLAGS = 

file_object_driver.x : file_object_driver.o test_file_object.o file_object_mod.o
	$(FC) -o $@ $?

file_object_driver.o : test_file_object.o
test_file_object.o : file_object_mod.o

%.o : %.f90
	$(FC) -c $(FCFLAGS) $<

clean :
	rm -f *.mod *.x *.o

.PHONY : clean
