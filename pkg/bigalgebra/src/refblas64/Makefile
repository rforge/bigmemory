FC = gfortran
FFLAGS = -fPIC -fdefault-integer-8 -fno-underscoring

all: compile link

compile:
	$(FC) $(FFLAGS) -c *.f

link:
	ar -rv librefblas64.a *.o
#	$(FC) $(FFLAGS) -shared -Wl,-soname,librefblas64.so -o librefblas64.so *.o

clean:
	rm -f *.o *.so
