#***********************************************************************
#***********************************************************************
#
# define executable and sourcecode macros
#
OBJ_DIR = ./obj
MOD_DIR = ./mod
FC = gfortran
# For release use these flags
# Expect this warning
# Warning: 'ceps.dim[1].stride' may be used uninitialized in this function
# See:
# https://gcc.gnu.org/bugzilla/show_bug.cgi?id=105937
# ... generates bogus warnings if compiled with -Wall AND -O1(or higher optimizations).
# Fixed in version 12.2.1
# This is written with currently using: GNU Fortran (GCC) 11.3.0
# -mcmodel=medium -msse -msse2 -m3dnow not supported on MAC
#FFLAGS = -Wall -std=f95 -fall-intrinsics -O3  -mcmodel=medium -funroll-all-loops -ffast-math -msse -msse2 -m3dnow -m64
FFLAGS = -Wall -std=f95 -fall-intrinsics -O3  -funroll-all-loops -ffast-math -m64

# For debug use these flags
#FFLAGS = -g -Wall -std=f95 -fall-intrinsics -fbounds-check

#NORF#SRCS = Globals.f90 UK_GridManager.f90 LinearSpatialFcn.f90 NonLinearSpatialFcn.f90 RandomField.f90 KrigingRoutines.f90 UniversalKriging.f90 IORoutines.f90
SRCS = Globals.f90 UK_GridManager.f90 LinearSpatialFcn.f90 NonLinearSpatialFcn.f90 KrigingRoutines.f90 UniversalKriging.f90 IORoutines.f90

ifeq ($(OS), Windows_NT)
# Now Only RandomField.f90 uses lapack and blas, for dpotrf and dgemm. The latter could also be replaced with a matmul, former search for code
#LIBS = -L..\Libs -llapack -lblas
else # MAC, UNIX
#LIBS = -L/usr/lib -llapack -lblas
endif
EXEC = UK

#-fno-autom
# ifneq ($(OS), Windows_NT)
# atic -fcheck=all -fcheck-array-temporaries -Wall
# endif
# #-fno-automatic
# -fcheck=all -fcheck-array-temporaries-fno-automatic -fmax-stack-var-size=100000 
#FFLAGS = -O3  -mcmodel=medium -funroll-all-loops -ffast-math -msse -msse2 -m3dnow -m64


OBJ = $(SRCS:%=$(OBJ_DIR)/%.o)
OBJS = $(OBJ:.f90.o=.o)
#
# create executable
#
# The final build step.
$(EXEC): $(OBJS)
	$(FC) $(FFLAGS) -I $(MOD_DIR) $(OBJS) $(LIBS) -o $(EXEC)

# Build step for f90 source
$(OBJ_DIR)/%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@ -J $(MOD_DIR)
#***********************************************************************
#***********************************************************************
clean:
ifeq ($(OS), Windows_NT)
	del  $(EXEC).exe
	del .\obj\*.o
	del .\mod\*.mod
	attrib -R IORoutines.f90
	attrib -R Globals.f90
	copy ..\SRC\IORoutines.f90 .
	copy ..\SRC\Globals.f90 .
# prevent modification these file in this directory
	attrib +R .\IORoutines.f90
	attrib +R Globals.f90
else
	rm -fr  $(EXEC)
	rm -fr $(OBJ_DIR)/*.o
	rm -fr $(MOD_DIR)/*.mod
	chmod 660 IORoutines.f90
	chmod 660 Globals.f90
	cp ../SRC/IORoutines.f90 .
	cp ../SRC/Globals.f90 .
# prevent modification these file in this directory
	chmod 440 IORoutines.f90
	chmod 440 Globals.f90
endif

setup:
ifeq ($(OS), Windows_NT)
ifeq ("$(wildcard .\obj)", "")
	mkdir .\obj
endif
ifeq ("$(wildcard .\mod)", "")
	mkdir .\mod
endif
else
	if [ ! -d "$(OBJ_DIR)" ]; then mkdir $(OBJ_DIR); fi
	if [ ! -d "$(MOD_DIR)" ]; then mkdir $(MOD_DIR); fi
endif