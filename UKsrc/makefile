#***********************************************************************
#***********************************************************************
#
# define executable and sourcecode macros
#
OBJ_DIR = ./obj
MOD_DIR = ./mod
FC = gfortran
FFLAGS = -g -Wall -std=f95 -fall-intrinsics
# -O3  -mcmodel=medium -funroll-all-loops -ffast-math -msse -msse2 -m3dnow -m64
 # -g debug
#-O3  -mcmodel=medium -funroll-all-loops -ffast-math -msse -msse2 -m3dnow -m64

SRCS = UK_Globals.f90 UK_GridManager.f90 LinearSpatialFcn.f90 NonLinearSpatialFcn.f90 RandomField.f90 KrigingRoutines.f90 UniversalKriging.f90 UK_IORoutines.f90

ifeq ($(OS), Windows_NT)
#LIBS = -L/usr/lib/x86_64-linux-gnu -llapack -lblas
LIBS = -L..\Libs -llapack -lblas
else # MAC, UNIX
LIBS = -L/usr/lib -llapack -lblas
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
else
	rm -fr  $(EXEC)
	ifneq "$(OBJ_DIR)", ""
		rm -fr $(OBJ_DIR)/*.o
		rm -fr $(MOD_DIR)/*.mod
	else 
		@echo "OBJ_DIR is not defined" 
	endif
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