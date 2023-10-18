#***********************************************************************
# Usage
#   First time, this will create directories for intermediate output
#   > make setup
#
#   Create exec
#   > make
#
#   To cleanup for a fresh compilation
#   > make clean
#***********************************************************************
#
# define executable and sourcecode macros
#
OBJ_DIR = ./obj
MOD_DIR = ./mod
FC = gfortran
#-fno-autom
#atic -fcheck=all -fcheck-array-temporaries -Wall
#-fno-automatic
# -fcheck=all -fcheck-array-temporaries-fno-automatic -fmax-stack-var-size=100000 
#FFLAGS = -O3  -mcmodel=medium -funroll-all-loops -ffast-math -msse -msse2 -m3dnow -m64
FFLAGS = -g -Wall -std=f95 -fall-intrinsics
SRC_DIRS = .

# Find all the f90 files we want to compile
# Need to compile MOD files first and in particular order
# TODO This will for for MAC and UNIX, needs tweeking for WIN
SRCS = Globals.f90 DataPoint.f90 ScallopGrowth.f90 ScallopRecruit.f90
#SRCS += $(shell find $(SRC_DIRS) -name '*.f90' | grep -v 'Globals\|DataPoint\|ScallopGrowth\|ScallopRecruit')
SRCS += ScallopPopDensity.f90 IORoutines.f90 asa239.f90

#LINUX, WIN?
#LIBS = -L/usr/lib/x86_64-linux-gnu -llapack -lblas
#MAC
#LIBS = -L/usr/lib -llapack -lblas
LIBS = 

EXEC = ScallopPopDensity

OBJ = $(SRCS:%=$(OBJ_DIR)/%.o)
OBJS = $(OBJ:.f90.o=.o)
#
# create executable
#
# The final build step.
$(EXEC): $(OBJS)
	$(FC) $(FFLAGS) $(LIBS) -I $(MOD_DIR) $(OBJS) -o $(EXEC)

# Build step for f90 source
$(OBJ_DIR)/%.o: %.f90
	$(FC) $(FFLAGS) -c $< -o $@ -J $(MOD_DIR)
#***********************************************************************
#***********************************************************************
clean:
	rm -f $(EXEC)
ifneq "$(OBJ_DIR)" ""
	rm -f $(OBJ_DIR)/*.o
	rm -f $(MOD_DIR)/*.mod
else 
	@echo "OBJ_DIR is not defined" 
endif

setup:
	if [ ! -d "$(OBJ_DIR)" ]; then mkdir $(OBJ_DIR); fi
	if [ ! -d "$(MOD_DIR)" ]; then mkdir $(MOD_DIR); fi
