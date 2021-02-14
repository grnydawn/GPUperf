# Predefined rules for Nsight tutorial on ORNL Summit system

ifndef NAME
    NAME := app
endif

ifdef JSRUNFLAGS
    JSRUN := jsrun ${JSRUNFLAGS} -- 
endif

CPUEXE := ${NAME}.cpu.exe
ACCEXE := ${NAME}.acc.exe

CC := pgcc
FC := pgfortran
OMP := -mp
DEBUG := -Minfo=accel,loop -g -Mcuda='lineinfo' ../../nvtx.f90 -L${CUDA_DIR}/lib64 -lnvToolsExt
CACCFLAGS := -acc ${DEBUG}
CFLAGS := ${DEBUG} ${OMP}
FACCFLAGS := -acc ${DEBUG}
FFLAGS := ${DEBUG} ${OMP}

PFFLAGS :=
PFACCFLAGS :=

NCUFLAGS :=  --nvtx --force-overwrite --target-processes all --export ${NAME} \
             --set=full -c 150

NSYSPROFILEFLAGS := -o ${NAME} --cuda-memory-usage=true --force-overwrite=true \
			--trace=cuda,nvtx,osrt,openacc

MPICC := mpicc
MPICXX := mpicxx
MPIFC := mpif90

MPICFLAGS := -I${OLCF_PARALLEL_NETCDF_ROOT}/include ${OLCF_PARALLEL_NETCDF_ROOT}/lib/libpnetcdf.a ${CFLAGS}
MPIFACCFLAGS := -I${OLCF_PARALLEL_NETCDF_ROOT}/include ${OLCF_PARALLEL_NETCDF_ROOT}/lib/libpnetcdf.a ${FACCFLAGS}
MPIFFLAGS := -I${OLCF_PARALLEL_NETCDF_ROOT}/include ${OLCF_PARALLEL_NETCDF_ROOT}/lib/libpnetcdf.a ${FFLAGS}

help:
	@echo "######################################"
	@echo "#    Makefile for OpenAcc Tutorial   #"
	@echo "######################################"
	@echo ""
	@echo "Usage: make <nsight|pgprof|cpu|acc|tar|clean|*help>"
	@echo ""

banner1:
	@echo "############################"
	@echo "# Compiling program        #"
	@echo "############################"
	@echo ""

pgprof: banner1 cpu acc
	@echo "############################"
	@echo "# Running CPU program      #"
	@echo "############################"
	@echo ""
	@${JSRUN}pgprof ${PFFLAGS} ./${CPUEXE}
	@echo ""
	@echo "############################"
	@echo "# Running OpenACC program  #"
	@echo "############################"
	@echo ""
	@${JSRUN}pgprof ${PFACCFLAGS} ./${ACCEXE}

nsight: banner1 cpu acc
	@echo "############################"
	@echo "# Running nsys             #"
	@echo "############################"
	@echo ""
	${JSRUN}nsys profile ${NSYSPROFILEFLAGS} ./${ACCEXE}
	@echo ""
	@echo "############################"
	@echo "# Running ncu              #"
	@echo "############################"
	@echo ""
	${JSRUN}ncu ${NCUFLAGS} ./${ACCEXE}

run: runcpu runacc

runcpu: cpu
	@echo "############################"
	@echo "# Running CPU              #"
	@echo "############################"
	@echo ""
	${JSRUN}./${CPUEXE}

runacc: acc
	@echo "############################"
	@echo "# Running ACC              #"
	@echo "############################"
	@echo ""
	${JSRUN}./${ACCEXE}

tar:
	tar --ignore-failed-read -cvzhf ${NAME}.tar ${NAME}.qdrep ${NAME}.ncu-rep *.f90 *.cu *.F90 *.c *.h 2>/dev/null

clean:
	rm -f *.o *.mod *.exe *.tar *.qdrep *.ncu-rep *.nc
