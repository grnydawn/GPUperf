NAME := miniweather_ver0_managed
#JSRUNFLAGS := -n1 -a1 -c32 -g1 --smpiargs="-disable_gpu_hooks"
JSRUNFLAGS := -n1 -g1 -c42 --smpiargs="-disable_gpu_hooks" -brs

include ../../Makefile.inc

# original configuration
#TEST_DEFS := -Mpreprocess -D_NX=100 -D_NZ=50 -D_SIM_TIME=400 -D_OUT_FREQ=400 -D_DATA_SPEC=DATA_SPEC_THERMAL

TEST_DEFS := -Mpreprocess -D_NX=100 -D_NZ=50 -D_SIM_TIME=40 -D_OUT_FREQ=400 -D_DATA_SPEC=DATA_SPEC_THERMAL

cpu: miniWeather_ver0_managed.f90
	${MPIFC} $< ${MPIFFLAGS} ${TEST_DEFS} -o ${CPUEXE}
	#${MPIFC} $< ${MPIFACCFLAGS} -ta=multicore ${TEST_DEFS} -o ${ACCEXE}

acc: miniWeather_ver0_managed.f90
	${MPIFC} $< ${MPIFACCFLAGS} -ta=tesla:managed,cc70 ${TEST_DEFS} -o ${ACCEXE}
