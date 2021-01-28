NAME := miniweather_collapse_present
JSRUNFLAGS := -n1 -a1 -c1 -g1 --smpiargs="-disable_gpu_hooks"

include ../../Makefile.inc

# original configuration
#TEST_DEFS := -Mpreprocess -D_NX=100 -D_NZ=50 -D_SIM_TIME=400 -D_OUT_FREQ=400 -D_DATA_SPEC=DATA_SPEC_THERMAL

TEST_DEFS := -Mpreprocess -D_NX=100 -D_NZ=50 -D_SIM_TIME=40 -D_OUT_FREQ=400 -D_DATA_SPEC=DATA_SPEC_THERMAL

cpu: miniWeather_collapse_present.f90
	${MPIFC} $< ${MPIFFLAGS} ${TEST_DEFS} -o ${CPUEXE}
	#${MPIFC} $< ${MPIFACCFLAGS} -ta=multicore ${TEST_DEFS} -o ${ACCEXE}

acc: miniWeather_collapse_present.f90
	#${MPIFC} $< ${MPIFACCFLAGS} -ta=tesla:managed,cc70 ${TEST_DEFS} -o ${ACCEXE}
	${MPIFC} $< ${MPIFACCFLAGS} -ta=tesla:pinned,cc70 ${TEST_DEFS} -o ${ACCEXE}
