NAME := dg
#JSRUNFLAGS := -n1 -a1 -c1 -g1 --smpiargs="-disable_gpu_hooks"
JSRUNFLAGS := -n1 -g1 -c42 --smpiargs="-disable_gpu_hooks" -brs

include ../../Makefile.inc

cpu: dg_acc.F90
	${FC} $< ${FFLAGS} -o ${CPUEXE}

acc: dg_acc.F90
	${FC} $< ${FACCFLAGS} -o ${ACCEXE}
