NAME := vecadd_acc
#JSRUNFLAGS := -n1 -a1 -c1 -g1 --smpiargs="-disable_gpu_hooks"
JSRUNFLAGS := -n1 -a1 -c32 -g1 --smpiargs="-disable_gpu_hooks"

include ../../Makefile.inc


cpu: vecadd.f90
	${FC} ${FFLAGS} -o ${CPUEXE} $<

acc: vecadd.f90
	${FC} ${FACCFLAGS} -o ${ACCEXE} $<
