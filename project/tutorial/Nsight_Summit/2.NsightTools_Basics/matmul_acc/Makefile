NAME := matmul_acc
JSRUNFLAGS := -n1 -a1 -c1 -g1 --smpiargs="-disable_gpu_hooks"

include ../../Makefile.inc


cpu: matmul_acc.f90
	${FC} ${FFLAGS} -o ${CPUEXE} $<

acc: matmul_acc.f90
	${FC} ${FACCFLAGS} -o ${ACCEXE} $<
