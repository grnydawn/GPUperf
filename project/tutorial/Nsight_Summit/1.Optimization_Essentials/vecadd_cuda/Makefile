NAME := vecadd_cuda

JSRUNFLAGS := -n1 -a1 -c1 -g1 --smpiargs="-disable_gpu_hooks"

include ../../Makefile.inc

cpu:
	@echo "echo no CPU code" > ${CPUEXE} ; chmod +x ${CPUEXE}

acc: cuda

cuda: vecadd.cu
	nvcc -o ${ACCEXE} $<
