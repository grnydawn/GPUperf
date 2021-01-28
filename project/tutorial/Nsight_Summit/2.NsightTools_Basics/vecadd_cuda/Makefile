NAME := vecadd_cuda_nvtx

JSRUNFLAGS := -n1 -a1 -c1 -g1 --smpiargs="-disable_gpu_hooks"

include ../../Makefile.inc

cpu:
	@echo "echo no CPU code" > ${CPUEXE} ; chmod +x ${CPUEXE}

acc: cuda

cuda: vecadd_nvtx.cu
	nvcc -o ${ACCEXE} -lnvToolsExt -g -lineinfo $<
