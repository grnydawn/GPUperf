APP := vecadd

NCUFLAGS :=  --nvtx --force-overwrite --target-processes all --export ${APP} \
             --set=full

NSYSPROFILEFLAGS := -o ${APP} --cuda-memory-usage=true --force-overwrite=true \
			--trace=cuda,nvtx,osrt,openacc

nsight: nsys ncu
	tar -cvzhf ${APP}.tar ${APP}.qdrep ${APP}.ncu-rep *.cu

nsys: build
	nsys profile ${NSYSPROFILEFLAGS} ${APP}.exe

ncu: build
	ncu ${NCUFLAGS} ${APP}.exe

build: ${APP}.cu
	nvcc ${APP}.cu -lnvToolsExt -g -lineinfo -o ${APP}.exe 

clean:
	rm -f ${APP}.qdrep ${APP}.ncu-rep ${APP}.exe ${APP}.tar
