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
	nvcc -o ${APP}.exe ${APP}.cu

cleanall: clean
	rm -f ${APP}.qdrep ${APP}.ncu-rep

clean:
	rm -f ${APP}.exe ${APP}.tar
