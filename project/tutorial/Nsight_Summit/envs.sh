#!/usr/bin/bash

module load pgi/19.9 \
			cuda/10.1.243 \
			nsight-systems/2020.5.1.85 \
			nsight-compute/2020.1.0 \
			spectrum-mpi/10.3.1.2-20200121 \
			parallel-netcdf/1.8.1

export OMP_NUM_THREADS=64
