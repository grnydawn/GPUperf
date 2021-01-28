#include <stdio.h>
#include <nvToolsExt.h>

// Size of array
#define N (1 << 20)

// Kernel
__global__ void add_vectors(double *a, double *b, double *c)
{
	int id = blockDim.x * blockIdx.x + threadIdx.x;
	if(id < N)
		c[id] = a[id] + b[id];
}

// Main program
int main()
{
    nvtxRangePush("main");

	// Number of bytes to allocate for N doubles
	size_t bytes = N*sizeof(double);

	// Allocate memory for arrays A, B, and C on host
	double *A = (double*)malloc(bytes);
	double *B = (double*)malloc(bytes);
	double *C = (double*)malloc(bytes);

	// Allocate memory for arrays d_A, d_B, and d_C on device
    nvtxRangePush("cudaMalloc");
	double *d_A, *d_B, *d_C;
	cudaMalloc(&d_A, bytes);
	cudaMalloc(&d_B, bytes);
	cudaMalloc(&d_C, bytes);
    nvtxRangePop();

	// Fill host arrays A and B
	for(int i=0; i<N; i++)
	{
		A[i] = (double) rand();
		B[i] = (double) rand();
	}

	// Copy data from host arrays A and B to device arrays d_A and d_B
    nvtxRangePush("cudaMemcpy H-to-D");
	cudaMemcpy(d_A, A, bytes, cudaMemcpyHostToDevice);
	cudaMemcpy(d_B, B, bytes, cudaMemcpyHostToDevice);
    nvtxRangePop();

	// Set execution configuration parameters
	//		thr_per_blk: number of CUDA threads per grid block
	//		blk_in_grid: number of blocks in grid
	int thr_per_blk = 256;
	int blk_in_grid = ceil( float(N) / thr_per_blk );

	// Launch kernel
    nvtxRangePush("kernel launch");
	add_vectors<<< blk_in_grid, thr_per_blk >>>(d_A, d_B, d_C);
    nvtxRangePop();

	// Copy data from device array d_C to host array C
    nvtxRangePush("cudaMemcpy D-to-H");
	cudaMemcpy(C, d_C, bytes, cudaMemcpyDeviceToHost);
    nvtxRangePop();

	// Verify results
	for(int i=0; i<N; i++)
	{
		if(C[i] != A[i]+B[i])
		{ 
			printf("\nError: value of C[%d] = %f instead of %f\n\n", i, C[i], A[i]+B[i]);
			exit(-1);
		}
	}	

	// Free CPU memory
	free(A);
	free(B);
	free(C);

	// Free GPU memory
    nvtxRangePush("cudaFree");
	cudaFree(d_A);
	cudaFree(d_B);
	cudaFree(d_C);
    nvtxRangePop();

	printf("\n---------------------------\n");
	printf("__SUCCESS__\n");
	printf("---------------------------\n");
	printf("N                 = %d\n", N);
	printf("Threads Per Block = %d\n", thr_per_blk);
	printf("Blocks In Grid    = %d\n", blk_in_grid);
	printf("---------------------------\n\n");

    nvtxRangePop();

	return 0;
}
