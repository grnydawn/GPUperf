! User defined parameters
#ifndef _NX
    #define _NX      4
#endif
#ifndef _NELEM
    #define _NELEM   6*120*120
#endif
#ifndef _NIT
    #define _NIT     10
#endif

!!------------------------------------------------
 !R.Nair NCAR/scd 08/03
 !Qudrature of the RHS evaluation 
!!------------------------------------------------

      PROGRAM Grad_Term_GPU
      
      IMPLICIT NONE

      INTEGER, PARAMETER :: DOUBLE=SELECTED_REAL_KIND(p=14,r=100)

      INTEGER, PARAMETER :: nx=_NX      ! element order
      INTEGER, PARAMETER :: npts=nx*nx
      INTEGER, PARAMETER :: nit=_NIT   ! iteration count
      INTEGER, PARAMETER :: nelem=_NELEM

      REAL(KIND=DOUBLE), PARAMETER :: dt=.005D0 ! fake timestep

      REAL(KIND=DOUBLE) :: der(nx,nx)   ! Derivative matrix
      REAL(KIND=DOUBLE) :: delta(nx,nx) ! Kronecker delta function
      REAL(KIND=DOUBLE) :: gw(nx)       ! Gaussian wts
      REAL(KIND=DOUBLE), DIMENSION(nx*nx,nelem) :: flx,fly
      REAL(KIND=DOUBLE), DIMENSION(nx*nx,nelem) :: grad     

      REAL(KIND=DOUBLE) :: s1, s2
      REAL(KIND=DOUBLE) :: start_time, stop_time, elapsed_time

      INTEGER :: i, j, k, l, ii, ie, it, begin, end, rate

      ! Init static matrices
      call system_clock(begin, rate)

      der(:,:)=1.0_8
      gw(:) = 0.5_8

      delta(:,:)=0.0_8
      delta(1,1)=1.0_8
      delta(2,2)=1.0_8

      ! Load up some initial values

      flx(:,:) = 1.0_8
      fly(:,:) = -1.0_8

      DO it=1,nit
      !$omp parallel private(ie, ii, k, l, s2, j, s1, i)
      !$& shared(nelem, npts, nx, delta, flx, der, fly, gw, dt, grad)
      !$omp do
      DO ie=1,nelem
         DO ii=1,npts
            k=MODULO(ii-1,nx)+1
            l=(ii-1)/nx+1
            s2 = 0.0_8
            DO j = 1, nx
               s1 = 0.0_8
               DO i = 1, nx
                  s1 = s1 + (delta(l,j)*flx(i+(j-1)*nx,ie)*der(i,k) + &
                             delta(i,k)*fly(i+(j-1)*nx,ie)*der(j,l))*gw(i)
               END DO  ! i loop
               s2 = s2 + s1*gw(j) 
            END DO ! j loop
            grad(ii,ie) = s2
         END DO ! ii loop
      END DO ! ie

     !write(*,*) "Done with gradient"

      !$omp do
      DO ie=1,nelem
         DO ii=1,npts
            flx(ii,ie) = flx(ii,ie)+ dt*grad(ii,ie)
            fly(ii,ie) = fly(ii,ie)+ dt*grad(ii,ie)
         END DO
      END DO
      !$omp end parallel
      
      END DO ! iteration count, it

      call system_clock(end)
      elapsed_time = real(end - begin) / real(rate)

      WRITE(*, *) "################################################################"
      WRITE(*, *) "#                          RESLT                               #"
      WRITE(*, *) "################################################################"
      WRITE(*, *)
      WRITE(*, "(A,I2,A,I10,A,I8)")  "   NX = ",_NX,", NELEM = ",_NELEM, &
            ", NIT = ", nit
      WRITE(*, "(A,E15.7)") "   MAX(flx) : expected = 0.1050566E+01, result = ", &
            MAXVAL(flx)
      WRITE(*, "(A,E15.7)") "   MIN(fly) : expected = -0.1050566E+01 result = ", &
            MINVAL(fly)
      WRITE(*, "(A,I10)") "   Original # calculations : ", nit*nelem*npts*(nx*nx*7+2*nx+4)
      WRITE(*, "(A)") "   Original elapsed time (sec) on Summit : 0.2620629966"
      WRITE(*, "(A,F14.10)") "   Current elapsed time (sec) : ", elapsed_time
      !WRITE(*, "(A,F14.10,A)") '   Completed in ', elapsed_time, ' seconds'
      WRITE(*, *)
      WRITE(*, *) "################################################################"

      END PROGRAM Grad_Term_GPU
