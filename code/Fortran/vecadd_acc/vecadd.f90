

PROGRAM vectorAdd

    use nvtx

    IMPLICIT NONE

    INTEGER, PARAMETER :: N = 2 ** 20

    REAL(KIND=8), ALLOCATABLE, DIMENSION(:) :: a, b, c
    INTEGER :: i

    call nvtxStartRange("vectorAdd")

    allocate (a(N), b(N), c(N))

    CALL RANDOM_NUMBER(a)
    CALL RANDOM_NUMBER(b)

    call nvtxStartRange("INIT ACC")
    !$acc init
    call nvtxEndRange

    call nvtxStartRange("ACC PARALLEL 1")
    !$acc parallel
    !$acc loop gang vector
    DO i=1, N
        c(i) = a(i) + b(i)
    END DO
    !$acc end parallel
    call nvtxEndRange

    call nvtxStartRange("ACC PARALLEL 2")
    !$acc parallel
    !$acc loop gang vector
    DO i=1, N
        c(i) = a(i) + b(i)
    END DO
    !$acc end parallel
    call nvtxEndRange
 
    call nvtxStartRange("ACC PARALLEL 3")
    !$acc parallel num_gangs(1024)
    !$acc loop gang vector
    DO i=1, N
        c(i) = a(i) + b(i)
    END DO
    !$acc end parallel
    call nvtxEndRange
  
    print *, "SUM(C) = ", SUM(C)

    deallocate (a, b, c)

    call nvtxEndRange

END PROGRAM
