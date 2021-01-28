PROGRAM matrixMultiply

    use nvtx

    IMPLICIT NONE

    INTEGER, PARAMETER :: N = 2 ** 7

    REAL(KIND=8), ALLOCATABLE, DIMENSION(:, :) :: A, B, C
    REAL(KIND=8) :: tmp
    INTEGER :: i, j, k

    call nvtxStartRange("MAIN")

    allocate (A(N, N), B(N, N), C(N, N))

    CALL RANDOM_NUMBER(A)
    CALL RANDOM_NUMBER(B)

    call nvtxStartRange("ACC INIT")
    !$acc init
    call nvtxEndRange


    call nvtxStartRange("WARMUP")
    !$acc parallel
    !$acc loop
    DO i=1, N
        DO j=1, N
            tmp = 0.0_8
            DO k=1, N
                tmp = tmp + A(i, k) * B(k, j)
            END DO
            C(i, j) = tmp
        END DO
    END DO
    !$acc end parallel
    call nvtxEndRange

    call verify(A, B, C, N)

    call nvtxStartRange("ver1: gang-vector, none, none")
    !$acc parallel
    !$acc loop gang vector
    DO i=1, N
        DO j=1, N
            tmp = 0.0_8
            DO k=1, N
                tmp = tmp + A(i, k) * B(k, j)
            END DO
            C(i, j) = tmp
        END DO
    END DO
    !$acc end parallel
    call nvtxEndRange

    call verify(A, B, C, N)

    call nvtxStartRange("ver2: gang, none, vector")
    !$acc parallel
    !$acc loop gang
    DO i=1, N
        DO j=1, N
            tmp = 0.0_8
            !$acc loop vector
            DO k=1, N
                tmp = tmp + A(i, k) * B(k, j)
            END DO
            C(i, j) = tmp
        END DO
    END DO
    !$acc end parallel
    call nvtxEndRange

    call verify(A, B, C, N)

    call nvtxStartRange("ver3: gang(collapse), none, vector")
    !$acc parallel
    !$acc loop gang collapse(2)
    DO i=1, N
        DO j=1, N
            tmp = 0.0_8
            !$acc loop vector
            DO k=1, N
                tmp = tmp + A(i, k) * B(k, j)
            END DO
            C(i, j) = tmp
        END DO
    END DO
    !$acc end parallel
    call nvtxEndRange

    call verify(A, B, C, N)

    call nvtxStartRange("ver4: gang(collapse) independent, none, vector")
    !$acc parallel
    !$acc loop gang collapse(2) independent
    DO i=1, N
        DO j=1, N
            tmp = 0.0_8
            !$acc loop vector
            DO k=1, N
                tmp = tmp + A(i, k) * B(k, j)
            END DO
            C(i, j) = tmp
        END DO
    END DO
    !$acc end parallel
    call nvtxEndRange

    call verify(A, B, C, N)

    call nvtxStartRange("ver5: gang vector tile(8, 8), none, none")
    !$acc parallel
    !$acc loop gang vector tile(8, 8)
    DO i=1, N
        DO j=1, N
            tmp = 0.0_8
            DO k=1, N
                tmp = tmp + A(i, k) * B(k, j)
            END DO
            C(i, j) = tmp
        END DO
    END DO
    !$acc end parallel
    call nvtxEndRange

    call verify(A, B, C, N)

    call nvtxStartRange("ver6: gang vector independent tile(8, 8), none, none")
    !$acc parallel
    !$acc loop gang vector independent tile(8,8)
    DO i=1, N
        DO j=1, N
            tmp = 0.0_8
            DO k=1, N
                tmp = tmp + A(i, k) * B(k, j)
            END DO
            C(i, j) = tmp
        END DO
    END DO
    !$acc end parallel
    call nvtxEndRange

    call verify(A, B, C, N)

    deallocate (A, B, C)

    call nvtxEndRange

CONTAINS

    SUBROUTINE verify(A, B, C, N)
        REAL(KIND=8), ALLOCATABLE, INTENT(IN), DIMENSION(:, :) :: A, B, C
        INTEGER, INTENT(IN) :: N
        REAL(KIND=8) :: tmp
        INTEGER :: i, j, k

        DO i=1, N
            DO j=1, N
                tmp = 0.0_8
                DO k=1, N
                    tmp = tmp + A(i, k) * B(k, j)
                END DO

                IF ( ABS(C(i, j) - tmp) .GT. 1.0E-13_8 ) THEN
                    PRINT *, C(i, j), ".NE.", tmp
                    STOP
                END IF
            END DO
        END DO

        print *, "Verified"

    END SUBROUTINE

END PROGRAM
