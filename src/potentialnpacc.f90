program potentialnp
    use, intrinsic :: iso_fortran_env, only:  dp=>real64, isp=>int64
    use functions
    implicit none

    integer :: i, j
    integer :: np
    integer :: ierr

    integer, dimension(:), allocatable:: semilla

    integer :: n

    character(len=2) :: npchar

    real(dp), dimension(:,:), allocatable :: rvec
    real(dp), dimension(3) :: rivec, rjvec
    real(dp), dimension(3) :: rijvec

    real(dp) :: r2ij, rij

    real(dp), dimension(:,:), allocatable :: vij

    real(dp) :: randnum

    real(dp) :: timeini, timefin

    real(dp) :: vijsum


    read(*,*) np

    write(*,*) np

    allocate(rvec(np, 3), stat=ierr)
    allocate(vij(np,np), stat=ierr)

    call random_seed(size=n)
    allocate(semilla(n), stat=ierr)
    semilla=1234
    call random_seed(PUT=semilla)



    do i=1, np
        do j=1, 3
            call random_number(randnum)
            rvec(i,j)=randnum*5._dp
        end do
    end do



    call cpu_time(timeini)
    !$acc data copyin(rvec(:,:)) copyout(vij(:,:))
    !$acc parallel loop private(rijvec)
    do i=1, np
        do j=i+1, np
            rivec=rvec(i,:)
            rjvec=rvec(j,:)
            rijvec=rvec(i,:) - rvec(j,:)
            r2ij=dot_product(rijvec, rijvec)
            rij=sqrt(r2ij)
            vij(i,j)=potential(rij)
!            vijsum=vijsum+vij(i,j)
        end do
    end do
    !$acc end parallel
    !$acc end data
    call cpu_time(timefin)
    write(*,*) "Vij(1,np) =", vij(1,np)
    write(*,*) "Tiempo ejecución = ", timefin-timeini, " s para ", np, " partículas."

end program potentialnp
