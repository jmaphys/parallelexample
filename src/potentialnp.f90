program potentialnp
    use, intrinsic :: iso_fortran_env, only:  dp=>real64, isp=>int64
    use functions
    implicit none

    integer :: i, j
    integer :: np
    integer :: ierr

    integer, dimension(1):: semilla

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

    if (np .gt. 2000) then
        print*, "np debe ser menor de 2000."
        print*, "Programa finalizado"
    end if

    write(*,*) np

    allocate(rvec(np, 3), stat=ierr)
    allocate(vij(np,np), stat=ierr)
    n=0
    semilla(1)=1234
    call random_seed(size=n)
    call random_seed(put=semilla)

    do i=1, np
        do j=1, 3
            call random_number(randnum)
            rvec(i,j)=randnum*5
        end do
    end do

    do i=1,np
        write(40,'(3f8.3)') rvec
    end do

    stop

!    open(200, file="indata.txt", status="old")
!
!    do i=1, np
!        read(200, '(3f8.3)') rvec
!    end do


    vijsum=0.
    call cpu_time(timeini)
    !$acc data copyin(rvec(:,:), vij(:,:)) copyout(vij(:,:))
    !$acc parallel loop private(rijvec) reduction(+:vijsum)
    do i=1, np
        do j=i+1, np
!            rivec=rvec(i,:)
!            rjvec=rvec(j,:)
            rijvec=rvec(i,:) - rvec(j,:)
            r2ij=dot_product(rijvec, rijvec)
            rij=sqrt(r2ij)
            vij(i,j)=rij*rij !potential(rij)
            vijsum=vijsum+vij(i,j)
        end do
    end do
    !$acc end parallel
    !$acc end data
    call cpu_time(timefin)
    !do i=1, np
    !    do j=i+1, np
    !        write(*, '(I4,2X, I4, f12.6)') i, j, vij(i,j)
    !    end do
    !end do
    write(*,*) "Vijsum =", vijsum
    write(*,*) "Tiempo ejecución = ", timefin-timeini, " s para ", np, " partículas."

end program potentialnp
