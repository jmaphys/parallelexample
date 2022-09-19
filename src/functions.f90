module functions
    use, intrinsic :: iso_fortran_env, only:  dp=>real64, isp=>int64
    implicit none

contains

    real(dp) function potential(rij)
        real(dp) :: rij
        potential= rij*rij
    end function

end module functions
