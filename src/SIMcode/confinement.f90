logical function check_if_inside(RP, NT, CONFINEMENT, CONFP1, CONFP2)

    INTEGER, intent(in) :: NT,CONFINEMENT
    DOUBLE PRECISION, intent(in) :: RP(NT,3)
    DOUBLE PRECISION, intent(in) :: CONFP1, CONFP2
    LOGICAL is_outside
    INTEGER i

    check_if_inside = .TRUE.
    is_outside = .FALSE.
    do i=1,NT
        select case (CONFINEMENT)
            case (0)
                is_outside = .FALSE.
            case (1)
                is_outside = check_outside_sphere(RP(i,:), CONFP1)
            case (2)
                is_outside = check_outside_cell(RP(i,:), CONFP1, CONFP2)
        end select
        if (is_outside) then
            is_inside = 0
            exit
        endif
    enddo
end function

! function get_beads_outside(RP, NT, CONFINEMENT, CONFP1, CONFP2)

!     INTEGER, intent(in) :: NT,CONFINEMENT
!     DOUBLE PRECISION, intent(in) :: RP(NT,3)
!     DOUBLE PRECISION, intent(in) :: CONFP1, CONFP2
!     INTEGER i
!     INTEGER, intent(out) :: get_beads_outside(NT)

!     do i=1,NT
!         get_beads_outside = check_single_confinement(RP(i,:), CONFINEMENT, CONFP1, CONFP2)
!     enddo
! end function

! function check_single_confinement(
! end function

! inside sphere of radius R centered at the origin
logical function check_outside_sphere(Ri, R)
    DOUBLE PRECISION, intent(in) :: R, RI(3)

    if (Ri(1)**2 + Ri(2)**2 + Ri(3)**2 .GE. R**2) then
        check_outside_sphere = .TRUE.
    else
        check_outside_sphere = .FALSE.
    endif
    return
end function

! inside cylinder of length L, with hemispherical caps of radius R
! centered at origin, runing along the x-axis.
logical function check_outside_cell(Ri, L, R)
    DOUBLE PRECISION, intent(in) :: L, R, RI(3)
    DOUBLE PRECISION YZ_R2, RXeff

    check_outside_cell = .FALSE.
    ! outside cylinder, absolutely
    YZ_R2 = Ri(2)**2 + Ri(3)**2
    if (YZ_R2 .GE. R**2) then
        check_outside_cell = .TRUE.
        return
    endif
    ! from now on, we know we're inside an infinite cylinder of radius R

    ! if we're inside the purely cylindrical part of the cell, we're definitely
    ! inside
    if (Ri(1) .LT. L/2 .AND. Ri(1) .GT. -L/2) then
        check_outside_cell = .FALSE.
        return
    endif
    ! beyond ends of caps, absolutely
    if (Ri(1) .GE. L/2 + R .OR. Ri(1) .LE. -L/2 - R) then
        check_outside_cell = .TRUE.
        return
    endif
    ! otherwise we're inside the region where the sphereical caps must be
    ! checked exactly. Simply transform it into an equivalent problem of checking
    ! if we're inside a sphere offset from the origin
    if (Ri(1) .LT. 0) then
        RXeff = Ri(1) + L/2
    else
        RXeff = Ri(1) - L/2
    endif
    if (RXeff**2 + YZ_R2 .GE. R**2) then
        check_outside_cell = .TRUE.
    endif
    return
end function
