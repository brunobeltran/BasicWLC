#include "../defines.inc"
!--------------------------------------------------------------*
!
!           Makes Monti Carlo Moves
!
!    Quinn split out this file on 8/9/17
!
!---------------------------------------------------------------

! variables that need to be allocated only on certain branches moved into MD to prevent segfaults
! please move other variables in as you see fit
subroutine MC_spider(MCAMP,rand_stat,success,spider_id)
! values from wlcsim_data
use params, only: wlc_UP, wlc_RP, wlc_explicitbindingpair, wlc_U, wlc_VP&
    , wlc_spiders, wlc_V, wlc_R, wlc_numberOfSpiders

use mersenne_twister
use params, only: wlcsim_params,  pi
use precision, only: dp, eps
use vector_utils, only: randomUnitVec, cross, distance, angle_of_triangle, &
                        round_into_pm1, rotateR, rotateU, axisAngle, rotateAintoB, random_perp
use windowTools, only: drawWindow

implicit none
type(random_stat), intent(inout) :: rand_stat  ! status of random number generator
logical, intent(out) :: success

real(dp), intent(in) :: MCAMP ! Amplitude of random change

integer leg_n, I, section_n
integer irnd(1) ! random intiger
real(dp) urnd(1) ! single random number
integer spider_id ! which spider
real(dp) dr(3) ! ranslation
integer hip, knee, toe
real(dp) thigh, shin, dold, dnew
real(dp) dalpha ! amount to rotate shin
real(dp) dbeta ! amount to rotate thigh
real(dp), dimension(3) :: hipR,kneeR,toeR,direction, temp,rold,rnew,rinter
real(dp) ROT(3,4) ! Rotation matrix
logical swing_past
integer dib, IT1,IT2,IB1,IB2,IP,otherEnd
real(dp) extent(3)
real(dp) extent2(3)

! choose spider
call random_number(urnd,rand_stat)
if (WLC_P__PROBABILITY_PRECALC_SPIDER > urnd(1)) then
    call random_index(wlc_numberOfSpiders,irnd,rand_stat)
    spider_id=irnd(1)
else
    call drawWindow(WLC_P__SPIDER_WINDOW,WLC_P__MAXWINDOW_CRANK_SHAFT,.true.,rand_stat,&
                    IT1,IT2,IB1,IB2,IP,DIB,success)
    if (success .eqv. .false.) return


    spider_id=wlc_numberOfSpiders+1
    wlc_spiders(spider_id)%nSections=1
    wlc_spiders(spider_id)%nLegs=2
    wlc_spiders(spider_id)%sections(1,1) = IT1
    wlc_spiders(spider_id)%sections(2,1) = IT2

    if (WLC_P__EXPLICIT_BINDING) then
        !  Right Leg
        knee = IT2
        do I = 1,WLC_P__SPIDER_LEG_LENGTH
            knee = knee + 1
            if (knee > WLC_P__NT) then
                success = .False.
                return
            endif
            otherEnd = wlc_explicitbindingpair(knee)
            if (otherEnd < 0) continue

            if (otherEnd < knee) then
                success = .False.
                return
            endif
            knee = otherEnd
        enddo
        toe=knee
        do I = 1,WLC_P__SPIDER_LEG_LENGTH
            toe = toe + 1
            if (toe > WLC_P__NT) then
                success = .False.
                return
            endif
            otherEnd = wlc_explicitbindingpair(toe)
            if (otherEnd < 0) continue

            if (otherEnd < toe) then
                success = .False.
                return
            endif
            toe = otherEnd
        enddo
        wlc_spiders(spider_id)%legs(1,1) = IT2
        wlc_spiders(spider_id)%legs(2,1) = knee
        wlc_spiders(spider_id)%legs(3,1) = toe

        !  Left Leg
        knee = IT1
        do I = 1,WLC_P__SPIDER_LEG_LENGTH
            knee = knee - 1
            if (knee > WLC_P__NT) then
                success = .False.
                return
            endif
            otherEnd = wlc_explicitbindingpair(knee)
            if (otherEnd < 0) continue

            if (otherEnd > knee) then
                success = .False.
                return
            endif
            knee = otherEnd
        enddo
        toe=knee
        do I = 1,WLC_P__SPIDER_LEG_LENGTH
            toe = toe - 1
            if (toe > WLC_P__NT) then
                success = .False.
                return
            endif
            otherEnd = wlc_explicitbindingpair(knee)
            if (otherEnd < 0) continue

            if (otherEnd > toe) then
                success = .False.
                return
            endif
            toe = otherEnd
        enddo
        wlc_spiders(spider_id)%legs(1,2) = IT1
        wlc_spiders(spider_id)%legs(2,2) = knee
        wlc_spiders(spider_id)%legs(3,2) = toe
    else
        knee = IT2+WLC_P__SPIDER_LEG_LENGTH
        toe = knee + WLC_P__SPIDER_LEG_LENGTH
        wlc_spiders(spider_id)%legs(1,1) = IT2
        wlc_spiders(spider_id)%legs(2,1) = knee
        wlc_spiders(spider_id)%legs(3,1) = toe

        knee = IT1-WLC_P__SPIDER_LEG_LENGTH
        toe = knee - WLC_P__SPIDER_LEG_LENGTH
        wlc_spiders(spider_id)%legs(1,2) = IT1
        wlc_spiders(spider_id)%legs(2,2) = knee
        wlc_spiders(spider_id)%legs(3,2) = toe
    endif

    toe = wlc_spiders(spider_id)%legs(3,1)
    if ((toe-1)/WLC_P__NB .ne. (IT2-1)/WLC_P__NB) then ! is on a different polyme
        success = .False.
        return
    endif
    if (toe-IT2 > WLC_P__MAX_SPIDER_LEG_LENGTH) then
        success = .False.
        return
    endif
    wlc_spiders(spider_id)%moved_sections(2,1) = toe

    toe = wlc_spiders(spider_id)%legs(3,2)
    if ((toe-1)/WLC_P__NB .ne. (IT1-1)/WLC_P__NB) then
        success = .False.
        return
    endif
    if (toe<1) then
        success = .False.
        return
    endif
    if (IT1-toe > WLC_P__MAX_SPIDER_LEG_LENGTH) then
        success = .False.
        return
    endif
    wlc_spiders(spider_id)%moved_sections(1,1) = toe


endif


! choose random offset.  Here we use p~r^(-2) between 0 and MCAMP
call randomUnitVec(dr,rand_stat)
call random_number(urnd,rand_stat)
dr=dr*urnd(1)*MCAMP

! check to see if all legs will reach
do leg_n = 1,wlc_spiders(spider_id)%nLegs
    hip = wlc_spiders(spider_id)%legs(1,leg_n)
    knee= wlc_spiders(spider_id)%legs(2,leg_n)
    toe = wlc_spiders(spider_id)%legs(3,leg_n)
    hipR=wlc_R(:,hip)
    kneeR=wlc_R(:,knee)
    toeR=wlc_R(:,toe)
    ! check triangle inequlity
    extent=hipR+dr
    if (distance(hipR,kneeR)+distance(kneeR,toeR) < distance(extent,toeR)) then
        success = .FALSE.
        return
    endif
    if (abs(distance(hipR,kneeR)-distance(kneeR,toeR)) > distance(extent,toeR)) then
        success = .FALSE.
        return
    endif
enddo
success= .TRUE.

! move legs
do leg_n = 1,wlc_spiders(spider_id)%nLegs
    hip = wlc_spiders(spider_id)%legs(1,leg_n)
    knee= wlc_spiders(spider_id)%legs(2,leg_n)
    toe = wlc_spiders(spider_id)%legs(3,leg_n)
    hipR=wlc_R(:,hip)
    kneeR=wlc_R(:,knee)
    toeR=wlc_R(:,toe)
    thigh = distance(kneeR,hipR)
    shin = distance(kneeR,toeR)

    rold = hipR-toeR
    dold = norm2(rold)
    rnew = hipR+dr-toeR
    dnew = norm2(rnew)

    if (dold<=0.001_dp*eps) then
        ! if hip and toe are in the same place, extend in random direction
        extent=(kneeR-toeR)/shin
        call random_perp(extent,direction,rinter,rand_stat)
        if (dot_product(rinter,rnew)<0.0_dp) then
            rinter=-1.0_dp*rinter
        endif
        extent=kneeR-toeR
        direction = cross(extent,rinter)
        direction = direction/norm2(direction)
        rinter = rinter*dnew
        dalpha = asin(dnew/(2.0_dp*shin))
        dbeta = -dalpha
        swing_past = .False.
    else
        ! Calculate how much to rotate each leg to get new |distance|
        swing_past = dot_product(rold,rnew) < 0.0_dp  ! toe passes hip
        if (swing_past) then
            dalpha = angle_of_triangle(thigh,shin,dold) - (PI - angle_of_triangle(thigh,shin,dnew))
            dbeta = (PI - angle_of_triangle(shin,thigh,dnew)) - angle_of_triangle(shin,thigh,dold)
        else
            dalpha = angle_of_triangle(thigh,shin,dold) - angle_of_triangle(thigh,shin,dnew)
            dbeta = angle_of_triangle(shin,thigh,dnew) - angle_of_triangle(shin,thigh,dold)
        endif
        ! direction needs to be consistant with other angles
        extent=kneeR-toeR
        extent2=hipR-kneeR
        direction = cross(extent,extent2)
        if (norm2(direction)<0.01_dp*eps) then
            ! if fully exteded/contracted then contract/extend in random direction
            extent = rold/norm2(rold)
            call random_perp(extent,direction,rinter,rand_stat)
        else
            direction = direction/norm2(direction)
        endif
        if (swing_past) then
            rinter = -dnew*rold/dold
        else
            rinter = dnew*rold/dold
        endif
    endif

    ! angle to rotate shin
    call axisAngle(ROT,dalpha,direction,toeR)

    ! rotate shin to intermediate position
    do I = min(knee,toe),max(knee,toe)
        wlc_RP(:,I) = rotateR(ROT,wlc_R(:,I))
        wlc_UP(:,I) = rotateU(ROT,wlc_U(:,I))
        if (WLC_P__LOCAL_TWIST) wlc_VP(:,I) = rotateU(ROT,wlc_V(:,I))
    enddo
    temp = wlc_RP(:,toe)

    !Check toe stayed in the sampe place
    if ( distance(wlc_RP(:,toe),wlc_R(:,toe)) > eps ) then
        print*, "Broken toe in spider move"
        stop 1
    endif

    ! angle to rotate thigh
    call axisAngle(ROT,dbeta,direction,hipR)

    ! Rotation matrix for hip to intermediate position
    I = knee
    temp = rotateR(ROT,wlc_R(:,I))
    temp = temp + rinter - rold

    ! Check knee
    if ( distance(wlc_RP(:,knee),temp) > eps ) then
        print*, "Broken knee"
        stop 1
    endif

    ! rotate thigh
    do I = min(knee,hip),max(knee,hip)
        wlc_RP(:,I) = rotateR(ROT,wlc_R(:,I))
        wlc_RP(:,I) = wlc_RP(:,I) + rinter - rold
        wlc_UP(:,I) = rotateU(ROT,wlc_U(:,I))
        if (WLC_P__LOCAL_TWIST) wlc_VP(:,I) = rotateU(ROT,wlc_V(:,I))
    enddo

    ! rotate from intermediate position to final position
    call rotateAIntoB(rinter,rnew,toeR,ROT)
    do I = min(toe,hip),max(toe,hip)
        temp = rotateR(ROT,wlc_RP(:,I))
        wlc_RP(:,I) = temp
        temp = rotateU(ROT,wlc_UP(:,I))
        wlc_UP(:,I) = temp
        if (WLC_P__LOCAL_TWIST) then
            temp = rotateU(ROT,wlc_VP(:,I))
            wlc_VP(:,I) = temp
        endif
    enddo

    ! don't rotatie hip and toe
    wlc_UP(:,hip)=wlc_U(:,hip)
    wlc_UP(:,toe)=wlc_U(:,toe)
    if (WLC_P__LOCAL_TWIST) then
        wlc_VP(:,hip) = wlc_V(:,hip)
        wlc_VP(:,toe) = wlc_V(:,toe)
    endif

    !Check toe stayed in the sampe place
    if ( distance(wlc_RP(:,toe),wlc_R(:,toe)) > eps ) then
        print*, "Broken toe in second half of spider move"
        stop 1
    endif
    ! chack to make sure hip moved the correct amount
    extent = hipR+dr
    if ( distance(wlc_RP(:,hip),extent) > eps ) then
        print*, "Broken hip"
    endif

enddo

! translate sections
do section_n = 1,wlc_spiders(spider_id)%nSections
    do I = wlc_spiders(spider_id)%sections(1,section_n), &
           wlc_spiders(spider_id)%sections(2,section_n)
        wlc_RP(:,I) = wlc_R(:,I) + dr
        wlc_UP(:,I) = wlc_U(:,I)
        if (WLC_P__LOCAL_TWIST) wlc_VP(:,I) = wlc_V(:,I)
    enddo
enddo
end subroutine
