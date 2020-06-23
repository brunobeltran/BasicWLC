#include "../defines.inc"
module nucleosome
!   --------------------------------------------------------------
!
!    This module is designed to handle the various geometrical
!    considerations of nucleosomes.
!
!   --------------------------------------------------------------

    use precision, only: dp, pi
    implicit none
    real(dp), parameter :: basePairsPerTurn = 10.5_dp
    real(dp), dimension(10,WLC_P__MAX_BACEPAIRS_PER_BEAD) :: multiParams
    real(dp), dimension(3,3,147) :: nucleosomeROT
    real(dp), dimension(3,147) :: nucleosomeTran

    private :: nucleosomeROT, nucleosomeTran
contains

subroutine nucleosomeProp(Uin,Vin,Rin,linkBP,wrapBP,Uout,Vout,Rout)
!Calculates the final position and orientation
!Rout,Uout,Vout
!based on the the incomming position and orientation
!Rin, Uin, Vin
!for a nucleosome with wrapBP bace paris of DNA wrapped around it.
!The intrisic rotation of a linkBP long linker that follows is also included.
    use vector_utils, only: cross
    implicit none
    real(dp), intent(in), dimension(3) :: Uin ! Entry direction along DNA
    real(dp), intent(in), dimension(3) :: Vin ! Entry tangent vector
    real(dp), intent(in), dimension(3) :: Rin ! Position of entry
    real(dp), intent(in) :: linkBP
    integer, intent(in) :: wrapBP
    real(dp), intent(out), dimension(3) :: Uout ! Exit position
    real(dp), intent(out), dimension(3) :: Vout ! Exit tangent vector
    real(dp), intent(out), dimension(3) :: Rout ! Exit position

    real(dp), dimension(3,3) :: linkRot
    real(dp), dimension(3,3) :: mtrx
    real(dp), parameter :: angle = 2*pi/basePairsPerTurn ! intrinsic rotation/bp

    mtrx(:,1) = Vin
    mtrx(:,2) = cross(Uin,Vin)
    mtrx(:,3) = Uin

    Rout = Rin + MATMUL(mtrx,nucleosomeTran(:,wrapBP))

    linkRot(1,:) = [cos(angle*linkBP),-sin(angle*linkBP), 0.0_dp]
    linkRot(2,:) = [sin(angle*linkBP), cos(angle*linkBP), 0.0_dp]
    linkRot(3,:) = [           0.0_dp,            0.0_dp, 1.0_dp]

    mtrx = MATMUL(MATMUL(mtrx,nucleosomeROT(:,:,wrapBP)),linkRot)

    Uout = mtrx(:,3)/norm2(mtrx(:,3))
    Vout = mtrx(:,1)/norm2(mtrx(:,1))
    return

end subroutine nucleosomeProp

function nucleosome_energy(RP1,R,UP1,U,VP1,V,linkBP,wrapBP)
!Caltulate the bending energy of a nucleosome
!at position R and orientation U, V
!with wrapBP of DNA wrapped around it followed by a liner linkBP
!bace pairs long that ends at RP1, UP1, and VP1.
!Assumes a SSWLCWT linker.
    use MC_wlc, only: E_SSWLCWT
    real(dp), intent(in), dimension(3) :: R ! R of bead i
    real(dp), intent(in), dimension(3) :: RP1 ! R of bead i+1
    real(dp), intent(in), dimension(3) :: U ! U of bead i
    real(dp), intent(in), dimension(3) :: UP1 ! U of bead i+1
    real(dp), intent(in), dimension(3) :: V ! V of bead i
    real(dp), intent(in), dimension(3) :: VP1 ! V of bead i+1
    real(dp), intent(in) :: linkBP
    integer, intent(in) :: wrapBP
    real(dp) nucleosome_energy(4)
    real(dp) EB, EPAR,EPERP,GAM,ETA,XIR,XIU,sigma,etwist,simtype


    real(dp) Rtemp(3)
    real(dp) Utemp(3)
    real(dp) Vtemp(3)


    call nucleosomeProp(U,V,R,linkBP,wrapBP,Utemp,Vtemp,Rtemp)

    !  need to interpolate between basepairs if fractional
    call get_params(linkBP,EB,EPAR,EPERP,GAM,ETA,XIR,XIU,sigma,etwist,simtype)

    nucleosome_energy =  E_SSWLCWT(RP1,Rtemp,UP1,Utemp,VP1,Vtemp, &
                   EB,&!multiParams(1,linkBP),&   ! EB
                   EPAR,&!multiParams(2,linkBP),&   ! EPAR
                   EPERP,&!multiParams(3,linkBP),&   ! EPERP
                   ETA,&!multiParams(5,linkBP),&   ! ETA
                   GAM,&!multiParams(4,linkBP),&   ! GAM
                   ETWIST)!multiParams(9,linkBP))   ! etwist
end function nucleosome_energy

function internucleosome_energy(RI,RJ,UI,UJ,VI,VJ)
! internucleosome attraction energy. this is based off the work out of 
! de pablos group where they look at the pairwise potential between 2
! nucleosome dependent on their orientation/geometry and distance
! from each other. parameters come from their counterion condensation 
! assumptions. i try to simplify their findings and create 3 main 
! classes of interactions: 1) face-face where the nucleosomes are 
! aligned such that their core histones can interact 2) face-side
! where the nucleosomes are aligned perpendicular such that the core
! histones of one nucleosome interacts with the DNA wrapping the exterior
! of the other nucleosome and 3) side-side where the nucleosome DNA wrappings
! are in line with each other.  
    use MC_wlc, only: E_SSWLCWT
    use vector_utils, only: cross
    use energies, only: energyOf, internucleosome_
    real(dp), intent(in), dimension(3) :: RI ! R of nuc i
    real(dp), intent(in), dimension(3) :: RJ! R of nuc j
    real(dp), intent(in), dimension(3) :: UI ! U of nuc i
    real(dp), intent(in), dimension(3) :: UJ ! U of nuc j
    real(dp), intent(in), dimension(3) :: VI ! V of nuc i
    real(dp), intent(in), dimension(3) :: VJ ! V of nuc j
    real(dp), parameter :: tau_faceface = 1.38
    real(dp), parameter :: e_faceface = 3.712*WLC_P__INTERNUCLEOSOME_ENERGY
    real(dp), parameter :: tau_faceside = 0.82
    real(dp), parameter :: e_faceside = 1.476*WLC_P__INTERNUCLEOSOME_ENERGY
    real(dp), parameter :: tau_sideside = 2.0
    real(dp), parameter :: e_sideside = 1.64*WLC_P__INTERNUCLEOSOME_ENERGY
    real(dp), dimension(3), parameter :: center = [4.8455, -2.4445, 0.6694]
    real(dp), dimension(3,3) :: mtrxI, mtrxJ
    real(dp), dimension(3) :: polyI, faceI, faceItop, faceIbot
    real(dp), dimension(3) :: polyJ, faceJ, faceJtop, faceJbot
    real(dp), dimension(4,3) :: faceDistList
    real(dp), dimension(4) :: distList
    real(dp), dimension(3) :: distS, distC, dist
    real(dp) cospsi, costhetaS, costhetaC
    integer i, indList(1)
    real(dp) internucleosome_energy

    ! initialiaze
    internucleosome_energy = 0

    ! construct matrices
    mtrxI(:,1) = VI
    mtrxI(:,2) = cross(UI,VI)
    mtrxI(:,3) = UI
    mtrxJ(:,1) = VJ
    mtrxJ(:,2) = cross(UJ,VJ)
    mtrxJ(:,3) = UJ

    ! center of nucs
    polyI = RI + MATMUL(mtrxI, center)
    polyJ = RJ +  MATMUL(mtrxJ, center)

    ! construct face I normal vector (pointing up through face)
    faceItop = RI + MATMUL(mtrxI, center+[0.0_dp,WLC_P__NUCLEOSOME_HEIGHT/2,0.0_dp])
    faceIbot = RI + MATMUL(mtrxI, center+[0.0_dp,-WLC_P__NUCLEOSOME_HEIGHT/2,0.0_dp])
    faceI = faceItop-faceIbot
    ! construct face J normal vector (pointing up through face)
    faceJtop = RJ + MATMUL(mtrxJ, center+[0.0_dp,WLC_P__NUCLEOSOME_HEIGHT/2,0.0_dp])
    faceJbot = RJ + MATMUL(mtrxJ, center+[0.0_dp,-WLC_P__NUCLEOSOME_HEIGHT/2,0.0_dp])
    faceJ = faceJtop-faceJbot
    cospsi = dot_product(faceI/norm2(faceI),faceJ/norm2(faceJ))

    ! list of combinatorial face attractions
    faceDistList(1,:) = faceItop - faceJbot 
    faceDistList(2,:) = faceItop - faceJtop 
    faceDistList(3,:) = faceIbot - faceJbot 
    faceDistList(4,:) = faceIbot - faceJtop 

    ! find the closest faces to define the face oritentation vector
    do i = 1,4
        distList(i) = norm2(faceDistList(i,:))
    enddo
    indList = minloc(distList)
    distS = faceDistList(indList(1),:)
    costhetaS = dot_product(distS/norm2(distS),faceJ/norm2(faceJ))!-cospsi*faceJ/(abs(cospsi)*norm2(faceJ)))
    ! face-face (histone-histone attraction)
    if (norm2(distS) <= tau_faceface) then 
        internucleosome_energy = internucleosome_energy &
                - e_faceface*abs(cospsi)*abs(costhetaS)/tau_faceface
    else
        internucleosome_energy = internucleosome_energy &
                - e_faceface*abs(cospsi)*abs(costhetaS)/norm2(distS)
    endif
    distC = polyI-polyJ
    costhetaC = dot_product(distC/norm2(distC),faceJ/norm2(faceJ))!-cospsi*faceJ/(abs(cospsi)*norm2(faceJ)))
    ! face-side (histone-DNA attraction)
    dist = norm2(distC)-WLC_P__NUCLEOSOME_HEIGHT/2-WLC_P__NUCLEOSOME_RADIUS
    if (norm2(dist) <= tau_faceside) then 
        internucleosome_energy = internucleosome_energy &
                - e_faceside*(1-abs(cospsi))*abs(costhetaC)/tau_faceside
    else
        internucleosome_energy = internucleosome_energy &
                - e_faceside*(1-abs(cospsi))*abs(costhetaC)/norm2(dist)
    endif
    ! side-side (DNA-DNA attraction)
    dist = norm2(distC)-2*WLC_P__NUCLEOSOME_RADIUS
    if (norm2(dist) <= tau_sideside) then 
        internucleosome_energy = internucleosome_energy &
                - e_sideside*(1-abs(costhetaC))/tau_sideside
    else
        internucleosome_energy = internucleosome_energy &
                - e_sideside*(1-abs(costhetaC))/norm2(dist)
    endif

end function internucleosome_energy

! ---------------------------------------------------------------------
!  Return parameters for a linker i bace pairs long
! ---------------------------------------------------------------------
subroutine get_params(i,EB,EPAR,EPERP,GAM,ETA,XIR,XIU,sigma,etwist,simtype)
!Return parameters for a linker that is i bace pairs long
    implicit none
    real(dp), intent(in) :: i ! Number of bace pairs in linder
    real(dp), intent(out) :: EB ! Bending modulus
    real(dp), intent(out) :: EPAR ! Stretch modulus
    real(dp), intent(out) :: EPERP ! Shear modulus
    real(dp), intent(out) :: GAM ! ground state segment length
    real(dp), intent(out) :: ETA ! bend-shear coupling
    real(dp), intent(out) :: XIR,XIU,sigma
    real(dp), intent(out) :: etwist ! Twist coefficient
    real(dp), intent(out) :: simtype ! WLC, SSWLC, or Gaussian Chain
    integer indUp, indDown
    real(dp) ratio, offratio

        ! interpolate between points
        indDown = floor(i)
        indUp = ceiling(i)
        ratio = i/indUp
        offratio = 1-ratio
        EB     = ratio*multiParams(1,indUp) + offratio*multiParams(1,indDown)
        EPAR   = ratio*multiParams(2,indUp) + offratio*multiParams(2,indDown)
        EPERP  = ratio*multiParams(3,indUp) + offratio*multiParams(3,indDown)
        GAM    = ratio*multiParams(4,indUp) + offratio*multiParams(4,indDown)
        ETA    = ratio*multiParams(5,indUp) + offratio*multiParams(5,indDown)
        XIR    = ratio*multiParams(6,indUp) + offratio*multiParams(6,indDown)
        XIU    = ratio*multiParams(7,indUp) + offratio*multiParams(7,indDown)
        sigma  = ratio*multiParams(8,indUp) + offratio*multiParams(8,indDown)
        etwist = ratio*multiParams(9,indUp) + offratio*multiParams(9,indDown)
        simtype = int(ratio*multiParams(10,indUp) + offratio*multiParams(10,indDown)+0.1_dp) ! the +0.1 is to prevent round down due to lack of precision
end subroutine get_params

! ----------------------------------------------------------------------
!
!          Look up elastic constants for linkers,
!          Look up translation and rotation for nucleosomes.
!
! ----------------------------------------------------------------------
subroutine setup_nucleosome_constants()
    use precision, only: nan
    use MC_wlc, only: calc_elastic_constants
    implicit none
    integer i,j, simtype
    real(dp) DEL
    real(dp) EB, EPAR,EPERP,GAM,ETA,XIR,XIU,sigma,etwist, dt

    multiParams=nan
    do i = 2,WLC_P__MAX_BACEPAIRS_PER_BEAD
        DEL = WLC_P__LENGTH_PER_BP*i/WLC_P__LP
        call calc_elastic_constants(DEL,WLC_P__LP,WLC_P__LT,EB,EPAR,GAM,XIR,EPERP,ETA,XIU,DT,&
                                SIGMA,ETWIST,simtype)
        if (simtype .ne. 2) then
            print*, "Error!  nucleosomes only setup for SSWLCWT"
            print*, "Simtype ",simtype, " encountered"
            stop
        endif
        multiParams(1,i) = EB
        multiParams(2,i) = EPAR
        multiParams(3,i) = EPERP
        multiParams(4,i) = GAM
        multiParams(5,i) = ETA
        multiParams(6,i) = XIR
        multiParams(7,i) = XIU
        multiParams(8,i) = sigma
        multiParams(9,i) = etwist
        multiParams(10,i) = real(simtype,dp)

    enddo

    open (UNIT = 5, FILE = "input/nucleosomeR", STATUS = "OLD")
    do i = 1,147
        do j = 1,3
            read(5,*) nucleosomeROT(j,:,148-i)
        enddo
    enddo
    close(5)

    if (WLC_P__INCLUDE_NUC_TRANS) then
        open (UNIT = 5, FILE = "input/nucleosomeT", STATUS = "OLD")
        do i = 1,147
            read(5,*) nucleosomeTran(:,148-i)
        enddo
        close(5)
    else
        nucleosomeTran = 0.0_dp
    endif
end subroutine setup_nucleosome_constants

subroutine loadNucleosomePositions(wlc_nucleosomeWrap,wlc_basepairs)
! if WLC_P__INCLUDE_DISCRETIZE_LINKER is turn on, then this determines the 
! discretization scheme of the beads throughout the chain. it defaults to trying
! to set integer values for each bead discretization, but not has the capability to 
! use real numbers. if WLC_P__INCLUDE_DISCRETIZE_LINKER is off, then each bead is 
! default assumed to be a nucleosome, so there are no additional beads used to model
! the fluctuations of the linker geometry with more detail. nucleosomes are intialized
! on the chain according to WLC_P__LINKER_TYPE, where the default is 'phased', i.e.
! the nuclesomes are separated by a constant linker length throughout the chain
    use precision, only: nan
    ! sterics testing !
    use GJKAlgorithm, only: GJK, sameShapeTest, noIntersectX, intersectX, tangentX, runtimeTest5, runtimeTest6, &
                            noIntersectY, intersectY, tangentY, runtimeTest1, runtimeTest3, &
                            noIntersectZ, intersectZ, tangentZ, runtimeTest2, runtimeTest4
    use polydispersity, only: first_bead_of_chain, last_bead_of_chain
    implicit none
    integer, intent(out) :: wlc_nucleosomeWrap(WLC_P__NT)
    real(dp), intent(out) :: wlc_basepairs(WLC_P__NT)
    real(dp), parameter :: L_in_bp = WLC_P__L/WLC_P__LENGTH_PER_BP
    integer, parameter :: nNucs = nint((L_in_bp-WLC_P__LL)/(147+WLC_P__LL)) ! assuming all octasomes
    real(dp) discretization, off_discretization, num_link_beads
    real(dp), dimension(2,33) :: LL_dist
    real(dp) cumlinker, linker
    real(dp) urand(3)  ! random vector
    integer iter, i, j, k

    print*, WLC_P__L0
    print*, nNucs, WLC_P__NB, WLC_P__LL
    if (WLC_P__INCLUDE_NUC_TRANS) then
        if (WLC_P__INCLUDE_DISCRETIZE_LINKER) then 
            ! figure out main discretization scheme
            discretization = WLC_P__LL*(NNucs+1)/(WLC_P__NB-1)
            num_link_beads = WLC_P__LL/discretization
            print*, discretization, num_link_beads
            if (WLC_P__LINKER_TYPE == 'phased' ) then
                do i = 1, WLC_P__NP
                    ! initialize
                    iter = first_bead_of_chain(i)
                    ! set first linker later
                    iter = iter + num_link_beads
                    ! set middle linkers 
                    do j = 2, nNucs ! hanging linker off nucleosomes
                        wlc_nucleosomeWrap(iter) = 147
                        wlc_basepairs(iter) = discretization
                        iter = iter + 1
                        if (iter + num_link_beads - 2 <= last_bead_of_chain(i) - num_link_beads) then
                            wlc_basepairs(iter:iter+num_link_beads-2) = discretization  
                            wlc_nucleosomeWrap(iter:iter+num_link_beads-2) = 1 
                            iter = iter + num_link_beads - 1
                        endif
                    enddo
                    ! first linker
                    wlc_nucleosomeWrap(first_bead_of_chain(i):first_bead_of_chain(i)+num_link_beads-1) = 1
                    wlc_basepairs(first_bead_of_chain(i):first_bead_of_chain(i)+num_link_beads-1) = discretization
                    ! last linker
                    wlc_basepairs(iter) = discretization
                    wlc_nucleosomeWrap(iter) = 147
                    iter = iter + 1
                    wlc_nucleosomeWrap(iter:iter+num_link_beads-1) = 1
                    wlc_basepairs(iter:iter+num_link_beads-1) = discretization
                    ! set last wlc_basepairs to 0 as reminder that this is not an actual extension
                    wlc_basepairs(last_bead_of_chain(i)) = 0
                enddo
            else if (WLC_P__LINKER_TYPE == 'voong') then 
                ! read in the LL distribution
                open (UNIT = 5, FILE = "input/Voong_LL_23to55.txt", STATUS = "OLD")
                do i = 1,33
                    read(5,*) LL_dist(:,i)
                enddo
                close(5)
                do k = 1, WLC_P__NP
                    linker = 0
                    do while (linker <= 23 .or. linker >= 55) 
                        ! initialize
                        cumlinker = 0
                        iter = first_bead_of_chain(k)
                        ! set first linker later
                        iter = iter + num_link_beads
                        ! set middle linkers 
                        do i = 2, nNucs ! hanging linker off nucleosomes
                            call random_number(urand)
                            do j = 1,33
                                if (LL_dist(2,j) >= urand(1)) then 
                                    exit
                                endif
                            enddo
                            linker =  LL_dist(1,j)
                            discretization = linker/num_link_beads
                            print*, linker, discretization, num_link_beads
                            wlc_nucleosomeWrap(iter) = 147
                            wlc_basepairs(iter) = discretization
                            iter = iter + 1
                            if (iter + num_link_beads - 2 <= last_bead_of_chain(k) - num_link_beads) then
                                wlc_basepairs(iter:iter+num_link_beads-2) = discretization  
                                wlc_nucleosomeWrap(iter:iter+num_link_beads-2) = 1 
                                iter = iter + num_link_beads - 1
                            endif
                            cumlinker = cumlinker + linker
                        enddo
                        ! set last (and first) linker to conserve contour length
                        linker = (WLC_P__LL*(1+nNucs)-cumlinker)/2
                        print*, 'attempted to sample from linker distribution for chain'
                    enddo
                    discretization = linker/num_link_beads
                    print*, linker, discretization, num_link_beads
                    ! first linker
                    wlc_nucleosomeWrap(first_bead_of_chain(k):first_bead_of_chain(k)+num_link_beads-1) = 1
                    wlc_basepairs(first_bead_of_chain(k):first_bead_of_chain(k)+num_link_beads-1) = discretization
                    ! last linker
                    wlc_basepairs(iter) = discretization
                    wlc_nucleosomeWrap(iter) = 147
                    iter = iter + 1
                    wlc_nucleosomeWrap(iter:iter+num_link_beads-1) = 1
                    wlc_basepairs(iter:iter+num_link_beads-1) = discretization
                    ! set last wlc_basepairs to 0 as reminder that this is not an actual extension
                    wlc_basepairs(last_bead_of_chain(k)) = 0
                enddo
            else
                print*, 'not recognized linker length type'
                stop
            endif
            ! testing sterics here !
            if(WLC_P__GJK_STERICS) then
            do iter = 1, 10000 ! check to make sure GJK is not stochastic
                call sameShapeTest()
                call noIntersectX()
                call intersectX()
                call tangentX()
                call noIntersectY()
                call intersectY()
                call tangentY()
                call noIntersectZ()
                call intersectZ()
                call tangentZ()
                call runtimeTest1()
                call runtimeTest2()
                call runtimeTest3()
                call runtimeTest4()
                call runtimeTest5()
                call runtimeTest6()
            enddo
                print*, "SUCCESS: successful completion of all GJK collision unit tests"
            endif
        endif
    else
        wlc_nucleosomeWrap = 147
        wlc_basepairs = WLC_P__LL
    endif
    print*, wlc_basepairs
    print*, wlc_nucleosomeWrap

end subroutine

end module nucleosome
