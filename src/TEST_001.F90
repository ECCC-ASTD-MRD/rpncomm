  function RPN_COMM_io_dist_coll_check(gni,gnj,halo_x,halo_y) result(status)
  use rpn_comm
  implicit none
  logical :: status
  integer, intent(IN) :: gni,gnj,halo_x,halo_y

  integer :: lni, lnj, lasti, lastj

  status = .true.
  lni = (gni+pe_nx-1)/pe_nx
  lasti = gni - (pe_nx-1)*lni
  if(lasti < max(1,halo_x)) then
    print 101,"CFG: ERROR: size of last tile along x, halo_x, gni, lni ",lasti, halo_x, gni, lni
    print 101,"CFG: ERROR: not enough points along x for problem, need gni >= ",max(lni,halo_x)*(pe_nx-1)+max(1,halo_x)
    status = .false.
  endif

  lnj = (gnj+pe_ny-1)/pe_ny
  lastj = gnj - (pe_ny-1)*lnj
  if(lastj < max(1,halo_y)) then
    print 101,"CFG: ERROR: size of last tile along y, halo_y, gnj, lnj ",lastj, halo_y, gnj, lnj
    print 101,"CFG: ERROR: not enough points along y for problem, need gnj >= ",max(lnj,halo_y)*(pe_ny-1)+max(1,halo_y)
    status = .false.
  endif

101 format(A,20I5)
end function RPN_COMM_io_dist_coll_check
subroutine RPN_COMM_io_dist_coll_test(nparams,params)
  use rpn_comm
  implicit none
  integer, intent(IN) :: nparams
  integer, intent(IN), dimension(nparams) :: params
  logical :: periodx, periody
  integer :: setno, me_io, n_io
  integer :: i, j, k, irep
  integer :: gni=10
  integer :: gnj=7
  integer :: dnk=2
  integer :: lnk=6
  integer :: iope_extra=3
  integer :: halo_x = 1
  integer :: halo_y = 1
  integer :: nrep_test=1
  integer, parameter :: MAX_PRINT=80
  integer :: max_dist_k
  integer, dimension(:), allocatable :: liste_k, liste_k2
  logical, dimension(:), allocatable  :: liste_o, liste_o2
  integer, dimension(:,:,:), allocatable :: global,global2
  integer, dimension(:,:,:), allocatable :: glob_l,glob_l2
  integer, dimension(:,:,:), allocatable :: local, loc_l2
  integer :: lni, lnj
  integer :: mini,maxi,minj,maxj,status
  integer, dimension(pe_nx) :: start_x,count_x
  integer, dimension(pe_ny) :: start_y,count_y
  integer :: i0,in,j0,jn,nerrors,nvalid,expected,effective_lnk,nerrors2,nvalid2
  logical, external :: RPN_COMM_io_dist_coll_check
  integer :: grid_id
!
  periodx = .false.
  periody = .false.
  lni = 0
  lnj = 0
!  goto 1
  if(pe_me == 0 .and. nparams > 2) then
    print 101,"CFG: nparams",nparams
    print 101,"CFG: params",params
  endif
  if(nparams > 2) then
    gni = params(1)
    gnj = params(2)
    dnk = params(3)
  endif
  if(nparams > 3) lnk = params(4)
  max_dist_k = lnk
  if(nparams >= 5) iope_extra = params(5)
  if(nparams >= 7) then
    halo_x = params(6)
    halo_y = params(7)
  endif
  if(nparams >= 8) max_dist_k = params(8)
  if(nparams >= 9) nrep_test = params(9)
  if(pe_me == 0) then
    print 101,"CFG: gni,gnj,dnk,halo_x,halo_y",gni,gnj,dnk,halo_x,halo_y
    print 101,"CFG: lnk,max_dist_k,pe_nx,pe_ny",lnk,max_dist_k,iope_extra,pe_nx,pe_ny
  endif
1 continue
  if(.not. RPN_COMM_io_dist_coll_check(gni,gnj,halo_x,halo_y)) return
!
  allocate( global(gni,gnj,dnk),  glob_l(1-halo_x:gni+halo_x,1-halo_y:gnj+halo_y,dnk))
  allocate(global2(gni,gnj,dnk))
  allocate(liste_k(dnk))
  liste_k = 0
  allocate(liste_k2(dnk))
  liste_k2 = 0
  allocate(liste_o(lnk), liste_o2(lnk))
  liste_o = .false.
  liste_o(max_dist_k) = .true.     ! test of warning for attempt to redistribute a level
  if(max_dist_k<lnk) liste_o(max_dist_k+1:lnk) = .false.
  liste_o2 = liste_o
!
! along X
!
  if(lni == 0) lni = (gni+pe_nx-1)/pe_nx
  mini = 1-halo_x
  maxi = lni+halo_x
  count_x = lni
  start_x(1) = 1
  do i = 2,pe_nx
    start_x(i) = start_x(i-1) + count_x(i-1)
  enddo
  count_x(pe_nx) = gni + 1 - start_x(pe_nx)
  lni = count_x(pe_mex+1)
  i0 = mini
  if(pe_mex == 0 .and. (.not. periodx)) i0 = 1
  in = maxi
  if(pe_mex == pe_nx-1 .and. (.not. periodx)) in = count_x(pe_nx)
  if(pe_me == 0) then
    print *,"start_x =",start_x
    print *,"count_x =",count_x
  endif
!
! along Y
!
  if(lnj == 0) lnj = (gnj+pe_ny-1)/pe_ny
  minj = 1-halo_y
  maxj = lnj+halo_y
  count_y = lnj
  start_y(1) = 1
  do j = 2,pe_ny
    start_y(j) = start_y(j-1) + count_y(j-1)
  enddo
  count_y(pe_ny) = gnj + 1 - start_y(pe_ny)
  lnj = count_y(pe_mey+1)
  j0 = minj
  if(pe_mey == 0 .and. (.not. periody)) j0 = 1
  jn = maxj
  if(pe_mey == pe_ny-1 .and. (.not. periody)) jn = count_y(pe_ny)
  if(pe_me == 0) then
    print *,"start_y =",start_y
    print *,"count_y =",count_y
  endif
!====================================================================================
  grid_id = rpn_comm_create_2dgrid(gni,gnj,mini,maxi,minj,maxj) ! create 2D grid descriptor
!====================================================================================
  if (grid_id == -1) then
    if(pe_me == 0) print 101,"ERROR: cannot create grid id, gni,gnj,mini,maxi,minj,maxj=",gni,gnj,mini,maxi,minj,maxj
    return
  endif
!
  allocate( local(mini:maxi,minj:maxj,lnk))
  allocate(loc_l2(mini:maxi,minj:maxj,lnk))
  local = 999999
  global = 888888
  glob_l = 777777
! create IO PE set
  if(pe_me == 0) then
    print *,'IO PE number of PEs =',min( min(pe_nx,pe_ny)+iope_extra , lnk)
    print *,'pe_nx,pe_ny,iope_extra,lnk',pe_nx,pe_ny,iope_extra,lnk
  endif
!  setno = RPN_COMM_create_io_set( min( min(pe_nx,pe_ny)+iope_extra , lnk) ,0)  ! make sure not to overflow lnk
  setno = RPN_COMM_create_io_set( min(pe_nx,pe_ny)+iope_extra , 0)  ! may overflow lnk
  if(setno <= 0) then
    print *,'ERROR: IO PE set creation error, quitting',setno
    return
  else
    print *,'INFO: IO PE set created :',setno
  endif
  me_io = RPN_COMM_is_io_pe(setno)     ! me in IO_set
  n_io = RPN_COMM_io_pe_size(setno)    ! IO_set population
!
  do irep = 1,nrep_test ! start of test  repetition loop
    liste_o = .false.
    liste_o(max_dist_k) = .true.     ! test of warning for attempt to redistribute a level
    if(max_dist_k<lnk) liste_o(max_dist_k+1:lnk) = .false.
    liste_o2 = liste_o
    if(me_io .ne. -1) then
      print *,"I am a busy IO pe!",me_io+1,' of',n_io
      do k=1,dnk               ! global levels that this PE will distribute
  !      liste_k(k) = lnk - me_io - (k-1)*n_io   !  levels lnk -> (lnk - nio*dnk + 1)
        liste_k(k) = 1 + me_io + (k-1)*n_io     !  levels 1 -> nio*dnk - 1
        if(liste_k(k) > max_dist_k) liste_k(k) = 0
        liste_k(k) = max( liste_k(k) , 0)
        liste_k2 = liste_k
        do j = 1,gnj
        do i = 1,gni
          global(i,j,k) = liste_k(k) + j*100 + i*10000
        enddo
        enddo
        do j = 1-halo_y,gnj+halo_y
        do i = 1-halo_x,gni+halo_x
          glob_l(i,j,k) = liste_k(k) + (j+100)*100 + (i+100)*100000
        enddo
        enddo
      enddo
! print *,'==== global array ====',liste_k
! do j = gnj,1,-1
!   print 102,' ',global(:,j,1)
! enddo
! print *,'==== glob_l array ====',liste_k
! do j = gnj+halo_y,1-halo_y,-1
!   print 102,' ',glob_l(:,j,1)
! enddo
      print *,"level list =",liste_k
      do k= dnk,1,-1
        if(liste_k(k) <= 0) cycle
        print *,"===== source level ==",liste_k(k),"  ====="
        do j=gnj,1,-1
          if(gni*gnj < MAX_PRINT) print 100,j,global(:,j,k)
        enddo
      enddo
    else
      print *,"I am a relaxed  NON-IO pe !"
    endif
! print *,'lni,lnj,mini,maxi,minj,maxj',lni,lnj,mini,maxi,minj,maxj
!return
#define EZ_TEST
#if defined(EZ_TEST)
!====================================================================================
! use previously obtained grid id
    print *,'=== RPN_COMM_shuf_ezdist ==='
    status = RPN_COMM_shuf_ezdist(setno, grid_id, global, dnk, local, lnk, liste_k, liste_o)
    status = RPN_COMM_shuf_ezdist_h(setno, grid_id, glob_l, dnk, loc_l2, lnk, liste_k, liste_o2)
!====================================================================================
#else
    print *,'=== RPN_COMM_shuf_dist ==='
    call RPN_COMM_shuf_dist(setno,  &
                            global,gni,gnj,dnk,  &
                            local,mini,maxi,minj,maxj,lnk,  &
                            liste_k,liste_o,  &
                            start_x,count_x,pe_nx,start_y,count_y,pe_ny,  &
                            periodx,periody,status)
    call RPN_COMM_shuf_dist(setno,  &
                            glob_l,gni+2*halo_x,gnj+2*halo_y,dnk,  &
                            loc_l2,mini,maxi,minj,maxj,lnk,  &
                            liste_k2,liste_o2,  &
                            start_x,count_x,pe_nx,start_y,count_y,pe_ny,  &
                            periodx,periody,status)
#endif
    print *,'liste_o, liste_o2 apres=',liste_o, liste_o2
    if(status .ne. RPN_COMM_OK) then
      print 101,"ERROR: RPN_COMM_shuf_dist failure, lnk,dnk,n_io ",lnk,dnk,n_io
      return
    endif
  enddo ! end of test repetition loop
!
  nerrors  = 0
  nerrors2 = 0
  nvalid   = 0
  nvalid2  = 0
  do k = lnk,1,-1
    if(liste_o(k)) then
      do j = j0,jn
      do i = i0,in
        nvalid = nvalid + 1
        expected = k + (start_y(pe_mey+1)+j-1)*100 + (start_x(pe_mex+1)+i-1)*10000
        if(expected .ne. local(i,j,k)) then
          if(nerrors<0)print *,'i,j,k,expected,local(i,j,k)',i,j,k,expected,local(i,j,k)
          nerrors = nerrors + 1
         if(nerrors>3)goto 666
        endif
      enddo
      enddo
      do j = mini,maxi
      do i = minj,maxj
        nvalid2 = nvalid2 + 1
        expected = k + (start_y(pe_mey+1)+j-1+100)*100 + (start_x(pe_mex+1)+i-1+100)*100000
        if(expected .ne. loc_l2(i,j,k)) then
          if(nerrors2<1)print *,'i,j,k,expected,local(i,j,k)',i,j,k,expected,loc_l2(i,j,k)
          nerrors2 = nerrors2 + 1
!          if(nerrors>3)goto 666
        endif
      enddo
      enddo
    else
      print *,'no data at level',k
    endif
  enddo
  print 101,"nerrors,  nvalid,  npts =",nerrors,nvalid,lnk*(in-i0+1)*(jn-j0+1),(in-i0+1),(jn-j0+1),lnk
  print 101,"nerrors2, nvalid2, npts =",nerrors2,nvalid2,lnk*(maxi-mini+1)*(maxj-minj+1),(maxi-mini+1),(maxj-minj+1),lnk

! return
  
  if(nerrors == 0) goto 777
!  goto 777
666 continue
#if  ! defined(DEPRECATED)
  if(gni*gnj < MAX_PRINT) then
    do k = lnk,1,-1
      if(liste_o(k)) then
        print *,"===== level",k," local ====="
        do j = maxj,minj,-1
  !        print 100,j,local(i0:in,j,k)
          print 100,j,local(:,j,k)
        enddo
      else
        print *,'no data at level',k
      endif
    enddo
  endif
#endif
!
100 format(I3,20I7.6)
101 format(A,20I5)
102 format(A,20I9)
!=======================================================================================
777 continue
!=======================================================================================
!
! collect test follows distribute test
!
  do irep = 1,nrep_test ! start of test repeat loop
    global2 = 999999
    liste_k2 = -999999
    effective_lnk = 1
    do k = 1 , lnk
      if(liste_o(k)) effective_lnk = k  ! highest level available
    enddo
    local = 989898
    do k = 1,effective_lnk
    do j = 1,lnj
    do i = 1,lni
      local(i,j,k) = k + (j - 1 + start_y(pe_mey+1))*100 + (i -1 + start_x(pe_mex+1))*10000
    enddo
    enddo
    enddo
    print *,"====== before  shuf_coll, max lnk ======",effective_lnk
#if defined(EZ_TEST)
!====================================================================================
! use previously obtained grid id
    status = RPN_COMM_shuf_ezcoll(setno, grid_id, global2, dnk, local, effective_lnk, liste_k2)
!====================================================================================
#else
    call RPN_COMM_shuf_coll(setno,  &
                            global2,gni,gnj,dnk,  &
                            local,mini,maxi,minj,maxj,effective_lnk,  &
                            liste_k2,  &
                            start_x,count_x,pe_nx,start_y,count_y,pe_ny,  &
                            status)
#endif
    if(status .ne. RPN_COMM_OK) then
      print 101,"ERROR: RPN_COMM_shuf_coll failure, lnk,dnk,n_io ",effective_lnk,dnk,n_io
      return
    endif
  enddo ! end of test repeat loop
! global2 should be identical to global once k has been adjusted
! expected k + global - mod(global,10)
  if(me_io .ne. -1) then ! I am an IO PE
    print *,"====== after shuf_coll ======"
    print *,"DEBUG: pe_me, liste_k2", pe_me,liste_k2
    nerrors = 0
    nvalid = 0
    do k = 1,dnk
      if(liste_k2(k) <= 0) cycle
      do j = 1,gnj
      do i = 1,gni
        expected = liste_k2(k) + j*100 + i*10000
        nvalid = nvalid + 1
        if(expected .ne. global2(i,j,k)) nerrors = nerrors + 1
        if(nerrors == 1) then
          print *,"ERROR: expected, got =",expected,global2(i,j,k)
        endif
      enddo
      enddo
    enddo
    print *,"nerrors, npoints =",nerrors,nvalid
    do k = 1, dnk
      if(liste_k2(k) > 0) then
        print *,"===== k, collected level",k,liste_k2(k),"  ====="
        do j = gnj , 1 , -1
          if(gni*gnj < MAX_PRINT) print 100,j,global2(:,j,k)
        enddo
      endif
    enddo
  endif
  return
end subroutine RPN_COMM_io_dist_coll_test

subroutine RPN_COMM_fast_dist_test(nparams,params)   ! fast distribute with halos
        use rpn_comm
        implicit none
        integer, intent(IN) :: nparams
        integer, intent(IN), dimension(nparams) :: params

        integer, dimension(:,:,:,:), allocatable :: garr
        integer :: ghalox,ghaloy,gmini,gmaxi,gmaxj,gminj
        integer :: nig,njg,size,mini,maxi,minj,maxj,nk
        integer :: status
        integer :: halox,haloy
        integer, dimension(:,:,:,:), allocatable :: larr
        logical :: periodx,periody
        integer, dimension(0:pe_nx) :: count_x, depl_x
        integer, dimension(0:pe_ny) :: count_y, depl_y
        integer :: ierr
        integer :: lmini, lmaxi, lminj, lmaxj
        integer :: i, j, k
        integer :: ii, jj
        integer :: nerrors, expected, npts
        integer :: n_halo, s_halo, e_halo, w_halo

        periodx = .false.
        periody = .false.
        halox   = 1
        w_halo  = halox
        if(pe_mex==0) w_halo = 0
        e_halo  = halox
        if(pe_mex==pe_nx-1) e_halo = 0
        haloy   = 1
        n_halo  = haloy
        if(pe_mey == pe_ny-1) n_halo = 0
        s_halo  = haloy
        if(pe_mey == 0) s_halo = 0
        ghalox  = halox
        ghaloy  = haloy
        nig     = 120
        gmini   = 1 - ghalox
        gmaxi   = nig + ghalox
        njg     = 60
        gminj   = 1 - ghaloy
        gmaxj   = njg + ghaloy
        nk      = 1
        size    = 1
        status = -9999
        if(pe_me == 0) then
          allocate(garr(1,gmini:gmaxi,gminj:gmaxj,nk))
          garr = 99099099
          do k=1,nk
          do j=1,njg
          do i=1,nig
            garr(1,i,j,k) = i*1000000 + j*1000 + k
          enddo
          enddo
          enddo
        else
          allocate(garr(1,1,1,1))
          garr = 88088088
        endif
        if(pe_me == 0 .and. nig < 13 .and. njg < 7) then
          print *,'Global array'
          do j = gmaxj,gminj,-1
            print 101,j,garr(1,gmini:gmaxi,j,1)
          enddo
        endif

        ierr =  RPN_COMM_limit(pe_mex, pe_nx, 1, nig , lmini, lmaxi, count_x, depl_x)
        mini = 1 - halox
        maxi = (lmaxi-lmini+1) + halox
        ierr =  RPN_COMM_limit(pe_mey, pe_ny, 1, njg , lminj, lmaxj, count_y, depl_y)
        minj = 1 - haloy
        maxj = (lmaxj-lminj+1) + haloy

        allocate(larr(1,mini:maxi,minj:maxj,nk))
        larr = 77077077

        call RPN_COMM_fast_dist(garr,gmini,gmaxi,gminj,      &
     &          gmaxj,nig,njg,nk,ghalox,ghaloy,size,         &
     &          larr,mini,maxi,minj,maxj,halox,haloy,        &
     &          periodx,periody,status)

        nerrors = 0
        npts = 0
!        print *,'DEBUG: mini, maxi, minj, maxj',mini,maxi,minj,maxj
!        print *,'DEBUG: lmini,lmaxi,lminj,lmaxj',lmini,lmaxi,lminj,lmaxj
        do k=1,nk
          do j=minj,maxj
            jj = (j+lminj-1)
            if(jj < 1 .or. jj > njg) cycle
            do i=mini,maxi
              ii = (i+lmini-1)
              expected = ii*1000000 + jj*1000 + k
              if(ii < 1 .or. ii > nig) cycle
              npts = npts + 1
              if(expected .ne. larr(1,i,j,k)) nerrors = nerrors + 1
            enddo
          enddo
        enddo
        if(pe_me == 0) then
          print *,'INFO: global ni,nj = ',nig,njg
        endif
        print *,'INFO: local ni,nj = ',count_x(pe_mex),count_y(pe_mey)
        print *,'INFO: npts, nerrors = ',npts,nerrors
        if(nerrors > 0) then
          print *,'Local array'
          do j = maxj, minj, -1
            print 101,j,larr(1,mini:maxi,j,1)
          enddo
        endif
101     format(I3,15I9)

        end subroutine
 
!=======================================================================
      integer function RPN_COMM_xch_halo_flip_test(nparams,params)
!=======================================================================
      use rpn_comm
      implicit none
      integer, intent(IN) :: nparams
      integer, intent(IN), dimension(nparams) :: params
      integer :: lni
      integer :: lnj
      integer :: nk
      integer :: gni, gnj
      logical :: periodx, periody
      integer, dimension(pe_nx) :: countx, offsetx
      integer, dimension(pe_ny) :: county, offsety
      integer :: halox, haloy, npol_row
      integer :: lminx, lmaxx, lminy, lmaxy
      integer :: minx, maxx, miny, maxy
      integer :: minx1, maxx1, miny1, maxy1
      integer, pointer, dimension(:,:,:) :: localarray,localarray2
      integer :: i, j, k, ierr
!
      RPN_COMM_xch_halo_flip_test=-1
      gni = params(1)
      gnj = params(2)
      nk = params(3)
      halox=params(4)
      haloy=params(5)
      periodx = .false.
      periody = .false.
      periodx = params(6).ne.0
      periody = params(7).ne.0

      npol_row = -999999
      ierr = RPN_COMM_limit(pe_mex,pe_nx,1,gni,lminx,lmaxx,countx,offsetx)
      lni = countx(pe_mex+1)
      ierr = RPN_COMM_limit(pe_mey,pe_ny,1,gnj,lminy,lmaxy,county,offsety)
      lnj = county(pe_mey+1)
      minx = lminx-halox ; minx1 = minx - lminx + 1
      maxx = lmaxx+halox ; maxx1 = maxx - lminx + 1
      miny = lminy-haloy ; miny1 = miny - lminy + 1
      maxy = lmaxy+haloy ; maxy1 = maxy - lminy + 1

      allocate(localarray(minx:maxx,miny:maxy,nk))
      allocate(localarray2(minx:maxx,miny:maxy,nk))
      localarray = 1111
      localarray = 2222
      do k = 1,nk
      do j = lminy,lmaxy
      do i = lminx,lmaxx
        localarray(i,j,k) = (i - 1)*(360.0/gni)
        localarray2(i,j,k) = -90 + (j - .5) * (180.0/(gnj))
      enddo
      enddo
      enddo
      do j = maxy,miny,-1
        print 101,j,localarray(minx:maxx,j,1),-999,localarray2(minx:maxx,j,1)
      enddo
101   format(40I5)
      call rpn_comm_xch_halo(localarray,minx1,maxx1,miny1,maxy1,lni,lnj,nk,halox,haloy,periodx,periody,gni,npol_row)
      call rpn_comm_xch_halo(localarray2,minx1,maxx1,miny1,maxy1,lni,lnj,nk,halox,haloy,periodx,periody,gni,npol_row)
      do j = maxy,miny,-1
        print 101,j,localarray(minx:maxx,j,1),-999,localarray2(minx:maxx,j,1)
      enddo

      RPN_COMM_xch_halo_flip_test=0
      return
      end function RPN_COMM_xch_halo_flip_test

!=======================================================================
      integer function RPN_COMM_xch_halo_test(nparams,params)
!=======================================================================
      use rpn_comm
      implicit none
      integer, intent(IN) :: nparams
      integer, intent(IN), dimension(nparams) :: params
!
      integer, pointer, dimension(:,:,:) :: localarray
      integer*8, pointer, dimension(:,:,:) :: localarray2
      integer :: lni
      integer :: lnj
      integer :: nk
      integer :: gni, gnj
      integer :: i, j, k, ierr
      integer :: lminx, lmaxx, lminy, lmaxy
      integer :: minx, maxx, miny, maxy
      integer :: minx1, maxx1, miny1, maxy1
      integer, dimension(pe_nx) :: countx, offsetx
      integer, dimension(pe_ny) :: county, offsety
      integer :: halox, haloy, npol_row, errors, value, ii, jj
      logical :: periodx, periody
!
      RPN_COMM_xch_halo_test=-1
      gni = params(1)
      gnj = params(2)
      nk = params(3)
      halox=params(4)
      haloy=params(5)
      periodx = .false.
      periody = .false.
      periodx = params(6).ne.0
      periody = params(7).ne.0
!
      ierr = RPN_COMM_limit(pe_mex,pe_nx,1,gni,lminx,lmaxx,countx,offsetx)
      lni = countx(pe_mex+1)
      ierr = RPN_COMM_limit(pe_mey,pe_ny,1,gnj,lminy,lmaxy,county,offsety)
      lnj = county(pe_mey+1)
      minx = lminx-halox ; minx1 = minx - lminx + 1
      maxx = lmaxx+halox ; maxx1 = maxx - lminx + 1
      miny = lminy-haloy ; miny1 = miny - lminy + 1
      maxy = lmaxy+haloy ; maxy1 = maxy - lminy + 1
      if(pe_me==pe_nx*pe_ny-1) write(rpn_u,100)  &
          'grid halo exchange test',  &
          pe_tot_grid,pe_nx,pe_ny,lminx,lmaxx,lminy,lmaxy,countx,county,  &
          minx1,maxx1,miny1,maxy1
100   format(A,25I5)
      allocate(localarray(minx:maxx,miny:maxy,nk))
      allocate(localarray2(minx:maxx,miny:maxy,nk))
!
      localarray = 99999
      localarray2 = 99999
      do k = 1,nk
      do j = lminy,lmaxy
      do i = lminx,lmaxx
        localarray(i,j,k) = k + 10*j + 1000*i
        localarray2(i,j,k) = k + 10*j + 1000*i
      enddo
      enddo
      enddo
      call mpi_barrier(MPI_COMM_WORLD,ierr)
!
      npol_row = 0
!      return
      call RPN_COMM_xch_halo(localarray,minx1,maxx1,miny1,maxy1, &
                   lni,lnj,nk,halox,haloy,periodx,periody,  &
                  gni,npol_row)

      call mpi_barrier(MPI_COMM_WORLD,ierr)
      if(pe_mex==0 .and. pe_mey==0)then
        do j=lmaxy+haloy,lminy-haloy,-1
          write(rpn_u,90)j,localarray(:,j,1)
        enddo
90      format(X,I5.4,20(X,I5.5))
      endif
      errors=0
      do k=1,nk
      do j=lminy-haloy,lmaxy+haloy
!      do j=lminy,lmaxy+1
      do i=lminx-halox,lmaxx+halox
!      do i=lminx,lmaxx
        ii = i
        if(i>gni .and. .not. periodx) cycle
        if(i<1   .and. .not. periodx) cycle
        if(i>gni) ii=i-gni
        if(i<1)   ii=i+gni
        jj = j
        if(jj>gnj .and. .not. periody) cycle
        if(jj<1   .and. .not. periody) cycle
        if(jj>gnj) jj=j-gnj
        if(jj<1)   jj=j+gnj
        value = k+10*jj+1000*ii
        if(localarray(i,j,k) /= value)errors=errors+1
      enddo
      enddo
      enddo
      call mpi_barrier(MPI_COMM_WORLD,ierr)
      write(rpn_u,100)'errors=',errors,pe_mex,pe_mey
!
      RPN_COMM_xch_halo_test=0
      return
      end function RPN_COMM_xch_halo_test
!=======================================================================

	subroutine rpncomm_test_001
	implicit none
	external :: RPN_COMM_grid_redist_test
	integer :: RPN_COMM_grid_redist_test
	external :: RPN_COMM_xch_halo_test
	integer :: RPN_COMM_xch_halo_test
	integer, external :: RPN_COMM_xch_halo_flip_test
	external RPN_COMM_init, TestUserInit, get_a_free_unit
        integer :: get_a_free_unit
	integer :: RPN_COMM_dist_test
	external RPN_COMM_dist_test
	integer :: Pelocal,Petotal,Pex,Pey,ierr,iun,test_to_perform
        integer :: nparams, i, ier, status
        integer, dimension(100) :: params
        character(len=256) :: RPN_COMM_TEST_CFG
        integer, external :: rpn_comm_2dgrid_test

        Pex = 0
        Pey = 0
!       UserInit supplied by TEST_helpers.f
        call RPN_COMM_init(TestUserInit,Pelocal,Petotal,Pex,Pey)
        print *,' Pelocal,Petotal,Pex,Pey =',Pelocal,Petotal,Pex,Pey

        call get_environment_variable("RPN_COMM_TEST_CFG",RPN_COMM_TEST_CFG,i,ier)
        if(ier==0) then
          read(RPN_COMM_TEST_CFG,FMT=*)test_to_perform,nparams,params(1:nparams)
        else
          iun=get_a_free_unit()
          open(UNIT=iun,FILE='TEST_001.cfg',STATUS='OLD')
          read(UNIT=iun,FMT=*)test_to_perform,nparams,params(1:nparams)
          close(UNIT=iun)
        endif
        if(IAND(test_to_perform,1)==1)then
          ierr=RPN_COMM_dist_test(Petotal)
        endif
        if(IAND(test_to_perform,2)==2)then
!          print *,'start grid_redist test'
          ierr=RPN_COMM_grid_redist_test(nparams,params)
        endif
        if(IAND(test_to_perform,4)==4)then
          print *,'start halo exchange test'
          ierr=RPN_COMM_xch_halo_test(nparams,params)
        endif
        if(IAND(test_to_perform,8)==8)then
          print *,'start haloflip exchange test'
          ierr=RPN_COMM_xch_halo_flip_test(nparams,params)
        endif
        if(IAND(test_to_perform,16)==16)then
          print *,'start fast distribution test'
          call RPN_COMM_fast_dist_test(nparams,params)
        endif
        if(IAND(test_to_perform,32)==32)then
          print *,'start shuffle distribution test'
          call RPN_COMM_io_dist_coll_test(nparams,params)
        endif
        if(IAND(test_to_perform,64)==64)then
          print *,'start 2D grid definition test'
         status = rpn_comm_2dgrid_test(nparams,params)
        endif
        if(IAND(test_to_perform,128)==128)then
          print *,'start one sided communications test'
          call RPN_COMM_i_win_test(nparams,params)
        endif
        call RPN_COMM_finalize(ierr)
        stop
        end
        subroutine TestUserInit(NX,NY) ! try to get NX,NY from file TEST.cfg if it exists
        external :: get_a_free_unit
        integer :: get_a_free_unit
        integer :: iun,ier,i
        character(len=128) :: RPN_COMM_TEST_SHAPE
        call get_environment_variable("RPN_COMM_TEST_SHAPE",RPN_COMM_TEST_SHAPE,i,ier)
        if(ier == 0) then
          read(RPN_COMM_TEST_SHAPE,*)NX,NY
          return
        endif
        iun=get_a_free_unit()
        if(iun<0)return
!        print *,'attempting to read TEST.cfg'
!        print *,'nx , ny =',nx,ny
        open(UNIT=iun,FILE='TEST.cfg',STATUS='OLD',ACTION='READ',IOSTAT=ier)
!        print *,'open iostat=',ier
        if(ier .ne. 0) then
!          print *,'attempting auto distribution, nx , ny =',nx,ny
          do i=7,2,-1
!            print *,'nx i mod(nx,i) =',nx, i, mod(nx,i)
            if(mod(NX,i) == 0 .and. nx .ne. i)then
              NY = NX / i
              NX = NX / NY
              return
            endif
          enddo
          return
        endif
        read(UNIT=iun,IOSTAT=ier,FMT=*)NX,NY
!        print *,'read iostat=',ier
        close(UNIT=iun)
        return
        end
        integer function get_a_free_unit()
        implicit none
        integer :: i
        character (len=16) :: access_mode
          get_a_free_unit=-1
          do i = 1 , 99  ! find an available unit number
            inquire(UNIT=i,ACCESS=access_mode)
            if(trim(access_mode) == 'UNDEFINED')then ! found
              get_a_free_unit = i
              exit
            endif
          enddo
        return
        end
