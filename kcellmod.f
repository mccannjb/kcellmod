      program kcellmod
c
c
c Program:	kcellmod    
c
c Language:	Fortran90
c Programmer:	James McCann (mccannjb (at) gmail (dot) com)
c Version:	0.3.2
c
c Note:		This is based on the maskpt utility developed by Bonyoung Koo and the
c icbcprep utility from ENVIRON
c
c Description:	This utility reads in an existing CAMx point-source emissions file 
c and modifies the hourly emission rates and kcell values for point-source locations
c matching those present in a provided KCELL list file. This utility outputs a new,
c modified CAMx point-source emissions file.
c 
c Inputs must be entered in the following order and format:
c [20 characters...ignored][Existing CAMx point-source file (string)]
c [20 characters...ignored][New CAMx point-source file (string)]
c [20 characters...ignored][Existing KCELL list file (string)]
c [20 characters...ignored][Emissions reduction factor (float [X.XX])]
c
c Additionally, the KCELL list file should be in the following format:
c [EGU X-coordinate (float)][space][EGU Y-coordinate (float)][space][K value (int)]
c REGEX: "-{0,1}[0-9]+\.[0-9]{4} -{0,1}[0-9]+\.[0-9]{4} [0-9]+"
c
c If using the kcellPrep python script to prepare KCELL list files, this
c will produce the properly formatted input.
c
c

      integer, parameter :: mxx=279, mxy=240, mxz=14, mxspec=760
      integer, parameter :: Mnstk=250000
      integer, parameter :: Mlines=10000, nvalues=3

      character*4 name(10)
      character*4 note(60)
      character*180 ifile
      character*4 mspec(10,mxspec)
      integer ione,nspec,ibdate,iedate,iutm
      integer nstk
      integer nx,ny,nz,idum,kcell(Mnstk)
      real xstk(Mnstk),ystk(Mnstk),hstk(Mnstk)
      real dstk(Mnstk),tstk(Mnstk),vstk(Mnstk)
      real flow(Mnstk),plmht(Mnstk)
      real ptems(Mnstk,mxspec)
      integer kcv(Mnstk)
      real btime,etime,rdum,xorg,yorg,delx,dely
      real klx,kly
      integer klk
      real,allocatable :: egux(:)
      real,allocatable :: eguy(:)
      integer,allocatable :: eguk(:)
      integer,allocatable :: klist(:)
      real rfac(Mnstk,mxspec)
      real fact
      integer negu
      integer ios
      integer countmatch
      character(len=1) :: junk

c     
c-----Read and open inputs
c
      write(*,*) 'Input path of original file: '
      read(*,'(20x,a)') ifile
      write(*,*) 'Opening file ',ifile
      open(10,file=ifile,status='old',form='UNFORMATTED')

      write(*,*) 'Path of output emiss file: '
      read(*,'(20x,a)') ifile
      write(*,*) 'Opening file ',ifile
      open(15,file=ifile,status='new',form='UNFORMATTED')

c      write(*,*) 'Path of kcell list file: '
c      read(*,'(20x,a)') ifile
c      write(*,*) 'Opening file ',ifile
c      open(11,file=ifile,status='old')

c      write(*,*) 'Emissions reduction factor (#): '
c      read(*,'(20x,F4.2)') fact

      write(*,*) 'Diagnostic file saved to kcell.diag'
      open(12,file='kcell.diag',status='REPLACE')

c  Count how many lines/records are present in the kcell list file
c      negu=0
c      do j=1,Mlines
c        read(11,*,IOSTAT=ios) junk
c        if (ios /= 0) EXIT
c        if (j == Mlines) then
c          write(*,*) "Error: Maximum numer of records exceeded..."
c          write(*,*) "Exiting program now..."
c          STOP
c        endif
c        negu = negu + 1
c      enddo
c      write(12,*) "Kcell-list file has ",negu,"lines"
c      rewind(11)
      
c
c  Allocate egux,eguy,eguk based on the number of records in kcell list file 
c 
c      allocate( egux(negu), stat=ios )
c      if (ios /= 0) STOP
c     & "***Not enough memory***: CSV parse"
c      allocate( eguy(negu), stat=ios )
c      if (ios /= 0) STOP
c     & "***Not enough memory***: CSV parse"
c      allocate( eguk(negu), stat=ios )
c      if (ios /= 0) STOP
c     & "***Not enough memory***: CSV parse"

c
c-----Parse CSV file
c
c      do i=1,negu
c        read(11,*) egux(i),eguy(i),eguk(i)
c      enddo

c      close(11)
c
c-----Read Headers Information from CAMx file
c-----Immediately write those headers into the new file
c

      read(10) name,note,ione,nspec,ibdate,btime,
     &         iedate,etime
      write(15) name,note,ione,nspec,ibdate,btime,
     &         iedate,etime

      read(10) rdum,rdum,iutm,xorg,yorg,delx,dely,
     &         nx,ny,nz,idum,idum,rdum,rdum,rdum
      write(15) rdum,rdum,iutm,xorg,yorg,delx,dely,
     &         nx,ny,nz,idum,idum,rdum,rdum,rdum

      read(10) ione,ione,nx,ny
      write(15) ione,ione,nx,ny

      read(10) ((mspec(n,l),n=1,10),l=1,nspec)
      write(15) ((mspec(n,l),n=1,10),l=1,nspec)

      read(10) ione,nstk
      write(15) ione,nstk

      write(12,*) "Date: ",ibdate

c      allocate( klist(nstk), stat=ios )
c      if (ios /= 0) STOP
c     & "***Not enough memory***: klist allocate"


c
c-----Check some dimensions
c
      if(nx .gt. mxx ) then
        write(*,*) "Increase MXX and recompile"
        stop
      endif
      if(ny .gt. mxy ) then
        write(*,*) "Increase MXY and recompile"
        stop
      endif
      if(nz .gt. mxz ) then
        write(*,*) "Increase MXZ and recompile"
        stop
      endif
      if(nspec .gt. mxspec) then
        write(*,*) "Increase MXSPEC and recompile"
        stop
      endif
      if(nstk .gt. Mnstk) then
        write(*,*) "Increase MNSTK and recompile"
        stop
      endif

c
c-----Read the existing NSTK Header, write to new file
c

      read(10) (xstk(n),ystk(n),hstk(n),dstk(n),
     & tstk(n),vstk(n),n=1,nstk)
      write(15) (xstk(n),ystk(n),hstk(n),dstk(n),
     & tstk(n),vstk(n),n=1,nstk)

c
c Iterate through stacks and find location matches with kcell list
c  if match: assign appropriate KCELL value and zero
c            emissions
c
c      write(12,*) "Comparing CAMx stack and EGU locations"
c-- Epsilon used for fuzzy matching of floating-point numbers
c   low precision needed for point location comparison here
c      epsilon=1E-2
c      countmatch=0
c      write(12,*) "Number of stacks: ",nstk
c      write(12,*) "-------------------"
c      do n=1,nstk
c        rfac(n,:)=1.0
c        klist(n)=0
c        do j=1,negu
c          if (abs(egux(j)-xstk(n)).lt.epsilon) then
c            write(12,*) "EGU Location (X,Y): ",egux(j),",",eguy(j)
c            write(12,*) "STK Location (X,Y): ",xstk(n),",",ystk(n)
c            klist(n)=eguk(j)
c            rfac(n,:)=fact
c            countmatch = countmatch + 1
c          endif
c        enddo
c      enddo
      
c      write(12,*) "Matched ",countmatch," points (incl. duplicates)"

c     
c-----Time Variant Portion
c     Read/write all time headers and data     
c
      write(12,*) "Reached time-variant portion"
 100  read(10,end=900) ibdate,btime,iedate,etime
      write(15)        ibdate,btime,iedate,etime

      read(10) ione,nstk
      write(15) ione,nstk

      read(10) (idum,idum,kcell(n),flow(n),plmht(n),n=1,nstk)

c  Iterate through each stack and assign appropriate kcell value
c  Note: because of CAMx requirements, this kcell value should be 
c        negative in order to be used for either OSAT/APCA or DDM
c        overrides.   
      do n=1,nstk
        if (kcell(n).eq.11) then
          write(12,*) "WARN: KCELL value of 11 encountered"
        endif
        if (kcell(n).gt.0) then
          kcell(n)=-1*(klist(n)+1)
        endif
      enddo

      write(15) (idum,idum,kcell(n),flow(n),plmht(n),n=1,nstk)

      do l=1,nspec

        read(10) ione,(mspec(n,l),n=1,10),
     &           (ptems(n,l),n=1,nstk)
        write(15) ione,(mspec(n,l),n=1,10),
     &            (ptems(n,l),n=1,nstk)

      enddo     
      
      goto 100

 900  write(12,*) "Finished reading file"

      close(10)
      close(15)

      end
