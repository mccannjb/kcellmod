      program kcellmod
c
c
c Program:	kcellmod    
c
c Language:	Fortran90
c Programmer:	James McCann (mccannjb (at) gmail (dot) com)
c Version:	0.3.2-HOURMOD
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
      real,allocatable :: ptems(:,:)
      integer kcv(Mnstk)
      real btime,etime,rdum,xorg,yorg,delx,dely
      real klx,kly
      integer klk
      real,allocatable :: egux(:)
      real,allocatable :: eguy(:)
      integer,allocatable :: eguk(:)
      integer,allocatable :: klist(:)
      real fact
      integer negu
      integer ios
      integer countmatch
      character(len=1) :: junk
      integer hrBefore
      integer doi
      integer hour
      integer doHour
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
c      open(15,file=ifile,status='new',form='UNFORMATTED')
      open(15,file=ifile,status='replace',form='UNFORMATTED')

      write(*,*) 'Path of kcell list file: '
      read(*,'(20x,a)') ifile
      write(*,*) 'Opening file ',ifile
      open(11,file=ifile,status='old')

      write(*,*) 'Emissions reduction factor (#): '
      read(*,'(20x,F4.2)') fact

      write(*,*) 'Day of Interest (eg. 05215): '
      read(*,'(20x,I5)') doi

      write(*,*) 'Hours Before (eg. 12 or 02): '
      read(*,'(20x,I2)') hrBefore

      write(*,*) 'Diagnostic file saved to kcell.diag'
      open(12,file='kcell.diag',status='REPLACE')

c  Count how many lines/records are present in the kcell list file
      negu=0
      do j=1,Mlines
        read(11,*,IOSTAT=ios) junk
        if (ios /= 0) EXIT
        if (j == Mlines) then
          write(*,*) "Error: Maximum numer of records exceeded..."
          write(*,*) "Exiting program now..."
          STOP
        endif
        negu = negu + 1
      enddo
      write(12,*) "Kcell-list file has ",negu,"lines"
      rewind(11)
      
c
c  Allocate egux,eguy,eguk based on the number of records in kcell list file 
c 
      allocate( egux(negu), stat=ios )
      if (ios /= 0) STOP
     & "***Not enough memory***: CSV parse"
      allocate( eguy(negu), stat=ios )
      if (ios /= 0) STOP
     & "***Not enough memory***: CSV parse"
      allocate( eguk(negu), stat=ios )
      if (ios /= 0) STOP
     & "***Not enough memory***: CSV parse"

c
c-----Parse CSV file
c
      do i=1,negu
        read(11,*) egux(i),eguy(i),eguk(i)
      enddo

      close(11)
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

      allocate( klist(nstk), stat=ios )
      if (ios /= 0) STOP
     & "***Not enough memory***: klist allocate"

      allocate( ptems(nstk,nspec), stat=ios )
      if (ios /= 0) STOP
     & "***Not enough memory***; ptems allocate"
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
      write(12,*) "Comparing CAMx stack and EGU locations"
c-- Epsilon used for fuzzy matching of floating-point numbers
c   low precision needed for point location comparison here
      epsilon=1E-2
      countmatch=0
      write(12,*) "Number of stacks: ",nstk
      write(12,*) "-------------------"
      do n=1,nstk
        klist(n)=0
        do j=1,negu
          if (abs(egux(j)-xstk(n)).lt.epsilon) then
            write(12,*) "EGU Location (X,Y): ",egux(j),",",eguy(j)
            write(12,*) "STK Location (X,Y): ",xstk(n),",",ystk(n)
            klist(n)=eguk(j)
            countmatch = countmatch + 1
          endif
        enddo
      enddo
      
      write(12,*) "Matched ",countmatch," points (incl. duplicates)"

c     
c-----Time Variant Portion
c     Read/write all time headers and data     
c
      write(12,*) "Reached time-variant portion"
 100  read(10,end=900) ibdate,btime,iedate,etime
      write(15)        ibdate,btime,iedate,etime

      hour=INT(btime)
      write(12,*) "Hour is:",hour
      if ((ibdate.ge.doi).and.(btime.ge.hrBefore)) then
        doHour=0
        write(12,*) doi,":",hrBefore
        write(12,*) "Zeroing out emissions after",hour,"on",ibdate
      else if (ibdate.gt.doi) then
        doHour=0
        write(12,*) "Zeroing out emissions:",ibdate,">",doi
      else
        doHour=1
        write(12,*) "Including emissions",INT(btime),ibdate
      endif

      read(10) ione,nstk
      write(15) ione,nstk

      read(10) (idum,idum,kcell(n),flow(n),plmht(n),n=1,nstk)

c  Iterate through each stack and assign appropriate kcell value
c  Note: because of CAMx requirements, this kcell value should be 
c        negative in order to be used for either OSAT/APCA or DDM
c        overrides.

cc
cc WHERE statement may work here instead of iterating, look into it!
cc   
      do n=1,nstk
        kcell(n)=-1*klist(n)
      enddo
      write(15) (idum,idum,kcell(n),flow(n),plmht(n),n=1,nstk)

      do l=1,nspec
        read(10) ione,(mspec(n,l),n=1,10),
     &           (ptems(n,l),n=1,nstk)

        if ((l.eq.3) .or. (l.eq.4)) then
          where (kcell .lt. 0) ptems(:,l)=ptems(:,l)*doHour
        endif

        write(15) ione,(mspec(n,l),n=1,10),
     &            (ptems(n,l),n=1,nstk)

      enddo     
      
      goto 100

 900  write(12,*) "Finished reading file"

      close(10)
      close(15)

      end
