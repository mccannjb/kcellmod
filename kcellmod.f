      program kcellmod
c Program:		kcellmod    
c
c
c Language:		Fortran
c Programmer:	James McCann
c Version:		0.3
c
c This is based on the maskpt utility developed by Bonyoung Koo and the
c icbcprep utility from ENVIRON
c 
      integer, parameter :: mxx=279, mxy=240, mxz=14, mxspec=760
      integer, parameter :: Mnstk=250000
      integer, parameter :: Mlines=10000, nvalues=3

      character*4 name(10)
      character*4 note(60)
      character*80 ifile
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
      integer negu
      integer ios
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

      write(*,*) 'Path of kcell list file: '
      read(*,'(20x,a)') ifile
      write(*,*) 'Opening file ',ifile
      open(11,file=ifile,status='old')
      negu=0
      do j=1,Mlines
        read(11,*,IOSTAT=iso) junk
        if (ios /= 0) EXIT
        if (j == Mlines) then
          write(*,*) "Error: Maximum numer of records exceeded..."
          write(*,*) "Exiting program now..."
          STOP
        endif
        negu = negu + 1
      enddo
      rewind(11)
      
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
c-----Read/Write Headers Information
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

      allocate( klist(nstk), stat=ios )
      if (ios /= 0) STOP
     & "***Not enough memory***: klist allocate"


c
c-----Check some dimensions
c
       if(nx .gt. mxx ) then
         write(*,*) 'Increase MXX and recompile'
         stop
       endif
       if(ny .gt. mxy ) then
         write(*,*) 'Increase MXY and recompile'
         stop
       endif
       if(nz .gt. mxz ) then
         write(*,*) 'Increase MXZ and recompile'
         stop
       endif
       if(nspec .gt. mxspec) then
         write(*,*) 'Increase MXSPEC and recompile'
         stop
       endif
       if(nstk .gt. mxpt) then
         write(*,*) 'Increase MXPT and recompile'
         stop
       endif

c
c-----NSTK Header
c

      read(10) (xstk(n),ystk(n),hstk(n),dstk(n),
     & tstk(n),vstk(n),n=1,nstk)
      write(15) (xstk(n),ystk(n),hstk(n),dstk(n),
     & tstk(n),vstk(n),n=1,nstk)

c
c-----Iterate through stacks and find location matches
c-----  if match: assign appropriate KCELL value and zero
c-----            emissions
c

      do n=1,nstk
        rfac(n,:)=1.0
        klist(n)=0
        do j=1,negu
          if ((egux(j).eq.xstk(n)).AND.(eguy(j).eq.ystk(n))) then
            klist(n)=eguk(j)
            rfac(n,:)=0.0
          endif
        enddo
      enddo

c     
c-----Time Variant Portion     
c
      write(*,*) 'Starting time-variant portion'
 100  read(10,end=900) ibdate,btime,iedate,etime
      write(15)        ibdate,btime,iedate,etime

      read(10) ione,nstk
      write(15) ione,nstk

      read(10) (idum,idum,kcell(n),flow(n),plmht(n),n=1,nstk)
      
      do n=1,nstk
        kcell(n)=-1*klist(n)
      enddo

      write(15) (idum,idum,kcell(n),flow(n),plmht(n),n=1,nstk)

      do l=1,nspec
        read(10) ione,(mspec(n,l),n=1,10),
     &           (ptems(n,l),n=1,nstk)

        ptems=ptems*rfac

        write(15) ione,(mspec(n,l),n=1,10),
     &            (ptems(n,l),n=1,nstk)

      enddo     
      
      goto 100

 900  write(*,*) 'End of File'

      close(10)
      close(15)

      end
