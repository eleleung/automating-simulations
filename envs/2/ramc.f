cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c     For smoothed 2d Density field of the simulated dusty galaxy
c      by Kenji Bekki 2014/4/1: Dust-regulated SF
c    New version for disk + bulge 2017/10/27      
c------ For AI simulation revised, 2018/1/30
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c	character*20 string1,string2,string3
c        dimension  xw(1100000,3),vw(1100000,3)

        open(1,file='2dft.dat')
        open(2,file='2dftn.dat')
c      ifile=1
c      write(6,*) 'all (1) or Disk+ring (2) or ring + stream(3) ?'      
c      read(5,*) ifile
      write(6,*) 'Density (1) or Kinematic (2) ?'      
      read(5,*) ifile
      if(ifile.eq.1) then
      nfile=3
        open(11,file='2df.dat.ram1')
        open(12,file='2df.dat.ram2')
        open(13,file='2df.dat.ram3')
      end if
      if(ifile.eq.2) then
      nfile=3
        open(11,file='2df.dat.ram4')
        open(12,file='2df.dat.ram5')
        open(13,file='2df.dat.ram6')
      end if
      if(ifile.eq.3) then
      nfile=2
        open(11,file='2df.dat.ram2')
        open(12,file='2df.dat.ram3')
      end if

      
c         write(6,*) 'Total file number for each model'
c         read(5,*) nmodel
c         write(6,*) 'Mesh number'
c         read(5,*) imesh
      nmodel=100
      imesh=50

      do 5 ij=1,nfile
      nk01=1
      nk02=2
      nk1=10+ij
      nk2=20+ij
      do 10 i=1,nmodel
c      read(nk2,*) ifv
      write(nk02,*) ij
      do 20 j=1,imesh*imesh
      read(nk1,*) fv
      write(nk01,*) fv
 20   continue
 10   continue
 5    continue

      end
