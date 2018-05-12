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
c      if(ifile.eq.1) then
      nfile=6
        open(11,file='2df.dat.r1')
        open(12,file='2df.dat.r2')
        open(13,file='2df.dat.r3')
        open(14,file='2df.dat.r4')
        open(15,file='2df.dat.r5')
        open(16,file='2df.dat.r6')
c      end if

      
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

      if(ij.eq.1.or.ij.eq.4) then
      write(nk02,*) 1
      end if
      if(ij.eq.2.or.ij.eq.5) then
      write(nk02,*) 2
      end if
      if(ij.eq.3.or.ij.eq.6) then
      write(nk02,*) 3
      end if

      do 20 j=1,imesh*imesh
      read(nk1,*) fv
      write(nk01,*) fv
 20   continue
 10   continue
 5    continue

      end
