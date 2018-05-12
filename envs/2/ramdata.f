cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c  For data output for RPS model
c        by 2016/1/23
c
c
c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	character*20 string1,string2,string3
        dimension  x1(1555555,3),v1(1555555,3),xwo(3),vwo(3),
     &        xcen(3),tmp(1555555),iw(1555555),eps(10),smass(10)
          
        NDIM=3
        pi=3.14159265359
c        write(6,*)'Input file names ;particle'
c        read(5,*) string1
c        open(1,file=string1)
        open(1,file='tout.dat')
        open(2,file='tout.datv')
        open(3,file='sfpt.para')
        open(4,file='ramdata.para')
        read(3,*) nstep
        read(4,*) icen
c        write(6,*) 'input step number'
c        read(5,*) nstep
c        write(6,*) 'Input favorite time step'
c        read(5,*) ifav
c        write(6,*) 'Center of mass: Bulge (1) or Disk (2) ?'
c        read(5,*) icen
        if(icen.eq.1) then
        icom=4
        else
        icom=1
        end if
        rnuc=1.0

         nk=1
         do 10 i=1,nstep

	  read(nk,*) num,tnow
	  read(nk,*) nspe
	  read(nk,*) (eps(j),j=1,nspe)
          read(nk,*) (smass(j),j=1,nspe)
	  write(6,*) 'tnow,num,nspe',tnow,num,nspe

      itest=0
      ngs=0
      do 15 k=1,3
      xcen(k)=0.0
 15   continue

      do 20 j=1,num
          read(nk,*) (xwo(k),k=1,3),(vwo(k),k=1,3),
     &    iwas,iwa,tmi,h0,d0,c0

c      if(iwas.eq.1.and.itest.eq.0) then
      if(iwas.eq.icom.and.itest.eq.0) then
      itest=j
      do 25 k=1,3 
      xcen(k)=xwo(k)
 25   continue
      end if

      if(iwas.ge.1.and.iwas.le.6) then
      ngs=ngs+1
      iw(ngs)=iwas
      tmp(ngs)=tmi
      do 30 k=1,3
      x1(ngs,k)=xwo(k)
      v1(ngs,k)=vwo(k)
 30   continue
      end if

 20        continue

      write(6,*) 'itest',itest
c      if(i.eq.nstep) then

      do 35 j=1,ngs
      do 36 k=1,3
      x1(j,k)=x1(j,k)-xcen(k)
 36   continue
 35   continue

      nkk=2
      write(nkk,*) ngs,tnow
      do 40 j=1,ngs
      write(nkk,*) (x1(j,k),k=1,3),(v1(j,k),k=1,3),
     &       iw(j),tmp(j)
c      if(iw(j).eq.4) write(6,*) 'bulge found'
 40   continue

c      end if

 10   continue

      end
