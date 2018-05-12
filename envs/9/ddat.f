cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c    Output of selected data for AI Simulations 
c     by Kenji Bekki 2018/2/1
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
        character*30 string
        dimension x1(1500000,3),v1(1500000,3),ifav(20),xcen(3)
          
        pi=3.14159265359
c	write(6,*) 'input file name'
c        read(5,*) string 
c         tunit=0.141
c       open(1,file=string)
        open(1,file='tout.dat')
        open(2,file='dr.dat')
        open(3,file='ddat.para')
        open(4,file='sfpt.para')

c	write(6,*) 'input step number'
c       read(5,*) nstep
      read(3,*) nstep

      read(4,*) nstep0
      read(4,*) ntheta
      read(4,*) nphai
      read(4,*) vmax_wo
      read(4,*) icen
      do 5 i=1,nstep0
      read(4,*) ifav(i)
 5    continue



         do 10 i=1,nstep
          nk=1
	  read(nk,*) num,tnow,tmp0
	  write(6,*) i,num,tnow,tmp0
          xcen(1)=0.0
          xcen(2)=0.0
          xcen(3)=0.0
          do 20 j=1,num
            read(nk,*) (x1(j,k),k=1,3),
     &       (v1(j,k),k=1,3)
         if(j.eq.1) then
         do 24 k=1,3
         xcen(k)=x1(j,k)
 24      continue
         end if

 20      continue
      write(6,*) 'xcen',(xcen(k),k=1,3)

      do 30 kk=1,nstep0

      if(i.eq.ifav(kk)) then
      write(6,*) 'ifav',ifav(kk)
      nkk=2

      write(nkk,*) num,tnow,tmp0
      do 100 j=1,num
      if(icen.eq.1) then
      do 110 k=1,3
      x1(j,k)=x1(j,k)-xcen(k)
 110  continue
      end if
      write(nkk,*) (x1(j,k),k=1,3),
     &       (v1(j,k),k=1,3)
 100     continue
      write(6,*) 'kk,nnew',kk,nnew
      end if

 30   continue

 10       continue

	end
