cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  For evolution of galaxies (test particle method) 
c    by Kenji Bekki, 2018/1/12
c
c  For Eleanor & Mark
c  Program name:  gint.f
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c /*
c#include <g5util.h>
c*/
c#define real real*8
c#define 2100000 (2000000)

c      program gint

      real xj(3, 2100000), vj(3, 2100000)
      real xwo(3),vwo(3),ang(10,3)
      real gp(10),gp1(10),unit(10),xp(3,2),xpt(3,10000,2)
      real a(3, 2100000)
      integer nj, i, j, k, iout
      integer nstep, step, imode, hmode, nmod,iext

      open(unit=1,file='time.para',status='old')
      open(unit=2,file='gpar.para',status='old')
      open(unit=4,file='unit.para',status='old')
      open(unit=7,file='gpot.para',status='old')
      open(unit=8,file='gpot1.para',status='old')

      read(1,*) endt,dt,imode,hmode

      read(2,*) (ang(1,k),k=1,3)
      read(2,*) (ang(2,k),k=1,3)
      read(2,*) (ang(3,k),k=1,2)

      read(4,*) (unit(k),k=1,3)


c-----For host galaxy potential
      do k=1,8
      read(7,*) gp(k)
      enddo
c-----For central galaxy potential
      do k=1,8
      read(8,*) gp1(k)
      enddo


      iext=int(gp(8))
c      write(6,*) 'iext=',iext
c      iext=2

      gp(1)=gp(1)/unit(2)
      gp(2)=gp(2)/unit(2)
      gp(3)=gp(3)/unit(2)
      gp(4)=gp(4)/unit(2)
      gp(5)=gp(5)/unit(1)
      gp(6)=gp(6)/unit(1)
      gp(7)=gp(7)/unit(3)
      write(6,*) 'unit',(unit(k),k=1,3)

      gp1(1)=gp1(1)/unit(2)
      gp1(2)=gp1(2)/unit(2)
      gp1(3)=gp1(3)/unit(2)
      gp1(4)=gp1(4)/unit(2)
      gp1(5)=gp1(5)/unit(1)
      gp1(6)=gp1(6)/unit(1)
      gp1(7)=gp1(7)/unit(3)

      write(6,*) 'gp',(gp(k),k=1,7)
      write(6,*) 'gp1',(gp1(k),k=1,7)


c------New version multiple-time step
c
c dt =  GC time step  dt_g= Galaxy time step= dt*nmod
c
c  id=1 for all GC stars id=1 only for nmod time steps
c
c------
      time = 0.0
      nstep = endt/dt
      iout=0

      nstep=nstep+1

cccc For initial data sets
      iout=iout+1
      call readorb(nstep0,xpt)
      call readnbody(nj, xj, vj, time)
      call inc(nj, xj, vj, ang)
      call writenbody(nj, xj, vj, time)
      if(nstep0.lt.nstep) then
      write(6,*) 'Error in nstep0',nstep0,nstep
      stop
      end if

      do k=1,3
      xp(k,1)=xpt(k,1,1)
      xp(k,2)=xpt(k,1,2)
      enddo

      call calc_ext(nj,xj,a,gp,xp)
      call calc_ext1(nj,xj,a,gp1,xp)

      do step=1,nstep
       
      do k=1,3
      xp(k,1)=xpt(k,step,1)
      xp(k,2)=xpt(k,step,2)
      enddo

      call push_velocity(vj, a, 0.5*dt, nj)
      call push_position(xj, vj, a, dt, nj)
      time = time + dt
      call calc_ext(nj,xj,a,gp,xp)
      call calc_ext1(nj,xj,a,gp1,xp)
      call push_velocity(vj, a, 0.5*dt, nj)

      if (mod(step, hmode).eq. 0) then
      write(6,*) 'time,dt',time,dt
      end if

      if (mod(step, imode).eq. 0) then
      call writenbody(nj, xj, vj, time)
      iout=iout+1
      end if

      enddo

      write(6,*) 'iout',iout

      end

c-----End of the program


c-----For velocity update

      subroutine push_velocity(vj, a, dt, nj)
      real vj(3, 2100000)
      real a(3, 2100000)
      real dt
      integer nj
      integer j, k

      do j=1,nj
	do k=1,3
	   vj(k,j) = vj(k,j) + dt*a(k,j)
	enddo
      enddo
      end

c-----For velocity update

      subroutine push_position(xj,vj,a,dt,nj)
      real xj(3, 2100000)
      real vj(3, 2100000)
      real a(3, 2100000)
      real dt
      integer nj
      integer j, k

      do j=1, nj
	 do k=1,3
	    xj(k,j) = xj(k,j) + dt*vj(k,j)
	 enddo
      enddo
      end

c-----  Input of orbital data

      subroutine readorb(nstep0,xpt)
      integer nstep0
      real xpt(3,10000,2)
      integer i,k

      open(unit=10,file='orbit.para',status='old')

c-----For host galaxy
c
c  For the central galaxy, xpos is always 0.
c
c-----
      read(10,*) nstep0
      do i=1,nstep0
      read(10,*) (xpt(k,i,1),k=1,3)

      do k=1,3
      xpt(k,i,2)=0.0
      enddo

      enddo

      end

c-----  Inclination of the disk galaxy

      subroutine inc(nj, xj, vj, ang)
      integer nj
      real xj(3, 2100000), vj(3, 2100000), ang(10,3)
      integer i

      pi=3.1415926535897
      theta1=2.*pi*ang(3,1)/360.
      phai1=2.*pi*ang(3,2)/360.


      do i=1,nj

c------For disk inclination
      theta=theta1
      phai=phai1
      posi1=xj(1,i)
      posi2=xj(2,i)
      posi3=xj(3,i)
      veli1=vj(1,i)
      veli2=vj(2,i)
      veli3=vj(3,i)
      xj(1,i)=cos(phai)*cos(theta)*posi1-sin(phai)*posi2
     &             -cos(phai)*sin(theta)*posi3
      xj(2,i)=sin(phai)*cos(theta)*posi1+cos(phai)*posi2
     &             -sin(phai)*sin(theta)*posi3
      xj(3,i)=sin(theta)*posi1+cos(theta)*posi3
      vj(1,i)=cos(phai)*cos(theta)*veli1-sin(phai)*veli2
     &              -cos(phai)*sin(theta)*veli3
      vj(2,i)=sin(phai)*cos(theta)*veli1+cos(phai)*veli2
     &             -sin(phai)*sin(theta)*veli3
      vj(3,i)=sin(theta)*veli1+cos(theta)*veli3

c------For initial galaxy position
      do k=1,3
      xj(k,i)=xj(k,i)+ang(1,k)
      vj(k,i)=vj(k,i)+ang(2,k)
      enddo


      enddo

      end

c-----  Input of position data

      subroutine readnbody(nj, xj, vj, time)
      integer nj
      real xj(3, 2100000), vj(3, 2100000)
      integer i
      real time

      open(unit=11,file='tinit.dat',status='old')

      read(11,*) nj,time
      do i=1,nj
      read(11,*)(xj(k,i),k=1,3), (vj(k,i),k=1,3)
      enddo

      end

c-----  Output of position data

      subroutine writenbody(nj, xj, vj, time)
      integer nj
      real xj(3, 2100000), vj(3, 2100000)
      real time
      integer i,k

c      open(unit=12,file='tout.dat',status='new')
      open(unit=12,file='tout.dat')


      tmp0=1.0/real(nj)
c      write(12,*) nj,time
c      do i=1,nj
c      write(12,*)(xj(k,i),k=1,3), (vj(k,i),k=1,3)
c      enddo
      write(12,601) nj,time,tmp0
      do i=1,nj
      write(12,602)(xj(k,i),k=1,3), (vj(k,i),k=1,3)
      enddo
 601  format(i10,2(1pe13.5))
 602  format(6(1pe13.5))
c 602  format(i3)
c 603  format(5(1pe13.5))
c 604  format(6(1pe13.5))
c 604  format(6(1pe13.5),i3)

      end


c-----   Calculations for  external  force from the Galaxy

      subroutine calc_ext(nj,xj,a,gp,xp)
      real xj(3, 2100000)
      real  a(3, 2100000),gp(10),xp(3,2)
      integer nj

      do 100 i=1,nj

c-----For initialization
      a(1,i) = 0.0
      a(2,i) = 0.0
      a(3,i) = 0.0


c-----From host galaxy
      x01=xj(1,i)-xp(1,1)
      x02=xj(2,i)-xp(2,1)
      x03=xj(3,i)-xp(3,1)
      r0=sqrt(x01**2.+x02**2.+x03**2.)

c---From halo
      p1=2.*r0*gp(7)**2.0
      p2=p1/(r0**2.+gp(4)**2.)
      fh=-p2

      fhx=x01/r0*fh
      fhy=x02/r0*fh
      fhz=x03/r0*fh

      a(1,i) = a(1,i)+fhx
      a(2,i) = a(2,i)+fhy
      a(3,i) = a(3,i)+fhz

c---From disk
      p1=x01*gp(5)
      p2=x01**2.+x02**2.
      p3=sqrt(x03**2.+gp(2)**2.)
      p4=(gp(1)+p3)**2.
      p5=(p2+p4)**1.5
      p6=p1/p5
      fhx=-p6

      p1=x02*gp(5)
      p2=x01**2.+x02**2.
      p3=sqrt(x03**2.+gp(2)**2.)
      p4=(gp(1)+p3)**2.
      p5=(p2+p4)**1.5
      p6=p1/p5
      fhy=-p6

c      p1=x03*gp(5)
      p2=x01**2.+x02**2.
      p3=sqrt(x03**2.+gp(2)**2.)
      p4=(gp(1)+p3)**2.
      p5=(p2+p4)**1.5
      p6=x03**2.+gp(2)**2.
      p7=gp(1)/sqrt(p6)
      p8=x03*(p7+1.0)
      p9=p8/p5
      fhz=-p9

      a(1,i) = a(1,i)+fhx
      a(2,i) = a(2,i)+fhy
      a(3,i) = a(3,i)+fhz


c---From bulge
      fh=-gp(6)/(r0+gp(3))**2.

      fhx=x01/r0*fh
      fhy=x02/r0*fh
      fhz=x03/r0*fh

      a(1,i) = a(1,i)+fhx
      a(2,i) = a(2,i)+fhy
      a(3,i) = a(3,i)+fhz

c      if(i.eq.1) then
c      write(6,*) 'a',(a(k,i),k=1,3)
c      stop
c      end if

 100  continue

      end 

c-----   Calculations for  external  force from the group central  

      subroutine calc_ext1(nj,xj,a,gp1,xp)
      real xj(3, 2100000)
      real  a(3, 2100000),gp1(10),xp(3,2)
      integer nj

      do 100 i=1,nj

c-----For initialization
c      a(1,i) = 0.0
c      a(2,i) = 0.0
c      a(3,i) = 0.0


c-----From central galaxy
      x01=xj(1,i)-xp(1,2)
      x02=xj(2,i)-xp(2,2)
      x03=xj(3,i)-xp(3,2)
      r0=sqrt(x01**2.+x02**2.+x03**2.)

c---From halo
      p1=2.*r0*gp1(7)**2.0
      p2=p1/(r0**2.+gp1(4)**2.)
      fh=-p2

      fhx=x01/r0*fh
      fhy=x02/r0*fh
      fhz=x03/r0*fh

      a(1,i) = a(1,i)+fhx
      a(2,i) = a(2,i)+fhy
      a(3,i) = a(3,i)+fhz

c---From disk
      p1=x01*gp1(5)
      p2=x01**2.+x02**2.
      p3=sqrt(x03**2.+gp1(2)**2.)
      p4=(gp1(1)+p3)**2.
      p5=(p2+p4)**1.5
      p6=p1/p5
      fhx=-p6

      p1=x02*gp1(5)
      p2=x01**2.+x02**2.
      p3=sqrt(x03**2.+gp1(2)**2.)
      p4=(gp1(1)+p3)**2.
      p5=(p2+p4)**1.5
      p6=p1/p5
      fhy=-p6

c      p1=x03*gp1(5)
      p2=x01**2.+x02**2.
      p3=sqrt(x03**2.+gp1(2)**2.)
      p4=(gp1(1)+p3)**2.
      p5=(p2+p4)**1.5
      p6=x03**2.+gp1(2)**2.
      p7=gp1(1)/sqrt(p6)
      p8=x03*(p7+1.0)
      p9=p8/p5
      fhz=-p9

      a(1,i) = a(1,i)+fhx
      a(2,i) = a(2,i)+fhy
      a(3,i) = a(3,i)+fhz


c---From bulge
      fh=-gp1(6)/(r0+gp1(3))**2.

      fhx=x01/r0*fh
      fhy=x02/r0*fh
      fhz=x03/r0*fh

      a(1,i) = a(1,i)+fhx
      a(2,i) = a(2,i)+fhy
      a(3,i) = a(3,i)+fhz

c      if(i.eq.1) then
c      write(6,*) 'a',(a(k,i),k=1,3)
c      stop
c      end if

 100  continue

      end 
