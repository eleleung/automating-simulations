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
      real gp(10),unit(10)
      real a(3, 2100000)
      integer nj, i, j, k, iout
      integer nstep, step, imode, hmode, nmod,iext

      open(unit=1,file='time.para',status='old')
      open(unit=4,file='unit.para',status='old')
      open(unit=7,file='gpot.para',status='old')

      read(1,*) endt,dt,imode,hmode


      read(4,*) (unit(k),k=1,3)


c      do 1 k=1,7
      do 1 k=1,8
      read(7,*) gp(k)
 1    continue

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
      write(6,*) 'gp',(gp(k),k=1,7)
c      stop

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
      call readnbody(nj, xj, vj, time)
      call writenbody(nj, xj, vj, time)

      call calc_ext(nj,xj,a,gp)

      do step=1,nstep

      call push_velocity(vj, a, 0.5*dt, nj)
      call push_position(xj, vj, a, dt, nj)
      time = time + dt
      call calc_ext(nj,xj,a,gp)
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


      write(12,*) nj,time
      do i=1,nj
      write(12,*)(xj(k,i),k=1,3), (vj(k,i),k=1,3)
      enddo
c      write(12,601) nj,time
c      do i=1,nj
c      write(12,604)(xj(k,i),k=1,3), (vj(k,i),k=1,3)
c      enddo
c 601  format(i10,1(1pe13.5))
c 602  format(i3)
c 603  format(5(1pe13.5))
c 604  format(6(1pe13.5))
c 604  format(6(1pe13.5),i3)

      end


c-----   Calculations for  external  force from the Galaxy

      subroutine calc_ext(nj,xj,a,gp)
      real xj(3, 2100000)
      real  a(3, 2100000),gp(10)
      integer nj

      do 100 i=1,nj

c-----For initialization
      a(1,i) = 0.0
      a(2,i) = 0.0
      a(3,i) = 0.0



      x01=xj(1,i)
      x02=xj(2,i)
      x03=xj(3,i)
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

