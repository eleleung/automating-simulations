cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  For evolution of galaxies (test particle method) 
c    by Kenji Bekki, 2018/1/12
c
c  Ram pressure version
c  For Eleanor & Mark
c  Program name:  gint2.f
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c /*
c#include <g5util.h>
c*/
c#define real real*8
c#define 2100000 (2000000)

c      program gint

      real xj(3, 2100000), vj(3, 2100000),d(2100000)
      real gp(10),unit(10),rp(10),rpara(10),gpara(10)
      real a(3, 2100000),time,tmgi
      integer nj, i, j, k, iout
      integer nstep, step, imode, hmode, nmod,iext

      open(unit=1,file='time.para',status='old')
      open(unit=2,file='ram.para',status='old')
      open(unit=3,file='dg2.para',status='old')
      open(unit=4,file='unit.para',status='old')
      open(unit=7,file='gpot.para',status='old')

      read(1,*) endt,dt,imode,hmode
      read(4,*) (unit(k),k=1,3)

      read(2,*) rpara(1),rpara(2)
      read(2,*) rpara(3)
      read(2,*) rpara(4)
      read(2,*) rpara(5)
      read(2,*) rpara(6)
      read(2,*) rpara(7)
      read(2,*) rpara(8)
      read(2,*) rpara(9)

      rpara(3)=rpara(3)/unit(3)
      rpara(4)=rpara(4)/2.21
      write(6,*) 'rpara(3-4)',rpara(3),rpara(4)

c----- For gas mass etc
      read(3,*) gpara(1)
      read(3,*) gpara(2),gpara(3)
      read(3,*) gpara(4),gpara(5)
      read(3,*) ngas
      tmgi=gpara(1)/real(ngas)
      write(6,*) 'tmgi,gpara(1)',tmgi,gpara(1)


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
c      write(6,*) 'unit',(unit(k),k=1,3)
c      write(6,*) 'gp',(gp(k),k=1,7)
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
      ih=0

      nstep=nstep+1

cccc For initial data sets
      iout=iout+1
      call ramp(rpara,rp)
      call readnbody(nj, xj, vj, d,time,gpara)
      call writenbody(nj, xj, vj, time, tmgi)

      call calc_ext(nj,xj,a,gp)
      call ramr(nj,xj,vj,a,d,rp,gpara)

      call ram(nj,xj,vj,a,d,rp)

      do step=1,nstep

      call push_velocity(vj, a, 0.5*dt, nj)
      call push_position(xj, vj, a, dt, nj)
      time = time + dt
      call calc_ext(nj,xj,a,gp)
      call ram(nj,xj,vj,a,d,rp)
      call push_velocity(vj, a, 0.5*dt, nj)

      if (mod(step, hmode).eq. 0) then
c     write(6,*) 'time',time
      ih=ih+1
      call strip(nj,xj,gpara,time)
      end if

      if (mod(step, imode).eq. 0) then
      call writenbody(nj, xj, vj, time, tmgi)
      iout=iout+1
      end if



      enddo


      write(6,*) 'iout,ih',iout,ih

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

c----- For parameter input for ram pressure 

      subroutine ramp(rpara,rp)
      real rpara(10),rp(10) 
      integer i

      pi=3.1415926535897

      theta=2.*pi*rpara(1)/360.
      phai=2.*pi*rpara(2)/360.
      posi1=rpara(3)
      posi2=0.0
      posi3=0.0
      rp(1)=cos(phai)*cos(theta)*posi1-sin(phai)*posi2
     &             -cos(phai)*sin(theta)*posi3
      rp(2)=sin(phai)*cos(theta)*posi1+cos(phai)*posi2
     &             -sin(phai)*sin(theta)*posi3
      rp(3)=sin(theta)*posi1+cos(theta)*posi3

      rp(4)=rpara(4)
      write(6,*)'rp(1-3)',(rp(k),k=1,3)
      write(6,*)'rp(4)',(rp(k),k=4,4)

      end

c-----  Input of position data

      subroutine readnbody(nj, xj, vj,d, time, gpara)
      integer nj
      real xj(3, 2100000), vj(3, 2100000),d(2100000)
      real gpara(10)
      integer i
      real time,dmin,dmax

      open(unit=11,file='tinit.dat',status='old')

      pi=3.1415926535897

c----- For disk surface gas density
c----- Central surface density estimation
      tmg=gpara(1)
      rdisk=gpara(2)
      rsca=gpara(4)
      s01=(1.+rdisk/rsca)*exp(-rdisk/rsca)
      s02=tmg/2./pi/rsca**2/(1.-s01)
      write(6,*) 'Central gas density',s02

      dmin=1.e10
      dmax=-dmin
      read(11,*) nj,time
      do i=1,nj
      read(11,*)(xj(k,i),k=1,3), (vj(k,i),k=1,3)
      r0=0.0
      do k=1,3
      r0=r0+xj(k,i)**2
      enddo
      r0=sqrt(r0)

      d(i)=s02*exp(-r0/rsca)
      dmin=min(dmin,d(i))
      dmax=max(dmax,d(i))

      enddo

      write(6,*) 'dmin,dmax',dmin,dmax

c-----Checking the total mass of gas

      tmg1=s02*2*pi*rsca**2
      write(6,*) 'tmg1,tmg',tmg1,tmg
      
      end

c-----  Output of position data

      subroutine writenbody(nj, xj, vj, time, tmgi)
      integer nj
      real xj(3, 2100000), vj(3, 2100000)
      real time,tmgi
      integer i,k

c      open(unit=12,file='tout.dat',status='new')
      open(unit=12,file='tout.dat')


      write(12,*) nj,time,tmgi
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


c-----   Calculations for ram pressure force 

      subroutine  ram(nj,xj,vj,a,d,rp)
      real xj(3, 2100000),vj(3,2100000),d(2100000)
      real  a(3, 2100000),rp(10),vr(3)
      real  costh
      integer nj
      open(unit=13,file='test.dat')

      itot=0
      rgr_min=1.e10
      rgr_max=-rgr_min
      do i=1,nj

c-----For inter-product
      costh=0.0
      do k=1,3
      costh=costh+vj(k,i)*rp(k)
      enddo

      do k=1,3
      vr(k)=rp(k)-costh*vj(k,i)
      enddo

      vrt=0.0
      r0=0.0
      do k=1,3
      vrt=vrt+vr(k)**2
      r0=r0+xj(k,i)**2
      enddo
      r0=sqrt(r0)
c      vrt=sqrt(vrt)
      
      rpt=0.0
      do k=1,3
      rpt=rpt+rp(k)**2
      enddo
      rpt=sqrt(rpt)

c-----For gravitational force 

      atg=0.0
      do k=3,3
      atg=atg+a(k,i)**2
      enddo
      atg=sqrt(atg)

      
c-----For ram pressure force (d(i)=surface gas mass density)

      atr=0.0
      do k=3,3
      atr0=vrt*rp(4)*vrt*rp(k)/rpt/d(i)
      atr=atr+atr0**2
      enddo
      atr=sqrt(atr)

      do k=1,3
      a(k,i)=a(k,i)+vrt*rp(4)*vrt*rp(k)/rpt/d(i)
      enddo


c      if(i.eq.1) then
c      write(13,*) 'costh,atg,atr,atr/atg,r0,d(i)'
c      write(13,*) 'rp',(rp(k),k=1,3)
c      end if

c      rgr=atr/atg
c      rgr_min=min(rgr_min,rgr)
c      rgr_max=max(rgr_max,rgr)

c      if(rgr.ge.2) then
c      itot=itot+1
c      write(6,*) itot,rgr,atg,atr,r0,abs(xj(3,i))
c      end if

c      write(13,*) costh,atg,atr,atr/atg,r0,d(i)
c      write(13,*) atr/atg,atg,atr,r0,abs(xj(3,i))
c      write(13,*) 'vj',(vj(k,i),k=1,3)
c      write(13,*) 'rp',(rp(k),k=1,3)
      enddo

c      write(6,*) 'rgr_min,max',rgr_min,rgr_max
c      write(6,*) 'itot/nj',real(itot)/real(nj)

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

c-----   Calculations for stripped gas particle number

      subroutine  strip(nj,xj,gpara,time)
      real xj(3, 2100000)
      real time,gpara(10)
      integer nj

      open(unit=14,file='strip.dat')

      nstrip=0
      rstrip=gpara(2)*1.01
      do i=1,nj
      r0=0.0
      do k=1,3
      r0=r0+xj(k,i)**2
      enddo
      r0=sqrt(r0)
      if(r0.gt.rstrip) then
      nstrip=nstrip+1
      end if
      enddo

      write(6,*) 'time,f_strip',time,real(nstrip)/real(nj)
      write(14,*) time,real(nstrip)/real(nj)

      end

c-----   Calculations for the relative important of RPS

      subroutine  ramr(nj,xj,vj,a,d,rp,gpara)
      real xj(3, 2100000),vj(3,2100000),d(2100000)
      real  a(3, 2100000),rp(10),vr(3),gpara(10)
      real  costh
      integer nj

      rs1=gpara(4)-gpara(2)*0.02
      rs2=gpara(4)+gpara(2)*0.02
      atg_max=0.0
      atgm=0.0
      atrm=0.0
      ise=0
      write(6,*) 'rs1,rs2',rs1,rs2

      do i=1,nj

      r0=0.0
      do k=1,3
      r0=r0+xj(k,i)**2
      enddo
      r0=sqrt(r0)

c-----For selection of gas particles
      if(r0.ge.rs1.and.r0.le.rs2) then
      ise=ise+1

      costh=0.0
      do k=1,3
      costh=costh+vj(k,i)*rp(k)
      enddo

      do k=1,3
      vr(k)=rp(k)-costh*vj(k,i)
      enddo

      vrt=0.0
      do k=1,3
      vrt=vrt+vr(k)**2
      enddo
      
      rpt=0.0
      do k=1,3
      rpt=rpt+rp(k)**2
      enddo
      rpt=sqrt(rpt)

c-----For gravitational force 

      atg=0.0
      do k=3,3
      atg=atg+a(k,i)**2
      enddo
      atg=sqrt(atg)

      atg_max=max(atg_max,atg)
      atgm=atgm+atg
      
c-----For ram pressure force (d(i)=surface gas mass density)

      atr=0.0
      do k=3,3
      atr0=vrt*rp(4)*vrt*rp(k)/rpt/d(i)
      atr=atr+atr0**2
      enddo
      atr=sqrt(atr)

      atrm=atrm+atr

      end if

      enddo

c------For mean gravitational force and RPS force
      if(ise.ne.0) then
      atrm=atrm/real(ise)
      atgm=atrg/real(ise)
      rforce=atrm/atgm
      rforce1=atrm/atgm_max
      write(6,*) 'ise,atrm,atgm',ise,atrm,atgm
      write(6,*) 'rforce,atgm_max',rforce,atgm_max
      write(6,*) 'rforce (max)',rforce1
      end if

       end
