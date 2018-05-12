cccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c
c
c      For iniital dinsity profile of an exponential disk 
c        revised  by Kenji Bekki, 2009/1/11
c      History of revise:  2010/5/15  soft.dini.para output
c
c-----Revised on 2018/1/30 for Reinforcment learning
c-----For ring or disk distirbuion
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccc

       dimension  x1(1200000,3),v1(1200000,3),id(1200000),
     &  rnt(1000),rbin(1000)
          pi=3.14159265359
       open(1,file='dr.para')  
       open(2,file='dr.dat')  
       open(3,file='2dfn.dat')  
       open(4,file='sfpt.para')  

       read(1,*) dmass
       read(1,*) rcutd,rcutdz 
       read(1,*) scadisk,scadiskz 
       read(1,*) numd 
       read(1,*) iring
       read(1,*) rcin
       read(1,*) rcout
       read(1,*) thin
       read(1,*) thout

       write(6,*) 'iring,thin,thout',iring,thin,thout



       read(4,*) nstep
       read(4,*) ntheta
       read(4,*) nphai
       read(4,*) vmax_wo
       nmodel=ntheta*nphai



c       scadisk=0.2
c       scadiskz=0.04
c       dmass=1.0
c       rcutdz=0.05

       scadisk=1./scadisk
       iover=0

c========================================================
c
c  Initialization of galactic disk 
c
c=======================================================
       in=0
       nn1=0
       rhmin=1000.
       rhmax=-1000.
       rhminz=1000.
       rhmaxz=-1000.

 250    continue
       r=ran1(ii)*rcutd
       rz0=ran1(ii)/scadiskz*rcutdz
       sigd=2.*pi*r*scadisk**2.*dmass/2./pi*exp(-scadisk*r)
c       sigd=2.*pi*r*sima00*exp(-scadisk*r)
       sigdz=4./(exp(-rz0)+exp(rz0))**2.
       rh=sigd
       rhz=sigdz
        rhmin=min(rh,rhmin)
        rhmax=max(rh,rhmax)
        rhminz=min(rhz,rhminz)
        rhmaxz=max(rhz,rhmaxz)
        nn1=nn1+1
        if(nn1.gt.30000) goto 280

       goto 250 
       

 280    continue

         write(6,*) 'rh0,rhmin,rhmax,sigma0',rh0,rhmin,rhmax,sigma0
         write(6,*) 'rhminz,rhmaxz',rhminz,rhmaxz
 300    continue
       r=ran1(ii)*rcutd
       sigd=2.*pi*r*scadisk**2.*dmass/2./pi*exp(-scadisk*r)
       rx=sigd
       r1=ran1(ii)
       
       r1=(rhmax-rhmin)*r1+rhmin
       if(r1.gt.rx) go  to 300

 310    continue


       rz0=ran1(ii)/scadiskz*rcutdz
       sigdz=4./(exp(-rz0)+exp(rz0))**2.
       rxz=sigdz
       r1=ran1(ii)
       r1z=(rhmaxz-rhminz)*r1+rhminz
       if(r1z.gt.rxz) go to 310


        in=in+1

        rposneg=ran1(ii)
        if(rposneg.gt.0.5) then
         posneg=1.0
        else
         posneg=-1.0
        end if

        zz0=rz0*scadiskz*posneg


c             z=ran1(ii) 
c             z=r*(1.-2.*z)  
             z=0.
             pz=z
             z=sqrt(r**2.-z**2.)
             ph1=ran1(ii)
             ph=2.*pi*ph1 
             px=cos(ph)
             py=sin(ph)
        x1(in,1)=z*px
        x1(in,2)=z*py
        x1(in,3)=zz0
        v1(in,1)=0.0
        v1(in,2)=0.0
        v1(in,3)=0.0

        zz0=x1(in,3)
        if(abs(zz0).ge.rcutdz) then
         iover=iover+1
        end if

        if(in.lt.numd) goto 300


        write(6,*) 'rcutdz,iover',rcutdz,iover
c------For softening length estimation

       rcut00=rcutd

       nbin=100
       rwid=rcut00/real(nbin)
       do 635 j=1,nbin
       rbin(j)=rwid*real(j)
       rnt(j)=0.0
 635    continue 

       do 640 i=1,numd
       r00=sqrt(x1(i,1)**2.+x1(i,2)**2.+x1(i,3)**2.)
       do 650 j=1,nbin
       if(r00.le.rbin(j)) then
       rnt(j)=rnt(j)+1.0
       end if
 650    continue
 640    continue

       rhalf=0.0
       rn0=rnt(nbin)/2.0
       itest=0

       do 660 j=1,nbin
       if(rnt(j).gt.rn0.and.itest.eq.0) then
       itest=1
       rhalf=rbin(j)
       goto 670
       end if
 660    continue
 670    continue

     
       thalf=rn0
       fac00=4.0*pi/3.0
       fac01=fac00**(1./3.0)
       fac02=thalf**(1./3.0)
       eps00=rhalf*fac01/fac02

       write(6,*) 'DISK softening length'
       write(6,*) 'thalf,rhalf',thalf,rhalf
       write(6,*) 'SOFTENING LENGTH=',eps00

c===========OUTPUT OF INITIAL DATA====================


c-----For ring selection


      nring=0
      do 680 i=1,numd
      id(i)=1

      if(iring.ge.2) then

      r0=0.0
      rxy=0.0
      id(i)=0

      do 685 k=1,3
      r0=r0+x1(i,k)**2
      if(k.le.2) then
      rxy=rxy+x1(i,k)**2
      end if
 685  continue

      r0=sqrt(r0)
      if(x1(i,2).gt.0.0) then
      costh=360.*x1(i,1)/rxy/2./pi
      else
      costh=360.-360.*x1(i,1)/rxy/2./pi
      end if

      if(r0.ge.rcin.and.r0.le.rcout) then

      if(iring.eq.2) then

c-----For ring
      nring=nring+1
      id(i)=1

      else

c-----For shell
      if(costh.ge.thin.and.costh.le.thout) then
      nring=nring+1
      id(i)=1
      end if

      end if


      end if

      end if

 680  continue

           
       tiny=0.001
       iwas00=1
       sm00=dmass/real(numd)

c-----For ring or disk or shell
  
      if(iring.ge.2) then
      numd0=nring
      else
      numd0=numd
      end if

      nr1=0
       write(2,*) numd0,0.,sm00
       do 700 i=1,numd
       if(i.eq.1) then
       x1(1,1)=tiny
       x1(1,2)=tiny
       x1(1,3)=tiny
       end if
       if(id(i).eq.1) then
       write(2,*) (x1(i,j),j=1,3),(v1(i,j),j=1,3)
       nr1=nr1+1
       end if
 700   continue
       write(6,*) 'numd,numd0,nring,nr1',numd,numd0,nring,nr1


c------For label output

       do 750 i=1,nmodel
       write(3,*) iring
 750   continue

        end
    
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c  (C) Copr. 1986-92 Numerical Recipes Software 41m.
c============================================================
c
c      RAN1
c
c============================================================
      FUNCTION ran1(idum)
      INTEGER idum,IA,IM,IQ,IR,NTAB,NDIV
      real ran1,AM,EPS,RNMX
      PARAMETER (IA=16807,IM=2147483647,AM=1.e0/IM,IQ=127773,IR=2836,
     &NTAB=32,NDIV=1+(IM-1)/NTAB,EPS=1.2e-7,RNMX=1.e0-EPS)
      INTEGER j,k,iv(NTAB),iy
      SAVE iv,iy
      DATA iv /NTAB*0/, iy /0/
      if (idum.le.0.or.iy.eq.0) then
        idum=max(-idum,1)
        do 11 j=NTAB+8,1,-1
          k=idum/IQ
          idum=IA*(idum-k*IQ)-IR*k
          if (idum.lt.0) idum=idum+IM
          if (j.le.NTAB) iv(j)=idum
   11   continue
        iy=iv(1)
      endif
      k=idum/IQ
      idum=IA*(idum-k*IQ)-IR*k
      if (idum.lt.0) idum=idum+IM
      j=1+iy/NDIV
      iy=iv(j)
      iv(j)=idum
      ran1=min(AM*iy,RNMX)
c
      return
      end
