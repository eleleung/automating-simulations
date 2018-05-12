cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  For density map estimation for AI
c
c  by Kenji Bekki 2017/12/02 revised on 2018/1/30
c

c
ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
	character*20 string1,string2,string3
        dimension  xw(1100000,3),vw(1100000,3),
     &       x0(1100000,3),v0(1100000,3),iw0(1100000),
     &       tmp(1100000),iw(1100000),rbin(200),tbin(200),
     &        xcen(10,3),vcen(10,3),rsq(200),
     &        vf(10,5,200,200),xpos(10,5,200,200),
     &        vf2(10,5,200,200),
     &        ypos(10,5,200,200),itest(10,5,200,200),
     &        itm(10,5,200),nvf(10,200,200),
     &        ss(12),tml(10),
     &        smass(10),eps(10),xq(60000000,3),iwq(60000000),
     &        vq(60000000,3),x0w(3),v0w(3),
     &        elp(2,200),pang(2,200),nct(10)
          
        NDIM=3
        pi=3.14159265359

c        open(1,file=string1)
c        open(1,file='v.dat')
        open(1,file='dr.dat')
        open(2,file='2df.dat')
        open(3,file='2dfsf.para')
        open(4,file='sfpt.para')
        open(7,file='2dfn.dat')
        open(8,file='denr.para')
        open(11,file='2df1.dat')
        open(12,file='dr.para')





cccccc Reading the parameter file for ID etc
       read(8,*) icomp
       read(8,*) tmfacn
       read(8,*) tmfacb
c       read(8,*) inorm
c       read(8,*) ifn

c-----For reading the disk/ring parameter only "iring" is used
       read(12,*) dmass11
       read(12,*) rcutd11,rcutdz11
       read(12,*) scadisk11,scadiskz11
       read(12,*) numd11
       read(12,*) iring
       read(12,*) rcin11
       read(12,*) rcout11
       read(12,*) thin11
       read(12,*) thout11

c-----For classification
       ifn=iring
       write(6,*) 'iring,ifn',iring,ifn


c         write(6,*) 'Input favorite component'
c         write(6,*) 'Gas =2  Bulge =4  Old stars =1 New stars =3'
c         write(6,*) 'Bulge + old stars =5'
c         write(6,*) 'Gas + new stars =6'
c         write(6,*) 'all stars  =7'
c         write(6,*) 'all comp  =8'
c         read(5,*) icomp
c         write(6,*) 'Input favorite integer to classify (1,2...)'
c         read(5,*) ifn

         if(icomp.le.4) then
         icomp1=icomp
         icomp2=icomp
         else

         if(icomp.eq.5) then
         icomp1=1
         icomp2=4
         end if
         if(icomp.eq.6) then
         icomp1=2
         icomp2=3
         end if
         if(icomp.eq.7.or.icomp.eq.8) then
         icomp1=1
         icomp2=4
         end if

         end if

         

c         write(6,*) 'Input nstep'
c         read(5,*) nstep
c          read(1,*) sqfac
c          read(1,*) alpha_min,alpha_max
c          read(1,*) delta_min,delta_max
c          read(1,*) (smass(ik),ik=1,2)
          read(3,*) nrbin 
          read(3,*) rcut_fac
          read(3,*) nsm 
          read(3,*) ipro 
          read(3,*) alpha_max
          read(3,*) delta_max
          read(3,*) tm_unit 
          read(3,*) tr_unit 

          read(4,*) nstep
          read(4,*) ntheta
          read(4,*) nphai
          read(4,*) vmax_wo

      write(6,*) 'nstep',nstep

      vmax_wo=vmax_wo/121.0

          ntbin=nrbin
          alpha_min=-alpha_max
          delta_min=-delta_max
          rcut00=rcut_fac


c----for M/L in B-band
         tml(1)=2.489
         tml(2)=0.328


         rwid=(alpha_max-alpha_min)/real(nrbin)
         do 10 kk=1,nrbin+1
         rbin(kk)=alpha_min+rwid*real(kk-1)
 10       continue

         twid=(delta_max-delta_min)/real(nrbin)
         do 15 kk=1,ntbin+1
         tbin(kk)=delta_min+twid*real(kk-1)
 15       continue
         twid0=twid*1.0183*1000.0
         write(6,*) 'rwid,twid',rwid,twid
c         write(6,*) 'nrbin',(rbin(k),k=1,nrbin+1)
c         write(6,*) 'ntbin',(tbin(k),k=1,ntbin+1)
c-----spn_unit=10^-9 *  6.0*10^10  / mass-to-light-ratio (tml_ratio)
c-----For MW
c         tr_unit=17.5
c         tm_unit=6.e10
         tr_unit1=tr_unit
         tr_unit=tr_unit*1000.
c         tml_sim=1.0
c         den_sim=1.0
c*****IMPORTANT
c         nsm=50
         spn_fac=tm_unit
         rwid00=(rwid*tr_unit)**2.
         spn_final=spn_fac/rwid00/real(nsm)
         write(6,*) 'spn_fac,rwid00,spn_final'
         write(6,*) spn_fac,rwid00,spn_final


c*****Very important*****
      sqfac=1.0

      istep=0
      nstept=nstep*ntheta*nphai
c      write(2,*) nstept,nrbin*ntbin
c      write(2,*) rwid,twid 
c      write(2,*) sqfac
c      write(2,*) alpha_min,alpha_max
c      write(2,*) delta_min,delta_max

      do 20 ijk=1,nstep


          nk=1



      ncen=0
           itest0=0
c          do 81 k=1,3
c	   xcen(i,k)=0.0
c	   vcen(i,k)=0.0
c 81       continue


      tmgas=0.0
          read(nk,*) numt,tnow,tmp0
c          read(nk,*) nspe
c          read(nk,*) (eps(j),j=1,nspe)
c          read(nk,*) (smass(j),j=1,nspe)
c          write(6,*) 'tnow,num',tnow,num
        do 90 j=1,numt
            read(nk,*) (x0(j,k),k=1,3),(v0(j,k),k=1,3)
c-----For gas only (fixed) 
        tmp(j)=tmp0
        iw0(j)=2 
c     &       iw0(j),tmp(j)
 
      if(iw0(j).eq.3) then
      tmp(j)=tmp(j)*tmfacn
      end if
      if(iw0(j).eq.4) then
      tmp(j)=tmp(j)*tmfacb
      end if

c------for all star component
      if(icomp.eq.7) then
      if(iw0(j).eq.3) iw0(j)=1
      end if
      if(icomp.eq.8) then
      if(iw0(j).eq.3.or.iw0(j).eq.2) iw0(j)=1
      end if

      if(iw0(j).eq.icomp1.or.iw0(j).eq.icomp2) tmgas=tmgas+tmp(j)

c      r0=0.0
c      do 91 k=1,3
c      r0=r0+x0(j,k)**2.
c 91   continue
c      r0=sqrt(r0)
c      if(r0.le.alpha_max) then
c      ncen=ncen+1
c      do 92 k=1,3
c      xcen(i,k)=xcen(i,k)+x0(j,k)
c      vcen(i,k)=vcen(i,k)+v0(j,k)
c 92   continue
c      end if

 90       continue


      write(6,*) 'tmgas',tmgas

c***** VERY IMPORTANT *****
       i=1
c*****

      do 95 ith=1,ntheta


      theta1=real(ith-1)*90.0/real(ntheta)
      theta1=2.*pi*theta1/360.

      do 96 iph=1,nphai

         do 97 j=1,2
         do 98 ij=1,nrbin
         if(ij.ne.1) then
         rsq(ij)=pi*(rbin(ij+1)**2.-rbin(ij)**2.)/real(ntbin)
         else
         rsq(ij)=pi*rbin(2)**2./real(ntbin)
         end if
         do 99 ik=1,ntbin
         vf(i,j,ij,ik)=0.0
         vf2(i,j,ij,ik)=0.0
         nvf(j,ij,ik)=0.0
 99      continue
 98      continue
 97      continue

c------For coordinate transformation
c-----For 2D map for given theta and phai direction
      istep=istep+1
c      write(2,*) istep

      write(6,*) 'ijk,istep',ijk,istep

      phai1=real(iph-1)*180.0/real(nphai)


      phai1=2.*pi*phai1/360.

      write(6,*) 'theta1,phai1',theta1,phai1

           ik1=0
           ik2=0
           ident=0

          nct(1)=0
          nct(2)=0
          nct(3)=0
          nct(4)=0
          nct(5)=0
          j=0

          tmgas=0.0
          do 100 ij=1,numt

c          if(iw0(ij).ge.1.and.iw0(ij).le.4) then
          if(iw0(ij).eq.icomp1.or.iw0(ij).eq.icomp2) then

          tmgas=tmgas+tmp(ij)
c-----Cordinate transformation

           theta=theta1
           phai=phai1
           posi1=x0(ij,1)
           posi2=x0(ij,2)
           posi3=x0(ij,3)
           veli1=v0(ij,1)
           veli2=v0(ij,2)
           veli3=v0(ij,3)
           x0w(1)=cos(phai)*cos(theta)*posi1-sin(phai)*posi2
     &             -cos(phai)*sin(theta)*posi3
           x0w(2)=sin(phai)*cos(theta)*posi1+cos(phai)*posi2
     &             -sin(phai)*sin(theta)*posi3
           x0w(3)=sin(theta)*posi1+cos(theta)*posi3
           v0w(1)=cos(phai)*cos(theta)*veli1-sin(phai)*veli2
     &             -cos(phai)*sin(theta)*veli3
           v0w(2)=sin(phai)*cos(theta)*veli1+cos(phai)*veli2
     &             -sin(phai)*sin(theta)*veli3
           v0w(3)=sin(theta)*veli1+cos(theta)*veli3



  
          j=j+1

          do 101 k=1,3
c          xw(j,k)=x0(ij,k)-xcen(i,k)
c          vw(j,k)=v0(ij,k)-vcen(i,k)
c          xw(j,k)=x0(ij,k)
c          vw(j,k)=v0(ij,k)
          xw(j,k)=x0w(k)
          vw(j,k)=v0w(k)
 101      continue

         iw(j)=iw0(ij)
c          if(iw0(ij).eq.3) then
c          iw(j)=1
c          end if
c          if(iw0(ij).eq.2) then
c          iw(j)=2
c          end if

          nct(iw(j))=nct(iw(j))+1

c-----For favorite projection

            x1=xw(j,1)
            x2=xw(j,2)
            x3=xw(j,3)
c            v1=vw(j,1)
c            v2=vw(j,2)
c            v3=vw(j,3)
             v1=tmp(ij)*spn_final
             v2=tmp(ij)*spn_final
             v3=tmp(ij)*spn_final

            if(ipro.eq.1) then
            xw(j,1)=x1
            xw(j,2)=x2
            xw(j,3)=x3
            vw(j,1)=v1
            vw(j,2)=v2
            vw(j,3)=v3
            end if
            if(ipro.eq.2) then
            xw(j,1)=x1
            xw(j,2)=x3
            xw(j,3)=x2
            vw(j,1)=v1
            vw(j,2)=v3
            vw(j,3)=v2
            end if
            if(ipro.eq.3) then
            xw(j,1)=x2
            xw(j,2)=x3
            xw(j,3)=x1
            vw(j,1)=v2
            vw(j,2)=v3
            vw(j,3)=v1
            end if

            end if

 100        continue

           write(6,*) 'tmgas after particle selection',tmgas 
c*****VERY IMPORTANT*****

         num =j

        
c******

c            write(6,*) 'j (tot nums),nct',j,(nct(k),k=1,2) 
c            write(6,*) 'i,xcen',i,(xcen(i,k),k=1,3)
            iout=0
            iin=0

c------For smoothing method

      tmgas=0.0
            inum=0
            ii=10

            do 1000 j=1,num

            do 1100 ij=1,nsm

            inum=inum+1

c        ii=j

c        do 1051 ik=1,3
          ki=0
          rk=0.
 1052    rk=rk+ran1(ii)
          ki=ki+1
         if(ki.ge.12) goto 1054
          goto 1052
 1054   rk=rk-6.

        r=rk*rcut00


c-----For 2D
c             z=0
c----For 3D
             z=ran1(ii) 
             z=r*(1.-2.*z)  


             pz=z
             z=sqrt(r**2.-z**2.)
             ph1=ran1(ii)
             ph=2.*pi*ph1 
             px=cos(ph)
             py=sin(ph)


       if(mod(inum,1000000).eq.0) then
         write(6,*) 'inum',inum
       end if
       if(inum.ge.60000000) then
        write(6,*) 'ERROR IN INUM'
       end if

        xq(inum,1)=z*px+xw(j,1)
        xq(inum,2)=z*py+xw(j,2)
        xq(inum,3)=pz+xw(j,3)
        vq(inum,1)=vw(j,1)
        vq(inum,2)=vw(j,2)
        vq(inum,3)=vw(j,3)

        iwq(inum)=iw(j)

        tmgas=tmgas+vq(inum,3)
 1100   continue
 1000   continue


       write(6,*) 'tmgas after nsm times',tmgas

           

            numwork=inum

            write(6,*) 'numwork',numwork

c-----******* IMPORTANT ****** For i=1
       i=1

c-----For estimation of smoothed velocity field


            do 110 j=1,numwork

            iwo1=0
            iwo2=0

c            x1=xw(j,1)-xcen(i,1)
c            x2=xw(j,2)-xcen(i,2)
c            x3=xw(j,3)-xcen(i,3)
c            iwas=iw(j)

c            x1=xq(j,1)-xcen(i,1)
c            x2=xq(j,2)-xcen(i,2)
c            x3=xq(j,3)-xcen(i,3)
            x1=xq(j,1)
            x2=xq(j,2)
            x3=xq(j,3)

            iwas=iwq(j)

c            if(iwas.eq.1.or.iwas.eq.3.or.iwas.eq.4) then
            if(iwas.eq.icomp1.or.iwas.eq.icomp2) then
            id=1
            goto 115
            end if

            write(6,*) 'Error iwas ne icomp1 nor icomp2'

c            if(iwas.eq.icom) then
            if(iwas.eq.icomp) then
            id=2
            goto 115
            end if

            if(iwas.lt.1.or.iwas.gt.4) then
            write(6,*) 'Error iwas ne 1 2 3 4'
            write(6,*) iwas
            stop
            end if

            goto 110

 115        continue

            if(id.ge.3) then
            write(6,*) 'ERROR IN ID'
            stop
            end if

c            r0=sqrt(x1**2.+x2**2.)
            r0=x1

            do 120 ij=1,nrbin
            if(r0.ge.rbin(ij).and.r0.lt.rbin(ij+1)) then
            iwo1=ij
            goto 125
            end if
 120        continue
 125        continue


            t0=x2

            do 130 ij=1,ntbin
            if(t0.ge.tbin(ij).and.t0.lt.tbin(ij+1)) then
            iwo2=ij
            goto 135
            end if
 130        continue
 135        continue


            if(iwo1.eq.0.or.iwo2.eq.0) then
c            write(6,*) 'ERROR IN IWO,j,r0,t0',j,r0,t0
c            stop

            iout=iout+1
            end if

c            if(vq(j,3).le.0.0) then
c            write(6,*) 'ERROR IN VQ(3)',vq(j,3)
c            stop
c            end if

            if(iwo1.ne.0.and.iwo2.ne.0) then
            nvf(id,iwo1,iwo2)=nvf(id,iwo1,iwo2)+1
            vf(i,id,iwo1,iwo2)=vf(i,id,iwo1,iwo2)+vq(j,3)
            vf2(i,id,iwo1,iwo2)=vf2(i,id,iwo1,iwo2)+vq(j,3)**2.
            iin=iin+1
            end if

 110        continue


c            write(6,*) 'iout,iin',iout,iin
        
            vmin=1.e10
            vmax=-vmin
            vmin1=1.e10
            vmax1=-vmin1

c            write(2,*) nrbin*ntbin,rcut 
c            write(10,*) nrbin*ntbin,rcut 
            do 138 ij=1,nrbin
            do 139 ik=1,ntbin

            if(nvf(1,ij,ik).ne.0) then
c            vf(i,1,ij,ik)=vf(i,1,ij,ik)/real(nvf(1,ij,ik))
c            vf2(i,1,ij,ik)=vf2(i,1,ij,ik)/real(nvf(1,ij,ik))
c            vf2(i,1,ij,ik)=sqrt(vf2(i,1,ij,ik)-vf(i,1,ij,ik)**2)
            vf(i,1,ij,ik)=vf(i,1,ij,ik)
            vf2(i,1,ij,ik)=vf2(i,1,ij,ik)
            vf(i,1,ij,ik)=log10(vf(i,1,ij,ik))

c-----For vmax limitation 
c      if(vf(i,1,ij,ik).gt.vmax_wo) then
c      vf(i,1,ij,ik)=vmax_wo
c      end if
c      if(vf(i,1,ij,ik).lt.-vmax_wo) then
c      vf(i,1,ij,ik)=-vmax_wo
c      end if

            vmax=max(vmax,vf(i,1,ij,ik))
            vmin=min(vmin,vf(i,1,ij,ik))
            end if

            if(nvf(2,ij,ik).ne.0) then
            vf(i,2,ij,ik)=vf(i,2,ij,ik)
            vf2(i,2,ij,ik)=vf2(i,2,ij,ik)
c            vf(i,2,ij,ik)=vf(i,2,ij,ik)/real(nvf(2,ij,ik))
c            vf2(i,2,ij,ik)=vf2(i,2,ij,ik)/real(nvf(2,ij,ik))
c            vf2(i,2,ij,ik)=sqrt(vf2(i,2,ij,ik)-vf(i,2,ij,ik)**2)
            vf(i,2,ij,ik)=log10(vf(i,2,ij,ik))
            vmax1=max(vmax1,vf(i,2,ij,ik))
            vmin1=min(vmin1,vf(i,2,ij,ik))
            end if

c            write(2,*) ij,ik,vf(i,1,ij,ik),vf(i,2,ij,ik)
c            write(10,*) ij,ik,vf2(i,1,ij,ik),vf2(i,2,ij,ik)
c            if(vf(i,2,ij,ik).gt.0.0) then
c            vf(i,2,ij,ik)=log10(vf(i,2,ij,ik))
c            vmax=max(vmax,vf(i,2,ij,ik))
c            vmin=min(vmin,vf(i,2,ij,ik))
c            else
c            vf(i,2,ij,ik)=0.0
c            end if

 139        continue
 138        continue


            write(6,*) 'vmin,vmax',vmin,vmax 
c            write(6,*) 'vmin1,vmax1',vmin1,vmax1 
c            write(2,*) nrbin*ntbin,rcut,vmin,vmax 

c-----For small contrast


           rwid=rwid*tr_unit1
           twid=twid*tr_unit1
           alpha_min=alpha_min*tr_unit1
           alpha_max=alpha_max*tr_unit1
           delta_min=delta_min*tr_unit1
           delta_max=delta_max*tr_unit1

ccccc Original output before 2017/3/22
c            write(2,*) nrbin*ntbin,vmin,vmax,vmin1,vmax1
c            write(2,*) rwid,twid 
c            write(2,*) sqfac
c            write(2,*) alpha_min,alpha_max
c            write(2,*) delta_min,delta_max
c            do 150 ij=1,nrbin
c            do 160 ik=1,ntbin
c            if(vf(i,2,ij,ik).gt.vmax) then
c            vf(i,2,ij,ik)=vmax
c            end if
c            write(2,*) ij,ik,vf(i,1,ij,ik),vf(i,2,ij,ik)
c 160        continue
c 150        continue

ccccc Original output before 2017/3/22
c            write(2,*) vmin,vmax
c            write(2,*) vmin1,vmax1
c            write(2,*) rwid,twid 
c            write(2,*) sqfac
c            write(2,*) alpha_min,alpha_max
c            write(2,*) delta_min,delta_max
            do 170 ij=1,nrbin
            do 175 ik=1,ntbin

            if(vf(i,1,ij,ik).gt.vmax) then
            vf(i,1,ij,ik)=vmax
            end if
            if(nvf(1,ij,ik).eq.0) then
            vf(i,1,ij,ik)=vmin
            end if

c            write(2,*) ij,ik,rbin(ij),tbin(ik),vf(i,1,ij,ik)
 175        continue
 170        continue

 80         continue


c-----For normalizatied value should be somewhere between 0 to 1
            vnmin=1.e10
            vnmax=-vnmin
            do 180 ij=1,nrbin
            do 185 ik=1,ntbin
            write(11,*) vf(i,1,ij,ik)
            vfnorm=(vf(i,1,ij,ik)-vmin)/(vmax-vmin)
            vnmin=min(vnmin,vfnorm)
            vnmax=max(vnmax,vfnorm)
            write(2,*) vfnorm
 185        continue
 180        continue
            write(7,*) ifn
            write(6,*) 'vnmin,max',vnmin,vnmax



c----End of phai  loop
 96         continue


c----End of theta loop
 95         continue

 20         continue

c-------End of V_Field estimation
      write(6,*) 'istep',istep

           end 


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

