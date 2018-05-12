cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  Generation of initial conditions of a galaxy under MW-like potential
c
c  by Kenji Bekki on 12/1/2018 for Eleanor & Mark
c
c  Code name: gini.f 
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

      real    eps(10),egc(10),smass(10),smgc(10),
     &        pos(100000,3),vel(100000,3),
     &        pgc(1000000,3),vgc(1000000,3),
     &        unit(10),gp(10),
     &        xsat(3),vsat(3),vcf(3),trgc(100),rgc(100),
     &        tmcom(10)
         integer iwas(100000),igc(1000000),itest(10),nc(10),
     &            nsta(10),nwo(10),numtot(10),ncom(10)

c      open(1,file='gcmc.para')
      open(2,file='td2')
      open(3,file='unit.para')
      open(4,file='gpot.para')
      open(10,file='tinit.dat')

         pi=3.1415926535897


c-----Reading the disk/bulge/halo parameter
c
c  a,b,c,d,M_d,M_b,v_cir
c
c----
      read(3,*) (unit(k),k=1,3)
      ngp=8
      do 1 k=1,ngp
      read(4,*) gp(k) 
 1    continue
      gp(1)=gp(1)/unit(2)
      gp(2)=gp(2)/unit(2)
      gp(3)=gp(3)/unit(2)
      gp(4)=gp(4)/unit(2)
      gp(5)=gp(5)/unit(1)
      gp(6)=gp(6)/unit(1)
      gp(7)=gp(7)/unit(3)

      write(6,*) 'gp',(gp(k),k=1,ngp)

      iext=int(gp(8))
      write(6,*) 'iext',iext


c-------INPUT  disk MC file
           totm01=0.0
           totm02=0.0
           iscen=0
           read(2,*) nbdies1,tnow
          if(nbdies1.gt.100000) then
          write(6,*) 'nbdies1 > 100000',nbdies1
          stop
          end if
           read(2,*) nspe
          if(nspe.ne.5) then
          write(6,*) 'nspe ne 5 (correction necessary)',nspe
          nspe=5
          end if
           read(2,*) (eps(i),i=1,nspe)
           read(2,*) (smass(i),i=1,nspe)

          do 10 i=1,nbdies1
           read(2,*) pos(i,1),pos(i,2),pos(i,3),
     &                vel(i,1),vel(i,2),vel(i,3),iwas(i)


          totm01=totm01+smass(iwas(i)+1)

c------For circular velocity etc


      r0=sqrt(pos(i,1)**2.+pos(i,2)**2+pos(i,3)**2)
      r00=sqrt(pos(i,1)**2.+pos(i,2)**2)
      costh=pos(i,1)/r00
      sinth=pos(i,2)/r00

c---From halo
      p1=2.*r0*gp(7)**2.0
      p2=p1/(r0**2.+gp(4)**2.)
      fh=p2

c---From disk
      p1=r0*gp(5)
      p2=(gp(1)+gp(2))**2.
      p3=(r0**2.+p2)**1.5
      p4=p1/p3
      fd=p4

c---From bulge
      fb=gp(6)/(r0+gp(3))**2.

      vc0=sqrt(r0*(fh+fd+fb))

      vel(i,1)=-sinth*vc0
      vel(i,2)=costh*vc0
      vel(i,3)=0.0

 10        continue

          ncomt=nspe
          do 420 k=1,ncomt
          numtot(k)=0
          tmcom(k)=0.0
 420      continue

        
 500   continue

      numt=nbdies1
      tnow=0.0
           write(10,*) numt,tnow
c           write(10,*) nspe
c           write(10,*) (eps(i),i=1,5)
c           write(10,*) (smass(i),i=1,5)
c          tmtot2=0.0
          do 530 i=1,nbdies1
          write(10,*) (pos(i,k),k=1,3),(vel(i,k),k=1,3),iwas(i)
 530        continue
c          write(6,*) 'tmtot2',tmtot2
c          write(6,*) 'tmcom',(tmcom(k),k=1,5)


         end 

