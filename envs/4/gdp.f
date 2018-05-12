cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
c
c  Generation of initial conditions of a galaxy under MW-like potential
c
c  by Kenji Bekki on 12/1/2018 for Eleanor & Mark
c
c  Code name: gdp.f  for creating a new dg.para for gas density
c
c
cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc


      real    unit(10),rpara(10) 

      open(1,file='ram.para')
      open(2,file='unit.para')
      open(3,file='dg2.para')


      read(1,*) rpara(1),rpara(2)
      read(1,*) rpara(3)
      read(1,*) rpara(4)
      read(1,*) rpara(5)
      read(1,*) rpara(6)
      read(1,*) rpara(7)
      read(1,*) rpara(8)

      read(2,*) (unit(k),k=1,3)

      tmg=rpara(5)/unit(1)
      rg=rpara(6)/unit(2)
      rgs=rpara(7)/unit(2)
      num=int(rpara(8))

      write(3,*) tmg
      write(3,*) rg,rg/5.0
      write(3,*) rgs,rgs/10.0
      write(3,*) num
      write(3,*) 1

      end 

