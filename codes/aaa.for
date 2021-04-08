      character*2 az,zlab1,zlab2,zlab3
      character*5 nuc
      character*3 bz,ba
      character*40 asys1,asys2,bsys1,bsys2,bsys3
      character*40 esys1,esys2,esys3,esys      
      character*40 tsys1,tsys2,dsys1,dsys2
      character*35 csys1,csys2
      character*70 asys4
      character*70 app
      dimension xx(10)
      common/czlabel/zlab1(200),zlab2(200),zlab3(200)
      call getenv("main",app)
c      print *,app
1000  continue
cbab ire = 0 to only as once
cbab ire = 1 to ask many times
      ire = 0
cbab iform = 0 to ask for o16
cbab iform = 1 to ask for 16,8
cbab iform = 2 to ask for 8,16
      iform = 2
      open(unit=10,file='aaa.dai',err=1001,status='old')
      read(10,*,err=1001) iform
1001  continue

c      call nmtop
      call zlab
      open(unit=10,file='nu.den')
      
      if(iform.eq.0) go to 1002
      if(iform.eq.1) go to 1003
      if(iform.eq.2) go to 1013      
1003  continue
      write(6,1011) 
1011  format('ia iz (integer format)')
      call readx(5,xx)
      ia = xx(1)
      iz = xx(2)
      if(ia.eq.0) go to 2000
c      read(5,*,err=2000,end=2000) ia,iz
      go to 1004
      
1013  continue
      write(6,1015) 
1015  format('Z A (integer format)')
      call readx(5,xx)
      iz = xx(1)
      ia = xx(2)
      if(iz.eq.0) go to 2000
c      read(5,*,err=2000,end=2000) ia,iz
      go to 1004      
      
1002  continue
      write(6,101) 
101   format('nucleus i.e. o16')
      nuc = ' '
      read(5,1005,err=2000) nuc
1005  format(a5)
      print *,nuc
      if(nuc.eq.' ') go to 2000
      ik = 1
      if(nuc(2:2).eq.'0') go to 300
      if(nuc(2:2).eq.'1') go to 300
      if(nuc(2:2).eq.'2') go to 300
      if(nuc(2:2).eq.'3') go to 300    
      if(nuc(2:2).eq.'4') go to 300
      if(nuc(2:2).eq.'5') go to 300
      if(nuc(2:2).eq.'6') go to 300
      if(nuc(2:2).eq.'7') go to 300 
      if(nuc(2:2).eq.'8') go to 300
      if(nuc(2:2).eq.'9') go to 300
      ik = 2
300   continue           
      if(ik.eq.1) write(az,301) nuc(1:1)
301   format(a1,' ')
      if(ik.eq.2) write(az,302) nuc(1:2)
302   format(a2)
      open(unit=99,file = 'num.dat')
      if(ik.eq.1) write(99,303) (nuc(i:i),i=2,5)
303   format(4a1)
      if(ik.eq.2) write(99,304) (nuc(i:i),i=3,5)
304   format(3a1)
      rewind(99)
      read(99,*) ia 
      close(99)
      call system('nudel num.dat') 
      print *,nuc,az,ia
      call low(az(1:1))
      call low(az(2:2))
c      read(5,*) aa,ia
      do 200 i = 1,200
      if(az.eq.zlab1(i)) iz = i
      if(az.eq.zlab2(i)) iz = i
200   continue


1004  continue

c      print 105,ia,iz,zlab1(iz),zlab2(iz)
105   format(2i3,1x,a2,1x,a2)



      write(10,102) ia,iz
102   format('az',/,i3,',',i3)

c      if(ia.eq.11.and.iz.eq.3) write(10,104)
c104   format('cocc',/,'2,3,2.,-0.3',/,'0')
      write(10,103) 

c103   format('ne',/,'param',/,'gp',/,'nm')
103   format('ne',/,'param',/,'gp')
c      write(10,106)
c106   format('pl',/,'1,13,,,potp',/,'pl',/,'1,14,,,potn')
      write(10,107)
107   format('st')
      close(10)
      call system('dens < nu.den')     
      call system('nudel dens.dao')
      if(iz.le.9) write(bz,501) iz
501   format('00',i1)
      if(iz.gt.9.and.iz.le.99) write(bz,502) iz
502   format('0',i2)
      if(iz.gt.99) write(bz,503) iz
503   format(i3)
      if(ia.le.9) write(ba,511) ia
511   format('00',i1)
      if(ia.gt.9.and.ia.le.99) write(ba,512) ia
512   format('0',i2)
      if(ia.gt.99) write(ba,513) ia
513   format(i3)

      asys4  = 'nudel sp'//bz//'-'//ba//'.txt'
      call system(asys4)  
      
      asys1  = 'sp'//bz//'-'//ba//'.top'
      asys2  = 'de'//bz//'-'//ba//'.top'
      
c      tsys1  = ' '//zlab3(iz)//ba//'-sps.top'
c      tsys2  = ' '//zlab3(iz)//ba//'-den.top'
      
      tsys1  = ' '//bz//'-'//ba//'-sps.top'
      tsys2  = ' '//bz//'-'//ba//'-den.top'      
        
      asys4  = 'nuren '//asys1//tsys1
      print *,asys4
      call system(asys4)
      asys4  = 'nuren '//asys2//tsys2
      call system(asys4)        
            
     
      asys1  = 'sp'//bz//'-'//ba//'.eps'
      asys2  = 'de'//bz//'-'//ba//'.eps'

c      bsys1  = ' '//zlab3(iz)//ba//'-sps.eps'
c      bsys2  = ' '//zlab3(iz)//ba//'-den.eps'   
c      bsys3  = ' '//zlab3(iz)//ba//'-pot.eps' 
      
      bsys1  = ' '//bz//'-'//ba//'-sps.eps'
      bsys2  = ' '//bz//'-'//ba//'-den.eps'   
c      bsys3  = ' '//bz//'-'//ba//'-pot.eps' 
            
      
c      esys  = ' '//zlab3(iz)//ba    
      esys  = ' '//bz//'-'//ba               
      
c      esys1  = ' '//zlab3(iz)//ba//'-sps'
c      esys2  = ' '//zlab3(iz)//ba//'-den'   
c      esys3  = ' '//zlab3(iz)//ba//'-pot'   
      
      esys1  = ' '//bz//'-'//ba//'-sps'
      esys2  = ' '//bz//'-'//ba//'-den'   
c      esys3  = ' '//bz//'-'//ba//'-pot'                 
                         
     

      asys4  = 'nuren '//asys1//bsys1
      call system(asys4)
      asys4  = 'nuren '//asys2//bsys2
      call system(asys4)  
      


      print *,tsys1
      write(asys4,801) esys1
801   format('call cps ',a40,' > cps.dat')
      call system(asys4)
c      write(asys4,802) bsys1
c802   format('bab ',a40)
c      call system(asys4)
c      write(asys4,803) bsys1
c803   format('nudel ',a40)
c      call system(asys4)
      
      write(asys4,801) esys2
      call system(asys4)
c      write(asys4,802) bsys2
c      call system(asys4)
c      write(asys4,803) bsys2
c      call system(asys4)
      
       
c      call pot(nuc)
c      call system('call cps pot > cps.dat')

c      call system('bab pot')

c      write(asys4,804) bsys3
c804   format('nuren pot.eps ',a40)
c      call system(asys4)

c      write(asys4,902) esys
c902   format('call pdf-all ',a40)           
      write(asys4,902) esys
902   format('call jpg-all ',a40)
      call system(asys4)      
c      write(asys4,901) esys2 
c      call system(asys4)       
c      write(asys4,901) esys3 
c      call system(asys4)            
      
c      call system('nudel eos.dao')
c      call system('nudel nm.dao')
c      call system('nudel nmv.plt')
c      call system('nudel amt.plt')
c      call system('nudel nmm.plt')
c      call system('nudel nmt.plt')
      
c      call system('call cps nm > cps.dat')

c      call system('call pdf nm')  

c      call system('call jpg nm')         
      
      
c      call system('nudel *.plt')
c      call system('nudel *.top')
c      call system('nudel *.eps')
c      call system('nudel nu.den')
c      call system('nudel help-top.txt')
c      call system('nudel cps.dat')
       

      close(99)
      close(50)
      
c      call system('call iprenew')

      if(ire.eq.1) go to 1000
      
2000  continue
      
   
                       
      end
      
      
      subroutine zlab
      character*2 zlab1,zlab2,zlab3
      common/czlabel/zlab1(200),zlab2(200),zlab3(200)
      zlab1(1) = 'H'
      zlab1(2) = 'He'
      zlab1(3) = 'Li'
      zlab1(4) = 'Be'
      zlab1(5) = 'B'
      zlab1(6) = 'C'
      zlab1(7) = 'N'
      zlab1(8) = 'O'
      zlab1(9) = 'F'
      zlab1(10) = 'Ne'
      zlab1(11) = 'Na'
      zlab1(12) = 'Mg'
      zlab1(13) = 'Al'
      zlab1(14) = 'Si'
      zlab1(15) = 'P'
      zlab1(16) = 'S'
      zlab1(17) = 'Cl'
      zlab1(18) = 'Ar'
      zlab1(19) = 'K'
      zlab1(20) = 'Ca'
      zlab1(21) = 'Sc'
      zlab1(22) = 'Ti'
      zlab1(23) = 'V'
      zlab1(24) = 'Cr'
      zlab1(25) = 'Mn'
      zlab1(26) = 'Fe'
      zlab1(27) = 'Co'
      zlab1(28) = 'Ni'
      zlab1(29) = 'Cu'
      zlab1(30) = 'Zn'
      zlab1(31) = 'Ga'
      zlab1(32) = 'Ge'
      zlab1(33) = 'As'
      zlab1(34) = 'Se'
      zlab1(35) = 'Br'
      zlab1(36) = 'Kr'
      zlab1(37) = 'Rb'
      zlab1(38) = 'Sr'
      zlab1(39) = 'Y'
      zlab1(40) = 'Zr'
      zlab1(41) = 'Nb'
      zlab1(42) = 'Mo'
      zlab1(43) = 'Tc'
      zlab1(44) = 'Ru'
      zlab1(45) = 'Rh'
      zlab1(46) = 'Pd'
      zlab1(47) = 'Ag'
      zlab1(48) = 'Cd'
      zlab1(49) = 'In'
      zlab1(50) = 'Sn'
      zlab1(51) = 'Sb'
      zlab1(52) = 'Te'
      zlab1(53) = 'I'
      zlab1(54) = 'Xe'
      zlab1(55) = 'Cs'
      zlab1(56) = 'Ba'
      zlab1(57) = 'La'
      zlab1(58) = 'Ce'
      zlab1(59) = 'Pr'
      zlab1(60) = 'Nd'
      zlab1(61) = 'Pm'
      zlab1(62) = 'Sm'
      zlab1(63) = 'Eu'
      zlab1(64) = 'Gd'
      zlab1(65) = 'Tb'
      zlab1(66) = 'Dy'
      zlab1(67) = 'Ho'
      zlab1(68) = 'Er'
      zlab1(69) = 'Tm'
      zlab1(70) = 'Yb'
      zlab1(71) = 'Lu'
      zlab1(72) = 'Hf'
      zlab1(73) = 'Ta'
      zlab1(74) = 'W'
      zlab1(75) = 'Re'
      zlab1(76) = 'Os'
      zlab1(77) = 'Ir'
      zlab1(78) = 'Pt'
      zlab1(79) = 'Au'
      zlab1(80) = 'Hg'
      zlab1(81) = 'Tl'
      zlab1(82) = 'Pb'
      zlab1(83) = 'Bi'
      zlab1(84) = 'Po'
      zlab1(85) = 'At'
      zlab1(86) = 'Rn'
      zlab1(87) = 'Fr'
      zlab1(88) = 'Ra'
      zlab1(89) = 'Ac'
      zlab1(90) = 'Th'
      zlab1(91) = 'Pa'
      zlab1(92) = 'U'
      zlab1(93) = 'Np'
      zlab1(94) = 'Pu'
      zlab1(95) = 'Am'
      zlab1(96) = 'Cm'
      zlab1(97) = 'Bk'
      zlab1(98) = 'Cf'
      zlab1(99) = 'Es'
      zlab1(100) = 'Fm'
      zlab1(101) = 'Md'
      zlab1(102) = 'No'
      zlab1(103) = 'Lr'
      zlab1(104) = 'Rf'
      zlab1(105) = 'Db'
      zlab1(106) = 'Sg'
      zlab1(107) = 'Bh'
      zlab1(108) = 'Hs'
      zlab1(109) = 'Mt'  
      zlab1(110) = 'Ds'
      zlab1(111) = 'Rg'
      zlab1(112) = 'Cn'
      zlab1(113) = '13'
      zlab1(114) = '14'     
      zlab1(115) = '15'
      zlab1(116) = '16'  
      zlab1(117) = '17'
      zlab1(118) = '18'
      zlab1(119) = '19'
      zlab1(120) = '20'
      zlab1(121) = '21'    
      zlab1(122) = '22'
      zlab1(123) = '23'
      zlab1(124) = '24'
      zlab1(125) = '25'   
      zlab1(126) = '26'                       
      zlab2(1) = 'h'
      zlab2(2) = 'he'
      zlab2(3) = 'li'
      zlab2(4) = 'be'
      zlab2(5) = 'b'
      zlab2(6) = 'c'
      zlab2(7) = 'n'
      zlab2(8) = 'o'
      zlab2(9) = 'f'
      zlab2(10) = 'ne'
      zlab2(11) = 'na'
      zlab2(12) = 'mg'
      zlab2(13) = 'al'
      zlab2(14) = 'si'
      zlab2(15) = 'p'
      zlab2(16) = 's'
      zlab2(17) = 'cl'
      zlab2(18) = 'ar'
      zlab2(19) = 'k'
      zlab2(20) = 'ca'
      zlab2(21) = 'sc'
      zlab2(22) = 'ti'
      zlab2(23) = 'v'
      zlab2(24) = 'cr'
      zlab2(25) = 'mn'
      zlab2(26) = 'fe'
      zlab2(27) = 'co'
      zlab2(28) = 'ni'
      zlab2(29) = 'cu'
      zlab2(30) = 'zn'
      zlab2(31) = 'ga'
      zlab2(32) = 'ge'
      zlab2(33) = 'as'
      zlab2(34) = 'se'
      zlab2(35) = 'br'
      zlab2(36) = 'kr'
      zlab2(37) = 'rb'
      zlab2(38) = 'sr'
      zlab2(39) = 'y'
      zlab2(40) = 'zr'
      zlab2(41) = 'nb'
      zlab2(42) = 'mo'
      zlab2(43) = 'tc'
      zlab2(44) = 'ru'
      zlab2(45) = 'rh'
      zlab2(46) = 'pd'
      zlab2(47) = 'ag'
      zlab2(48) = 'cd'
      zlab2(49) = 'in'
      zlab2(50) = 'sn'
      zlab2(51) = 'sb'
      zlab2(52) = 'te'
      zlab2(53) = 'i'
      zlab2(54) = 'xe'
      zlab2(55) = 'cs'
      zlab2(56) = 'ba'
      zlab2(57) = 'la'
      zlab2(58) = 'ce'
      zlab2(59) = 'pr'
      zlab2(60) = 'nd'
      zlab2(61) = 'pm'
      zlab2(62) = 'sm'
      zlab2(63) = 'eu'
      zlab2(64) = 'gd'
      zlab2(65) = 'tb'
      zlab2(66) = 'dy'
      zlab2(67) = 'ho'
      zlab2(68) = 'er'
      zlab2(69) = 'tm'
      zlab2(70) = 'yb'
      zlab2(71) = 'lu'
      zlab2(72) = 'hf'
      zlab2(73) = 'ta'
      zlab2(74) = 'w'
      zlab2(75) = 're'
      zlab2(76) = 'os'
      zlab2(77) = 'ir'
      zlab2(78) = 'pt'
      zlab2(79) = 'au'
      zlab2(80) = 'hg'
      zlab2(81) = 'tl'
      zlab2(82) = 'pb'
      zlab2(83) = 'bi'
      zlab2(84) = 'po'
      zlab2(85) = 'at'
      zlab2(86) = 'rn'
      zlab2(87) = 'fr'
      zlab2(88) = 'ra'
      zlab2(89) = 'ac'
      zlab2(90) = 'th'
      zlab2(91) = 'pa'
      zlab2(92) = 'u'
      zlab2(93) = 'np'
      zlab2(94) = 'pu'
      zlab2(95) = 'am'
      zlab2(96) = 'cm'
      zlab2(97) = 'bk'
      zlab2(98) = 'cf'
      zlab2(99) = 'es'
      zlab2(100) = 'fm'
      zlab2(101) = 'md'
      zlab2(102) = 'no'
      zlab2(103) = 'lr'
      zlab2(104) = 'rf'
      zlab2(105) = 'db'
      zlab2(106) = 'sg'
      zlab2(107) = 'bh'
      zlab2(108) = 'hs'
      zlab2(109) = 'mt'  
      zlab2(110) = 'ds'
      zlab2(111) = 'rg'
      zlab2(112) = 'cn'
      zlab2(113) = '13'
      zlab2(114) = '14'     
      zlab2(115) = '15'
      zlab2(116) = '16'  
      zlab2(117) = '17'
      zlab2(118) = '18'
      zlab2(119) = '19'
      zlab2(120) = '20'
      zlab2(121) = '21'    
      zlab2(122) = '22'
      zlab2(123) = '23'
      zlab2(124) = '24'
      zlab2(125) = '25'   
      zlab2(126) = '26'                         

      zlab3(1) = 'h_'
      zlab3(2) = 'he'
      zlab3(3) = 'li'
      zlab3(4) = 'be'
      zlab3(5) = 'b_'
      zlab3(6) = 'c_'
      zlab3(7) = 'n_'
      zlab3(8) = 'o_'
      zlab3(9) = 'f_'
      zlab3(10) = 'ne'
      zlab3(11) = 'na'
      zlab3(12) = 'mg'
      zlab3(13) = 'al'
      zlab3(14) = 'si'
      zlab3(15) = 'p_'
      zlab3(16) = 's_'
      zlab3(17) = 'cl'
      zlab3(18) = 'ar'
      zlab3(19) = 'k_'
      zlab3(20) = 'ca'
      zlab3(21) = 'sc'
      zlab3(22) = 'ti'
      zlab3(23) = 'v_'
      zlab3(24) = 'cr'
      zlab3(25) = 'mn'
      zlab3(26) = 'fe'
      zlab3(27) = 'co'
      zlab3(28) = 'ni'
      zlab3(29) = 'cu'
      zlab3(30) = 'zn'
      zlab3(31) = 'ga'
      zlab3(32) = 'ge'
      zlab3(33) = 'as'
      zlab3(34) = 'se'
      zlab3(35) = 'br'
      zlab3(36) = 'kr'
      zlab3(37) = 'rb'
      zlab3(38) = 'sr'
      zlab3(39) = 'y_'
      zlab3(40) = 'zr'
      zlab3(41) = 'nb'
      zlab3(42) = 'mo'
      zlab3(43) = 'tc'
      zlab3(44) = 'ru'
      zlab3(45) = 'rh'
      zlab3(46) = 'pd'
      zlab3(47) = 'ag'
      zlab3(48) = 'cd'
      zlab3(49) = 'in'
      zlab3(50) = 'sn'
      zlab3(51) = 'sb'
      zlab3(52) = 'te'
      zlab3(53) = 'i_'
      zlab3(54) = 'xe'
      zlab3(55) = 'cs'
      zlab3(56) = 'ba'
      zlab3(57) = 'la'
      zlab3(58) = 'ce'
      zlab3(59) = 'pr'
      zlab3(60) = 'nd'
      zlab3(61) = 'pm'
      zlab3(62) = 'sm'
      zlab3(63) = 'eu'
      zlab3(64) = 'gd'
      zlab3(65) = 'tb'
      zlab3(66) = 'dy'
      zlab3(67) = 'ho'
      zlab3(68) = 'er'
      zlab3(69) = 'tm'
      zlab3(70) = 'yb'
      zlab3(71) = 'lu'
      zlab3(72) = 'hf'
      zlab3(73) = 'ta'
      zlab3(74) = 'w_'
      zlab3(75) = 're'
      zlab3(76) = 'os'
      zlab3(77) = 'ir'
      zlab3(78) = 'pt'
      zlab3(79) = 'au'
      zlab3(80) = 'hg'
      zlab3(81) = 'tl'
      zlab3(82) = 'pb'
      zlab3(83) = 'bi'
      zlab3(84) = 'po'
      zlab3(85) = 'at'
      zlab3(86) = 'rn'
      zlab3(87) = 'fr'
      zlab3(88) = 'ra'
      zlab3(89) = 'ac'
      zlab3(90) = 'th'
      zlab3(91) = 'pa'
      zlab3(92) = 'u_'
      zlab3(93) = 'np'
      zlab3(94) = 'pu'
      zlab3(95) = 'am'
      zlab3(96) = 'cm'
      zlab3(97) = 'bk'
      zlab3(98) = 'cf'
      zlab3(99) = 'es'
      zlab3(100) = 'fm'
      zlab3(101) = 'md'
      zlab3(102) = 'no'
      zlab3(103) = 'lr'
      zlab3(104) = 'rf'
      zlab3(105) = 'db'
      zlab3(106) = 'sg'
      zlab3(107) = 'bh'
      zlab3(108) = 'hs'
      zlab3(109) = 'mt'  
      zlab3(110) = 'ds'
      zlab3(111) = 'rg'
      zlab3(112) = 'cn'
      zlab3(113) = '13'
      zlab3(114) = '14'     
      zlab3(115) = '15'
      zlab3(116) = '16'  
      zlab3(117) = '17'
      zlab3(118) = '18'
      zlab3(119) = '19'
      zlab3(120) = '20'
      zlab3(121) = '21'    
      zlab3(122) = '22'
      zlab3(123) = '23'
      zlab3(124) = '24'
      zlab3(125) = '25'   
      zlab3(126) = '26'                     
   
      return
      end   
      
      subroutine nmtop
      open(unit=50,file='nm.top')
      write(50,101)      
101   format('ls 1.3',/,'ts 1.5')

      write(50,102) 
102   format('ax',/,'lrbt')
      write(50,103)
103   format('nr 0.,0.,0.,55.',/,'fi 2.,6.,2.,5.')
      write(50,104) 
104   format('0.,.6,.05,.1,^r (nucleons/fm$3)')
      write(50,105)
105   format('0.,60.,5.,10.,Neutron EOS (MeV)')
      write(50,106)
106   format('ds line',/,'df nm.plt',/,'ds dash')
      write(50,107)
107   format('di',/,'.1,0.',/,'.1,59.9')

      write(50,202) 
202   format('ax',/,'lrt')
      write(50,203)
203   format('nr 0.,0.,-20.,40.',/,'fi 2.,6.,5.,8.')
      write(50,204) 
204   format('0.,.6,.05,.1,^r (nucleons/fm$3)')
      write(50,205)
205   format('-20.,40.,5.,10.,Matter EOS (MeV)')
      write(50,206)
206   format('ds line',/,'df am.plt',/,'ds dash')
      write(50,207)
207   format('di',/,'.16,-20.',/,'.16,39.9')
  
      close(50)
      return
      end
               
      subroutine pot(nuc)
      character*5 nuc
      open(unit=60,file='pot.top')
      write(60,101)
101   format('ls 1.3',/,'fi',/,'0.,10.,1.,2.,r (fm)')
      write(60,102)
102   format('-70.,0.,5.,10.,V(r) (MeV)')
      write(60,103)
103   format('ds line',/,'co red',/,'df potp.plt')
      write(60,104) 
104   format('dl 1.,-14.,protons',/,'co blue',/,'df potn.plt')
      write(60,105)
105   format('dl 1.,-20.,neutrons')
      write(60,106) nuc
106   format('dl 1.,-8.,'a5)
      close(60)
      return
      end   
      
      subroutine low(a)
      character*1 a
      if(a.eq.'A') a = 'a'   
      if(a.eq.'B') a = 'b'  
      if(a.eq.'C') a = 'c'   
      if(a.eq.'D') a = 'd'      
      if(a.eq.'E') a = 'e'   
      if(a.eq.'F') a = 'f'  
      if(a.eq.'G') a = 'g'   
      if(a.eq.'H') a = 'h'         
      if(a.eq.'I') a = 'i'   
      if(a.eq.'J') a = 'j'  
      if(a.eq.'K') a = 'k'   
      if(a.eq.'L') a = 'l'      
      if(a.eq.'M') a = 'm'   
      if(a.eq.'N') a = 'n'  
      if(a.eq.'O') a = 'o'   
      if(a.eq.'P') a = 'p'        
      if(a.eq.'Q') a = 'q'   
      if(a.eq.'R') a = 'r'  
      if(a.eq.'S') a = 's'   
      if(a.eq.'T') a = 't'      
      if(a.eq.'U') a = 'u'   
      if(a.eq.'V') a = 'v'  
      if(a.eq.'W') a = 'w'   
      if(a.eq.'X') a = 'x'         
      if(a.eq.'Y') a = 'y'   
      if(a.eq.'Z') a = 'z'  
      return       
      end          
      
cbab extracts up to 10 values into x(10) from a general read format      
      subroutine readx(iin,x)
      character*100 a,aa
      dimension x(10)
      a = ' '
      read(iin,800,err=900,end=900) a
800   format(a100)
      is = 1
      k = 0
      do 400 i = 1,10
400   x(i) = 0.
      do 420 i = 1,100
      if(a(i:i).eq.'!') go to 421
420   continue
421   imax = i
      do 440 i = imax,100
440   a(i:i) = ' '
      do 430 i = 100,1,-1
      if(a(i:i).ne.' ') go to 431
430   continue
431   imax = i+2
      if(imax.gt.100) imax = 100



300   continue
      k = k + 1
      if(k.gt.10) print 600
600   format('more than 10 entries in readx')
      if(k.gt.10) return
      do 100 i = is,imax
      
cbab two commas in a row    
c      print 7771,i,k,is,(a(ibab:ibab),ibab=1,10)  
c7771  format(3i3,1x,10a1)
      if(a(i:i).eq.','.and.i.eq.1) go to 1101
      if(i-1.le.0) go to 1105
      if(a(i:i).eq.','.and.a(i-1:i-1).eq.',') go to 1101
      if(a(i:i).eq.','.and.a(i-1:i-1).eq.' ') go to 1101
1105  continue      
      if(a(i:i).ne.' ') go to 101
100   continue
101   i1 = i
      if(i1.ge.imax) return
      
      do 102 i = i1,imax   
      if(a(i:i).eq.' ') go to 103
      if(a(i:i).eq.',') go to 103
102   continue
103   i2 = i
      ie = i2-1
      if(i2.ge.imax) return
      write(aa,210)  (a(i:i),i=i1,ie)
210   format(50a1)
      read(aa,*) x(k)
211   format(f12.6)
c      print 200,i1,ie,x(k),(a(i:i),i=i1,ie)
200   format(2i3,1x,f12.6,1x,50a1)
      is = i2+1
      if(is.ge.imax) return
      go to 300
1101  continue
      x(k) = 0.
      is = i+1
      go to 300    
1102  continue
      x(k) = 0.
      is = i+2
      go to 300              
900   continue
      x(1) = 999.
c      print 901 
901   format('end of file in readx ')
      return
      end      
            
                           