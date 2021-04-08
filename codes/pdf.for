      character*2 az,zlab1,zlab2,zlab3
      character*5 nuc
      character*3 bz,ba
      character*30 asys1,asys2,bsys1,bsys2,bsys3
      character*30 tsys1,tsys2,dsys1,dsys2
      character*35 csys1,csys2
      character*70 asys4
      common/czlabel/zlab1(200),zlab2(200),zlab3(200)
      
      call nmtop
      call zlab
      open(unit=10,file='nu.den')
      write(6,101) 
101   format('nucleus i.e. o16')
      nuc = ' '
      read(5,*) nuc
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
      
c      read(5,*) aa,ia
      do 200 i = 1,200
      if(az.eq.zlab1(i)) iz = i
      if(az.eq.zlab2(i)) iz = i
200   continue
      print 105,ia,iz,zlab1(iz),zlab2(iz)
105   format(2i3,1x,a2,1x,a2)

      write(10,102) ia,iz
102   format('az',/,i3,',',i3)

c      if(ia.eq.11.and.iz.eq.3) write(10,104)
c104   format('cocc',/,'2,3,2.,-0.3',/,'0')
      write(10,103) 

103   format('ne',/,'param',/,'gp',/,'nm')
      write(10,106)
106   format('pl',/,'1,13,,,potp',/,'pl',/,'1,14,,,potn')
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
      
      tsys1  = ' sps_'//zlab3(iz)//ba//'.top'
      tsys2  = ' den_'//zlab3(iz)//ba//'.top'
        
  
      asys4  = 'nuren '//asys1//tsys1
      call system(asys4)
      asys4  = 'nuren '//asys2//tsys2
      call system(asys4)        
            
     
      asys1  = 'sp'//bz//'-'//ba//'.eps'
      asys2  = 'de'//bz//'-'//ba//'.eps'

      bsys1  = ' sps_'//zlab3(iz)//ba
      bsys2  = ' den_'//zlab3(iz)//ba       
      bsys3  = ' pot_'//zlab3(iz)//ba//'.pdf'        
c      dsys1  = 'sps_'//zlab3(iz)//ba//'.bat'
c      dsys2  = 'den_'//zlab3(iz)//ba//'.bat'  
  
      asys4  = 'nuren '//asys1//bsys1//'.eps'
      call system(asys4)
      asys4  = 'nuren '//asys2//bsys2
      call system(asys4)  
      



      write(asys4,801) tsys1
801   format('call cps ',a30,' > cps.dat')
      call system(asys4)
      write(asys4,802) bsys1
802   format('bab ',a30)
      call system(asys4)
      write(asys4,803) bsys1
803   format('nudel ',a30)
      call system(asys4)
      
      write(asys4,801) tsys2
      call system(asys4)
      write(asys4,802) bsys2
      call system(asys4)
      write(asys4,803) bsys2
      call system(asys4)
      
      call pot(nuc)
      call system('call cps pot > cps.dat')
      call system('bab pot')
      write(asys4,804) bsys3
804   format('copy pot.pdf ',a30)
      call system(asys4)
      call system('nudel pot.pdf')      
      
      call system('nudel eos.dao')
      call system('nudel nm.dao')
      call system('nudel nmv.plt')
      call system('nudel amt.plt')
      call system('nudel nmm.plt')
      call system('nudel nmt.plt')
      call system('call cps nm > cps.dat')
      call system('bab nm')
      
      
      call system('nudel *.plt')
      call system('nudel *.top')
      call system('nudel *.eps')
      call system('nudel nu.den')
      call system('nudel help-top.txt')
      call system('nudel cps.dat')
       

      close(99)
      
   
                       
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
      zlab1(104) = 'X4'
      zlab1(105) = 'X5'
      zlab1(106) = 'X6'
      zlab1(107) = 'X7'
      zlab1(108) = 'X8'
      zlab1(109) = 'X9'  
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
      zlab2(104) = 'x4'
      zlab2(105) = 'x5'
      zlab2(106) = 'x6'
      zlab2(107) = 'x7'
      zlab2(108) = 'x8'
      zlab2(109) = 'x9'  
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
      zlab3(104) = 'x4'
      zlab3(105) = 'x5'
      zlab3(106) = 'x6'
      zlab3(107) = 'x7'
      zlab3(108) = 'x8'
      zlab3(109) = 'x9'        
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