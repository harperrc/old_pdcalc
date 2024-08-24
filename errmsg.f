      subroutine errmsg(ierr,iv,jt,kf,yld,cep,hob1,r95,d,wr,pod,iflg)
      include "real8.h"
      include "const.h"
      include "files.h"
      if(iflg.eq.5.or.iflg.eq.6)d=zero
      if(iflg.ne.9.and.iflg.ne.10)wr=zero
      if(iflg.ne.5.and.iflg.ne.6)pod=zero
c
      goto (10,20,30,40,50,60,70,80,90,100,110,120),ierr
c
 10   write(ldbg,11)
 11   format(' you cannot achieve desired pod with this weapon')
      goto 999
 20   write(ldbg,21)
 21   format(' vn is too large to use for available data curves')
      goto 999
 30   write(ldbg,31)
 31   format(' shob > 900 ft')
      goto 999
 40   write(ldbg,41)
 41   format(' only options with eta tgts are iflg=1 or 2')
      goto 999
 50   write(ldbg,51)
 51   format(' t of vntk must be an x when iflg=7')
      goto 999
 60   write(ldbg,61)
 61   format(' k for this type of vntk must be < 10')
      goto 999
 70   write(ldbg,71)
 71   format(' k of personnel vntk must be 1-9 or a-q')
      goto 999
 80   write(ldbg,81)
 81   format(' k of special crater cases must be 1-9 or a-p')
      goto 999
 90   write(ldbg,91)
 91   format(' t of vntk is not a valid character')
      goto 999
 100  write(ldbg,101)
 101  format(' crater required by z or y..... contact burst needed')
      goto 999
 110  write(ldbg,111)
 111  format(' k of vntk must specify a fatality curve for iflg=7')
      goto 999
 120  write(ldbg,121)
 121  format(' shob > 1000 ft')
c
 999  shob=hob1/yld**third
      write(ldbg,1000)iv,jt,kf,yld,cep,hob1,r95,d,wr,pod,iflg,shob
 1000 format(' iv: ',i3,' jt: ',i2,' kf: ',i2,' yld: ',f10.3,/,
     *  ' cep: ',f10.3,' hob1: ',f10.3,' r95: ',f10.3,' d: ',f10.3,/,
     *  ' wr: ',f10.3,' pod: ',f10.6,' iflg: ',i8,' shob: ',f10.3,/)
      return
      end
