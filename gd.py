#!/usr/bin/python3

import math
import sys

ofile = 'gd.dat'
fo    = open(ofile,'w')

yld  = 100.0
gr   = 0.0
dgr  = 0.05
dhob = 0.05

yld13 = yld**0.3333333333333333
yldic = 1.0 / yld13

while (gr <= 10.0):

   hob = 0.0
   while (hob <= 10.0):
      hobft = hob *3280.8
      shob  = hobft * yldic
      sil   = shob / 100.0 + 1.0001

      if (sil >= 10.0):
         break

      fo.write('%15.5f %15.5f %15.5f\n' % (gr,hob,yld))

      hob = hob + dhob

   gr = gr + dgr

fo.close()
