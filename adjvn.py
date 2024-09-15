#!/usr/bin/python3

import armsControlWonkLib as acw
import matplotlib.pyplot as plt
import math
import numpy as np
import os
import sys
from nuclib import *


vn  = 5
kf  = 1
yld = 1

yldcu = yld**0.3333333333
yldic = 1.0 / yldcu

vna = [1,2,3,4,5,6,10,20,30]
kfa = [0,1,2,3,4,5,6,7,8,9]

fo = open('out','w')

for vn in vna:
   for kf in kfa:
      fk10 = kf * 0.10
      r2    = 2.0
      abdif = 100.0
      while (abdif > 0.0):
         r1    = 1.0 - fk10 * (1.0 - 2.71441760 * yldic * (r2**0.50))
         abdif = abs(r1 - r2)
         r2    = r1

      avn = vn + 5.484 * math.log(r2)
      fo.write('%10.1f %2d %15.5f %15.5f\n' % (vn,kf,avn,adjustPVN(yld,vn,kf)))
   fo.write('\n')

fo.close()
