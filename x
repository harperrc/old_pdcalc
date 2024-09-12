for aLine in fi:
   aLine = aLine.strip()
   parts = aLine.split()
   i     = int(parts[0])
   j     = int(parts[1])
   k     = int(parts[2])

   dat[k] = float(parts[3])

   if (k == 10):
      fo.write('%s' % (ftrn))
      fo.write('data wp(%2d,%2d,ii),ii=1,10) /\n' % (i,j))
      fo.write('%s' % (ftrnc))
      n = 0
      for i in range(0,4):
         fo.write('%14.7e,' % (dat[n]))
         n = n + 1
      fo.write('\n')
      fo.write('%s' % (ftrnc))
      for i in range(0,4):
         fo.write('%14.7e,' % (dat[n]))
         n = n + 1
      fo.write('\n')
      fo.write('%s' % (ftrnc))
      fo.write('%14.7e,%14.7e/\n' % (dat[9],dat[10]))
      fo.write('\n')
     
fi.close()
fo.close()
