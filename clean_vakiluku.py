#! /usr/bin/python
# -*- coding: utf-8 -*-

FILE = open('vakiluku_posnro_2012.csv','r')
OUTFILE = open('vakiluku_posnro_2012_proper.csv','w')

FILE.readline()  
FILE.readline()
line = FILE.readline()
#print >> OUTFILE, "Postinumero;alue;"+line.split(';')[-1]
print >> OUTFILE, "Postinumero;alue;vakiluku"

for line in FILE:
    fields = line.rstrip().split(';')
    print >> OUTFILE, ';'.join(fields[0].split(' ',1))+';'+fields[1]
    

