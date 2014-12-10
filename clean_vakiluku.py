#! /usr/bin/python
# -*- coding: utf-8 -*-

FILE = open('vakiluku_posnro_2012.csv','r')
OUTFILE = open('vakiluku_posnro_2012_proper.csv','w')

print >> OUTFILE, FILE.readline()  
print >> OUTFILE, FILE.readline()
line = FILE.readline()
print >> OUTFILE, "Postinumero;alue;"+line.split(';')[-1]

for line in FILE:
    fields = line.rstrip().split(';')
    print >> OUTFILE, ';'.join(fields[0].split(' ',1))+';'+fields[1]
    

