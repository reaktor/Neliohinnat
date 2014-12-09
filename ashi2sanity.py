import csv, re
ifile = csv.reader(open("004_ashi_tau_109_fis.csv"), delimiter=";")
ofile = open("sane-ashi.txt", 'w')
print >> ofile, "year pnro price n"
for i, fields in enumerate(ifile):
    if len(fields)==4: year = int(fields[3])
    if len(fields)==7 and re.match('^[0-9]+$', fields[5]):
        wtf, wtf, wtf, year_maybe, pnro, price, n = fields
        print >> ofile, year, pnro, float(price), int(n)
        

    
