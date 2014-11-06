#Removes some data from the CIC LARA .csv files so they can be read using R into SQLITE
#Also allows reading some lines from the histcrs.csv file that have non-ASCII characters

import csv
fd = "c:/users/n/onedrive/documents/research/CIC project/UMD LARA Data/"
fn = "rawadmiss.csv"
outfn = "rawadmiss_short.csv"
inf = open(fd+fn, "r" )
outf = open(fd+outfn,'w')

i = 0
for line in inf:
    i = i +1
    line = line.replace("\00"," ")
    array = line.split(',')
    first_term = array[1]
    newid = array[3]
    SAT_HIGH_VERBAL = array[6]
    SAT_HIGH_MATH = array[7]
    HS_ACAD_GPA = array[9]
    HIGH_SCHOOL_CD = array[86]
    outf.write(newid + "," + first_term + "," +  SAT_HIGH_VERBAL +  "," +  SAT_HIGH_MATH +  "," +  HS_ACAD_GPA + "\n")

print (i)
inf.close()


import csv
fd = "c:/users/n/onedrive/documents/research/CIC project/UMD LARA Data/"
fn = "histcrs.csv"
outfn = "histcrs2.csv"
inf = open(fd+fn, "r" )
outf = open(fd+outfn,'w')

for line in inf:
    line = line.replace("\00"," ")
    array = line.split(',')
    newid = (array[3])
    TERM = (array[4])
    COURSE = array[6]
    CRS_GRADE = array[19]
    CRS_CREDIT = array[12]
    outf.write(newid + "," + TERM + "," +  COURSE +  "," +  CRS_GRADE +  "," +  CRS_CREDIT + "\n")

inf.close()

