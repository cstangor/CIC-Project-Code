#Set your working directory.  All data is read and written here:
setwd("c:/users/n/onedrive/documents/research/CIC project/UMD LARA Data/")

#Put this source code in the working directory.
source ("startup.R")

workdir <- "d:/umd lara data/"
sr <- read.delim(paste(workdir,"md_sr_nb_2.csv",sep=""), sep = ",")
sc   <- read.delim(paste(workdir,"md_sc_short_nb_2.csv",sep=""), sep = ",")
sr_sc <- merge(sc,sr,by='ID',all.x=TRUE)

wom = subset(sr_sc, GENDER == "F")
men = subset(sr_sc, GENDER == "M")
transfers = subset(sr_sc, TRANSFER == "Y")
freshmen = subset(sr_sc, TRANSFER == "N")

whites = subset(sr_sc, ETHNICITY == 1)
blacks = subset(sr_sc, ETHNICITY == 2)
asians = subset(sr_sc, ETHNICITY == 3)
hispanics = subset(sr_sc, ETHNICITY == 6)

ntot = aggregate(sr_sc$GPAO, list(sr_sc$COURSE_CODE), count)
nfemale = aggregate(wom$GPAO, list(wom$COURSE_CODE), count)
nmale = aggregate(men$GPAO, list(men$COURSE_CODE), count)

ntransfer = aggregate(transfers$GPAO, list(transfers$COURSE_CODE), count)
nfreshmenenter = aggregate(freshmen$GPAO, list(freshmen$COURSE_CODE), count)

nwhite = aggregate(whites$GPAO, list(whites$COURSE_CODE), count)
nasian = aggregate(asians$GPAO, list(asians$COURSE_CODE), count)
nblack = aggregate(blacks$GPAO, list(blacks$COURSE_CODE), count)
nhispanic = aggregate(hispanics$GPAO, list(hispanics$COURSE_CODE), count)

gpa = aggregate(sr_sc$GPAO, list(sr_sc$COURSE_CODE), mean)
grade  = aggregate(sr_sc$GRADE, list(sr_sc$COURSE_CODE), mean)

gpafemale = aggregate(wom$GPAO, list(wom$COURSE_CODE), mean)
gpamale = aggregate(men$GPAO, list(men$COURSE_CODE), mean)

gradefemale = aggregate(wom$GRADE, list(wom$COURSE_CODE), mean)
grademale = aggregate(men$GRADE, list(men$COURSE_CODE), mean)

gpatransfer = aggregate(transfers$GPAO, list(transfers$COURSE_CODE), mean)
gradetransfer = aggregate(transfers$GRADE, list(transfers$COURSE_CODE), mean)

gpafreshmenenter = aggregate(freshmen$GPAO, list(freshmen$COURSE_CODE), mean)
gradefreshmenenter = aggregate(freshmen$GRADE, list(freshmen$COURSE_CODE), mean)

gpawhite = aggregate(whites$GPAO, list(whites$COURSE_CODE), mean)
gpaasian = aggregate(asians$GPAO, list(asians$COURSE_CODE), mean)
gpablack = aggregate(blacks$GPAO, list(blacks$COURSE_CODE), mean)
gpahispanic = aggregate(hispanics$GPAO, list(hispanics$COURSE_CODE), mean)

gradewhite = aggregate(whites$GRADE, list(whites$COURSE_CODE), mean)
gradeasian = aggregate(asians$GRADE, list(asians$COURSE_CODE), mean)
gradeblack = aggregate(blacks$GRADE, list(blacks$COURSE_CODE), mean)
gradehispanic = aggregate(hispanics$GRADE, list(hispanics$COURSE_CODE), mean)

#nyoung
#nold
#ntwoormore
#gpayoung
#gradeyoung
#gpaold
#gradeold
#gpatwoormore
#gradetwoormore  

gpt = merge(ntot, nfemale, by = "Group.1")
gpt = cbind(gpt, nmale)
gpt = cbind(gpt, ntransfer)
gpt = cbind(gpt, nfreshmenenter)
gpt = cbind(gpt, nwhite)
gpt = cbind(gpt, nasian)
gpt = cbind(gpt, nblack)
gpt = cbind(gpt, nhispanic)
gpt = cbind(gpt, gpa)
gpt = cbind(gpt, gpafemale)
gpt = cbind(gpt, gpamale)
gpt = cbind(gpt, gpatransfer)
gpt = cbind(gpt, gpafreshmenenter)
gpt = cbind(gpt, gpawhite)
gpt = cbind(gpt, gpaasian)
gpt = cbind(gpt, gpablack)
gpt = cbind(gpt, gpahispanic)
gpt = cbind(gpt, gradefemale)
gpt = cbind(gpt, grademale)
gpt = cbind(gpt, gradetransfer)
gpt = cbind(gpt, gradefreshmenenter)
gpt = cbind(gpt, gradewhite)
gpt = cbind(gpt, gradeasian)
gpt = cbind(gpt, gradeblack)
gpt = cbind(gpt, gradehispanic)

  
names (gpt) = c("course", "ntot",
                "nfemale", 
                "V1", "nmale",
                "V2","ntransfer",
                "V3","nfreshmen",
                "V4","nwhite",
                "V5","nasian",
                "V6","nblack",
                "V7","nhispanic",
                "V7a","gpa",
                "V8","gpafemale",
                "V9","gpamale",
                "V10","gpatransfer",
                "V11","gpafreshmen",
                "V12","gpawhite",
                "V13","gpaasian",
                "V14","gpablack",
                "V15","gpahispanic",
                "V16","gradefemale",
                "V17","grademale",
                "V18","gradetransfer",
                "V19","gradefreshmen",
              "V20","gradewhite",
              "V21","gradeasian",
              "V22","gradeblack",
              "V23","gradehispanic")
    
gpt$V1 = NULL
gpt$V2 = NULL
gpt$V3 = NULL
gpt$V4 = NULL
gpt$V5 = NULL
gpt$V6 = NULL
gpt$V7 = NULL
gpt$V7a = NULL
gpt$V8 = NULL
gpt$V9 = NULL
gpt$V10 = NULL
gpt$V11 = NULL
gpt$V12 = NULL
gpt$V13 = NULL
gpt$V14 = NULL
gpt$V15 = NULL
gpt$V16 = NULL
gpt$V17 = NULL
gpt$V18 = NULL
gpt$V19 = NULL
gpt$V20 = NULL
gpt$V21 = NULL
gpt$V22 = NULL
gpt$V23 = NULL

View(gpt)
write.csv(gpt,"gp_summary_table.csv")
