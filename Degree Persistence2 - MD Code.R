# COMPUTES THE FLOWCHARTS FOR INITIAL DECLARED MAJOR (STEM/NONSTEM) AND
# DEGREE ATTAINMENT (STEM/NONSTEM)

#Set your working directory.  All data is read and written here:
#setwd("c:/users/n/onedrive/documents/research/CIC project/UMD LARA Data/R3")
setwd("X:/BAR/Projects/CIC/Learning Analytics/R3_UMD_RCode")

#Put the following 3 files in the above directory
#also be sure the file "STEM_CIP.csv" is there

source ("startup.R")
source ("CIC_startup.R")
source("degree_persistence_flowchart.R", local=FALSE)

#Send the printout to a file if you want
pdf('degree_persistence.pdf')

#Put your student record data file in the directory as well
#Read the data from the appropriate file (.csv example here)
#sr = read.csv("d:/indiana_sr.csv")
sr = read.csv("X:/BAR/Projects/CIC/Learning Analytics/R3_UMD_RCode/stu_rec_tbl.csv", header = T)


#Make the vnames uppper_case
names(sr)[names(sr) == 'major1_stem'] <- 'MAJOR1_STEM'
sr$MAJOR1_STEM = as.character(sr$MAJOR1_STEM)

names(sr)[names(sr) == 'first_declare_stem'] <- 'FIRST_DECLARE_STEM'
sr$FIRST_DECLARE_STEM = as.character(sr$FIRST_DECLARE_STEM)

#Subset on a term
#Make this line choose only people prior to fall 2008
sr_ = subset(sr, FIRST_TERM < "200808")
table(sr_$MAJOR1_STEM)
table(sr_$FIRST_DECLARE_STEM)

sr_$MAJOR1_STEM[sr_$MAJOR1_STEM == 'Y'] <- TRUE
sr_$MAJOR1_STEM[sr_$MAJOR1_STEM == 'N'] = FALSE

sr_$FIRST_DECLARE_STEM[sr_$FIRST_DECLARE_STEM == 'Y'] <- TRUE
sr_$FIRST_DECLARE_STEM[sr_$FIRST_DECLARE_STEM == 'N'] = FALSE

sr_$FIRST_MAJOR = ifelse(sr_$MAJOR1_STEM == FALSE , "2NonStem", NA)
sr_$FIRST_MAJOR = ifelse(sr_$MAJOR1_STEM == TRUE , "1Stem", sr_$FIRST_MAJOR)
sr_$FIRST_MAJOR [is.na(sr_$FIRST_MAJOR)] <- "3No Degree"
#table(sr_$FIRST_MAJOR)

sr_$FIRST_DECLARE_STEM[sr_$FIRST_DECLARE_STEM == FALSE] <- "2NonStem"
sr_$FIRST_DECLARE_STEM[sr_$FIRST_DECLARE_STEM == TRUE] <- "1Stem"
#table(sr_$FIRST_DECLARE_STEM)

# Create flowchart ----

tt = CrossTable(sr_$FIRST_DECLARE_STEM,sr_$FIRST_MAJOR, prop.t = FALSE, prop.r = TRUE, prop.c = TRUE, prop.chisq = FALSE, missing.include = FALSE) #, format = c("SPSS"))

g = dp_flow_chart(tt, out_type = "", center_title = NULL, title = "Degree Persistence - Counts", print_center = "None")
g = dp_flow_chart(tt, out_type = "percents", center_title = NULL, title = "Degree Persistence - Proportions", print_center = "None")

dev.off()
