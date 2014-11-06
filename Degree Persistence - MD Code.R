# COMPUTES THE FLOWCHARTS FOR INITIAL DECLARED MAJOR (STEM/NONSTEM) AND
# DEGREE ATTAINMENT (STEM/NONSTEM)

#Set your working directory.  All data is read and written here:
setwd("c:/users/n/onedrive/documents/research/CIC project/UMD LARA Data/R3")

#Put the following 3 files in the above directory
#also be sure the file "STEM_CIP.csv" is there

source ("startup.R")
source ("CIC_startup.R")
source("degree_persistence_flowchart.R", local=FALSE)

#Send the printout to a file if you want
pdf('degree_persistence.pdf',width=7,height=11)

#Put your student record data file in the directory as well
#Read the data from the appropriate file (.csv example here)
#sr = read.csv("d:/umd lara data/MD_SR.csv")
#sr = read.csv("d:/umd lara data/student.record.sample.tab", sep = "\t")

#Subset on a term
#Make this line choose only people prior to fall 2008
sr_ = subset(sr, FIRST_TERM < "200808")

sr_ = GetSTEM(sr_)

sr_$FIRST_MAJOR = ifelse(sr_$MAJOR1_STEM == FALSE | sr_$MAJOR2_STEM == FALSE, "2NonStem", NA)
sr_$FIRST_MAJOR = ifelse(sr_$MAJOR1_STEM == TRUE | sr_$MAJOR2_STEM == TRUE, "1Stem", sr_$FIRST_MAJOR)
sr_$FIRST_MAJOR [is.na(sr_$FIRST_MAJOR)] <- "3No Degree"

sr_$FIRST_DECLARE_STEM[sr_$FIRST_DECLARE_STEM == FALSE] <- "2NonStem"
sr_$FIRST_DECLARE_STEM[sr_$FIRST_DECLARE_STEM == TRUE] <- "1Stem"

# Create flowchart ----

tt = CrossTable(sr_$FIRST_DECLARE_STEM,sr_$FIRST_MAJOR, prop.t = FALSE, prop.r = TRUE, prop.c = TRUE, prop.chisq = FALSE, missing.include = FALSE) #, format = c("SPSS"))

# Print the flowchart ----
table(sc$TRANSFER_CREDITS_TOT, useNA = "always")

g = dp_flow_chart(tt, out_type = "", center_title = NULL, title = "Degree Persistence - Counts", print_center = "None")
g = dp_flow_chart(tt, out_type = "percents", center_title = NULL, title = "Degree Persistence - Proportions", print_center = "None")

dev.off()
