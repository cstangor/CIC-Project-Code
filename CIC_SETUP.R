#THIS CODE IS USED BY UMD TO CREATE THE STUDENT_RECORD AND STUDENT_COURSE DATATABLES FOR THE CIC LARA PROJECT
#THE INPUT IS 6 .CSV FILES (PREPROCESSED IN PYTHON USING CIC_SETUP.py) 
#THE OUTPUT IS A SQLITE DATABASE CIC.DB (ON A D: DRIVE)

#Set your working directory.  All data is read and written here:
setwd("c:/users/n/onedrive/documents/research/CIC project/UMD LARA Data/")

#Put this source code in the working directory.
source ("startup.R")
source("CIC_shiny/CIC_Startup.R")

#The name of the directory containing the input data:
read_dir = "d:/UMD LARA Data/"

#This should show the 6 .csv files as listed in the Excel file filecontents.xlxs
list.files (read_dir)

#The name of the output SQlite database for the individual tables:
write_db = "d:/UMD LARA Data/CIC_TABLES.db"

#The name of the output SQlite database for the STUDENT_COURSE and STUDENT_RECORD tables:
#Can be the same as above
write_db_final = "d:/UMD LARA Data/CIC_TABLES.db"

#READ IN THE TABLES ----

#degrees.csv
#For degrees, we need to reshape the dataset so that potential multiple majors occur on one record
  fn = paste(read_dir, "degrees.csv", sep = "")
  test = read.csv(fn, nrows = -1) 

  names(test)[names(test) == 'newid'] <- 'ID'
  test$seq = sequence(rle(test$ID)$lengths)
  test = reshape(test, 
                   timevar = "seq",
                   idvar = "ID",
                   direction = "wide")

  #Just keep up to 2 majors
  test = test[,c("ID", "first_term.1", 
  "DEG_TERM.1", "DEGREE_CD.1", "DEGREE.1", "DEGREE_TYPE_CD.1","DEGREE_TYPE.1",
  "MAJOR_CD.1", "MAJOR.1", "CIP_CD.1", "CIP.1",
  "DEG_TERM.2", "DEGREE_CD.2", "DEGREE.2", "DEGREE_TYPE_CD.2", "DEGREE_TYPE.2",
  "MAJOR_CD.2", "MAJOR.2", "CIP_CD.2", "CIP.2")]

  names(test)[names(test) == 'DEG_TERM.1'] <- 'DEGREE_TERM_1'
  names(test)[names(test) == 'MAJOR.1'] <- 'DEGREE_MAJOR_1'
  names(test)[names(test) == 'CIP_CD.1'] <- 'DEGREE_CIP_CD_1'

  names(test)[names(test) == 'DEG_TERM.2'] <- 'DEGREE_TERM_2'
  names(test)[names(test) == 'MAJOR.2'] <- 'DEGREE_MAJOR_2'
  names(test)[names(test) == 'CIP_CD.2'] <- 'DEGREE_CIP_CD_2'


  #The list of valid STEM CIPs:
  stem_cip_ = read.csv("STEM_CIP.csv")
  stem_cip = c(as.character(stem_cip_$stem_cip))
  stem_cip_ = NULL
  
  #Redo the CIP code to include the period and then see if it is a stem major:
  #Major 1:
  CIP1 = with(test, str_sub(test$DEGREE_CIP_CD_1,0,2))
  CIP2 = with(test, str_sub(test$DEGREE_CIP_CD_1,3,7))
  test$DEGREE_CIP_CD_1 = with(test, paste(CIP1, ".", sep = ""))
  test$DEGREE_CIP_CD_1 = with(test, paste(test$DEGREE_CIP_CD_1, CIP2, sep = ""))
  test$DEGREE_CIP_CD_1 = ifelse(test$DEGREE_CIP_CD_1 == ".", NA, test$DEGREE_CIP_CD_1)
  test$DEGREE_STEM_CIP_1 = with(test, DEGREE_CIP_CD_1 %in% as.character(stem_cip))
  test$DEGREE_STEM_CIP_1 = ifelse(is.na(test$DEGREE_CIP_CD_1), NA, test$DEGREE_STEM_CIP_1)
  test$DEGREE_STEM_CIP_1 = ifelse(test$DEGREE_CIP_CD_1 == "NA.NA", NA, test$DEGREE_STEM_CIP_1)
  table(test$DEGREE_CIP_CD_1, useNA = "always")
  table(test$DEGREE_STEM_CIP_1, useNA = "always")

  #Major 2:
  CIP1 = with(test, str_sub(test$DEGREE_CIP_CD_2,0,2))
  CIP2 = with(test, str_sub(test$DEGREE_CIP_CD_2,3,7))
  test$DEGREE_CIP_CD_2 = with(test, paste(CIP1, ".", sep = ""))
  test$DEGREE_CIP_CD_2 = with(test, paste(test$DEGREE_CIP_CD_2, CIP2, sep = ""))
  test$DEGREE_CIP_CD_2 = ifelse(test$DEGREE_CIP_CD_2 == ".", NA, test$DEGREE_CIP_CD_2)
  test$DEGREE_STEM_CIP_2 = with(test, DEGREE_CIP_CD_2 %in% as.character(stem_cip))
  test$DEGREE_STEM_CIP_2 = ifelse(is.na(test$DEGREE_CIP_CD_2), NA, test$DEGREE_STEM_CIP_2)
  test$DEGREE_STEM_CIP_2 = ifelse(test$DEGREE_CIP_CD_2 == "NA.NA", NA, test$DEGREE_STEM_CIP_2)
  table(test$DEGREE_CIP_CD_2, useNA = "always")
  table(test$DEGREE_STEM_CIP_2, useNA = "always")

  
#Save data
  sqlitecon = dbConnect(SQLite(), write_db)
  dbWriteTable(sqlitecon, "Degrees", test, overwrite = TRUE)

#histcrs.csv
  #To avoid memory overrun, use Python script to create short file first
  fn = paste(read_dir, "histcrs_short.csv", sep = "")
  test = read.csv(fn, nrows = -1) 
  names(test)[names(test) == 'newid'] <- 'ID'

  #Keep only fall and spring semesters
  test = subset(test, str_sub(test$TERM,5,6) == "01" | str_sub(test$TERM,5,6) == "08")
  
  #Remove all AU, XF, I, W, WW, S, etc grades:
  test$CRS_GRADE = as.character(test$CRS_GRADE)
  c = c("A+", "A", "A-", "B+", "B", "B-", "C+", "C", "C-", "D+", "D", "D-", "F")
  test = test[which (test$CRS_GRADE %in% c),]
  dim(test)  

  #Recode letter grades to numerics
  test$CRS_GRADE = as.character(test$CRS_GRADE)
  test$CRS_GRADE[test$CRS_GRADE == "A+"] = 4
  test$CRS_GRADE[test$CRS_GRADE == "A"] = 4
  test$CRS_GRADE[test$CRS_GRADE == "A-"] =3.7
  test$CRS_GRADE[test$CRS_GRADE == "B+"] = 3.3
  test$CRS_GRADE[test$CRS_GRADE == "B"] = 3
  test$CRS_GRADE[test$CRS_GRADE == "B-"] =2.7
  test$CRS_GRADE[test$CRS_GRADE == "C+"] = 2.3
  test$CRS_GRADE[test$CRS_GRADE == "C"] = 2
  test$CRS_GRADE[test$CRS_GRADE == "C-"] = 1.7
  test$CRS_GRADE[test$CRS_GRADE == "D+"] = 1.3
  test$CRS_GRADE[test$CRS_GRADE == "D"] = 1
  test$CRS_GRADE[test$CRS_GRADE == "D-"] = .7 
  test$CRS_GRADE[test$CRS_GRADE == "F"] = 0

  test$CRS_GRADE = as.numeric(test$CRS_GRADE)
  dim(test)

  #Rename variables to CIC standards:
  names(test)[names(test) == 'COURSE'] <- 'CATALOGNBR'
  names(test)[names(test) == 'CRS_GRADE'] <- 'GRADE'
  names(test)[names(test) == 'CRS_CREDIT'] <- 'COURSECREDIT'

  #Keep only credits < = 12
  test$COURSECREDIT = as.character(test$COURSECREDIT)
  test$COURSECREDIT = as.numeric(test$COURSECREDIT)

  test = subset(test, test$COURSECREDIT > 0 & test$COURSECREDIT <=12)
    
  test$SUBJECT = str_sub(test$CATALOGNBR, 0, 4)

  #Save data
  sqlitecon = dbConnect(SQLite(), write_db)
  dbWriteTable(sqlitecon, "CourseHistory", test, overwrite = TRUE)
  dbListTables(sqlitecon)

#rawadmiss.csv
  fn = paste(read_dir, "rawadmiss.csv", sep = "")
  test = read.csv(fn, nrows = -1) 
  names(test)[names(test) == 'newid'] <- 'ID'
  names(test)[names(test) == 'FIRST_TERM'] <- 'FIRST_TERM_UNUSED'
  names(test)[names(test) == 'ENTRY_TERM'] <- 'FIRST_TERM'
table(test$UGA_ENTRY_TYPE)

  test_ = subset(test, FIRST_TERM == "200608")

  #Save data
  sqlitecon = dbConnect(SQLite(), write_db)
  dbWriteTable(sqlitecon, "Admiss", test, overwrite = TRUE)
  dbListTables(sqlitecon)

#rawBTERMS.csv
  fn = paste(read_dir, "rawBTERMS.csv", sep = "")
  test = read.csv(fn, nrows = -1) 
  names(test)[names(test) == 'newid'] <- 'ID'
  
  #Rename variables to CIC standards:
  names(test)[names(test) == 'LAST_CUM_CR_ERN_UG'] <- 'TOTCREDITS'
  names(test)[names(test) == 'LAST_CUM_GPA_UG'] <- 'CUM_GPA'
  names(test)[names(test) == 'RACE_ETHN_CITZ_MULTI_CD'] <- 'ETHNICITY'
  names(test)[names(test) == 'RACE_CITZ_CD'] <- 'ETHNICITY_2'

  #ETHNICITY
  test$ETHNICITY1FAC <- factor(test$ETHNICITY, levels = LETTERS[1:9],
  labels = c("White", 
             "Black or African American",
             "Asian", 
             "American Indian or Alaska Native", 
             "Native Hawaiian or Other Pacific Islander",
             "Hispanic",
             "Unknown",
             "Two or More",
             "Foreign"))
  
  table(test$ETHNICITY1_SHORT_FAC, useNA = "always")
  
  test$ETHNICITY1_SHORT = recode(test$ETHNICITY, "'A'='A'; 'B'='B'; 'C'= 'C'; 'G' = NA; else = 'D'")
  
  test$ETHNICITY1_SHORT_FAC <- factor(test$ETHNICITY1_SHORT, levels = LETTERS[1:4],
                             labels = c("White", 
                                        "Black or African American",
                                        "Asian", 
                                        "Other"))

  test$ETHNICITY2FAC <- factor(test$ETHNICITY_2, levels = c(0,1,2,3,4,5),
                             labels = c("Not Reported", 
                                        "American Indian or Alaskan Native",
                                        "Black or African American",
                                        "Asian", 
                                        "Hispanic",
                                        "White"))

#Save data
  sqlitecon = dbConnect(SQLite(), write_db)
  dbWriteTable(sqlitecon, "StudentTerms", test, overwrite = TRUE)
  dbListTables(sqlitecon)

#rawmajor_prelim.csv
  #We make 2 tables:
    #MajorTerms has all terms
    #FirstMajor has only the first declared major (earliest term)
  
  fn = paste(read_dir, "rawmajor_prelim.csv", sep = "")
  test = read.csv(fn, nrows = -1) 
  test$MAJOR[test$MAJOR == "LTSC"] = "LTSC UNDECID"
  names(test)[names(test) == 'newid'] = 'ID'

  #Remove Codes with # and & -- Minors?
  test = subset (test, regexpr("#", test$MAJOR_CD) != 1)
  test = subset (test, regexpr("&", test$MAJOR_CD) != 1)
  
  #Create valid CIP Codes and CIP_STEM variable
  #Based on this list: https://www.ice.gov/doclib/sevis/pdf/stem-list.pdf
  
  #The list of valid STEM CIPs:
  stem_cip_ = read.csv("STEM_CIP.csv")
  stem_cip = c(as.character(stem_cip_$stem_cip))
  stem_cip_ = NULL
  
  #Redo the CIP code to include the period:
  CIP1 = with(test, str_sub(test$CIP_CD,0,2))
  CIP2 = with(test, str_sub(test$CIP_CD,3,7))
  test$MAJOR_CIP_CD = with(test, paste(CIP1, ".", sep = ""))
  test$MAJOR_CIP_CD = with(test, paste(test$MAJOR_CIP_CD, CIP2, sep = ""))
  
  test$MAJOR_CIP_CD = ifelse(test$MAJOR_CIP_CD == ".", NA, test$MAJOR_CIP_CD)
  test$MAJOR_STEM_CIP = with(test, MAJOR_CIP_CD %in% as.character(stem_cip))
  test$MAJOR_STEM_CIP = ifelse(is.na(test$MAJOR_CIP_CD), NA, test$MAJOR_STEM_CIP)
  #table(test$MAJOR_CIP_CD, useNA = "always")
  #table(test$MAJOR_STEM_CIP, useNA = "always")

  #Save data
  sqlitecon = dbConnect(SQLite(), write_db)
  dbWriteTable(sqlitecon, "MajorTerms", test, overwrite = TRUE)

  #Select First semester for each student only
  scode = "SELECT * , MIN(TERM) as TERM FROM MajorTerms GROUP BY ID ORDER BY ID, TERM"
  test = dbGetQuery(sqlitecon, gsub("\n", " ", scode))
  
  #Add this variable label for CIC
  test$FIRST_DECLARE = test$MAJOR_CIP_CD
  test$FIRST_DECLARE_TERM = test$TERM
  test$FIRST_DECLARE_CIP_CD = test$MAJOR_STEM_CIP

  #Save data
  sqlitecon = dbConnect(SQLite(), write_db)
  test$row_names = NULL
  dbWriteTable(sqlitecon, "FirstMajor", test, overwrite = TRUE)
  dbListTables(sqlitecon)


#transeq.csv
  fn = paste(read_dir, "transeq.csv", sep = "")
  test = read.csv(fn, nrows = -1) 
  warnings()
  names(test)[names(test) == 'newid'] <- 'ID'

  #Save data
  sqlitecon = dbConnect(SQLite(), write_db)
  dbWriteTable(sqlitecon, "Transfers", test, overwrite = TRUE)
  dbListTables(sqlitecon)

#CREATE TABLE WITH THE SUMS OF TRANSFER CREDITS
  scode = "CREATE TABLE IF NOT EXISTS TransfersSum AS SELECT ID, sum(TRANS_CRS_EQUIV_CR) 
    AS TRANSFER_CREDITS_TOT FROM Transfers GROUP BY ID"
  gsub("\n", " ", scode)
  dbSendQuery (sqlitecon, gsub("\n", " ", scode))

#NOW THAT ALL OF THE INDIVIDUAL TABLES ARE CREATED, WE CREATE AND RETRIEVE THE STUDENT_COURSE TABLE ----
sqlitecon = dbConnect(SQLite(), write_db_final)

#Delete it if it's already there
scode ="DROP TABLE IF EXISTS STUDENT_COURSE"
dbSendQuery(sqlitecon, scode)

#The filter;
scode = "CREATE TABLE STUDENT_COURSE AS SELECT 

Admiss.UGA_DEG_SEEKING_IND,
Admiss.FIRST_TERM,

CourseHistory.CATALOGNBR, 
CourseHistory.GRADE, 
CourseHistory.COURSECREDIT, 
CourseHistory.TERM,
CourseHistory.ID,  
CourseHistory.SUBJECT,

StudentTerms.ETHNICITY,
StudentTerms.ETHNICITY_2,
StudentTerms.ETHNICITYFAC,
StudentTerms.ETHNICITY2FAC,
StudentTerms.Gender AS GENDER,
StudentTerms.CUM_GPA,
StudentTerms.TOTCREDITS,

TransfersSum.TRANSFER_CREDITS_TOT

FROM CourseHistory 
LEFT JOIN Admiss USING (ID) 
LEFT JOIN StudentTerms USING (ID, TERM) 
LEFT JOIN TransfersSum USING (ID)

WHERE Admiss.UGA_DEG_SEEKING_IND == 'Y' AND Admiss.FIRST_TERM >= 200608

ORDER BY ID, TERM" 

#Send the query to create the table:
dbSendQuery(sqlitecon, gsub("\n", " ", scode))

# Retrieve the table (sc) and make a few more modifications
write_db = "d:/UMD LARA Data/CIC_TABLES.db"
sqlitecon = dbConnect(SQLite(), write_db)
sc = dbGetQuery(sqlitecon, "SELECT * FROM STUDENT_COURSE")

sc$TRANSFER_CREDITS_TOT = as.numeric(sc$TRANSFER_CREDITS_TOT)

#ONLY RUN ONCE!!
#Add transfer credits to total credits 
sc$TOTCREDITS = sc$TOTCREDITS + sc$TRANSFER_CREDITS_TOT

#Create GPAO Using Term Data (There are more missing values here, so don't use)
#sc$TOTGRDPTS = sc$CUM_GPA * sc$TOTCREDITS
#sc$GPAO = (sc$TOTGRDPTS-sc$GRADE*sc$COURSECREDIT)/(sc$TOTCREDITS-sc$COURSECREDIT)

#Create GPAO Using Course Data (TAKES SOME TIME)

sc = ddply(sc, .(ID), transform, TOTCREDITS2 = cumsum(COURSECREDIT))
sc = ddply(sc, .(ID), transform, TOTGRDPTS2 = cumsum(COURSECREDIT*GRADE))
sc = ddply(sc, .(ID, TERM),  transform, TOTCREDITS2 = max(TOTCREDITS2) )
sc = ddply(sc, .(ID, TERM),  transform, TOTGRDPTS2 = max(TOTGRDPTS2) )
sc$GPAO = (sc$TOTGRDPTS2-sc$GRADE*sc$COURSECREDIT)/(sc$TOTCREDITS2-sc$COURSECREDIT)

#TOTCREDITS2, TOTGRDPTS2 are Temp variables only
#sc$TOTCREDITS2 = NULL
#sc$TOTGRDPTS2 = NULL

#cor(sc$TOTCREDITS, sc$TOTCREDITS2, use="complete.obs", method = "pearson")

#Create granular GPAO score:
sc$GPAO_GRANULAR = round(floor(sc$GPAO*3)/3, digits=1)

#Delete a couple of variables:
sc$UGA_DEG_SEEKING_IND = NULL
sc$'ID:1' = NULL

sc = AddCOURSE_CODE(sc, "Maryland")
check_missing_sc(sc)
names(sc)[names(sc) == 'TOTCREDITS2'] <- 'TOTALCREDITS'
names(sc)[names(sc) == 'TOTGRDPTS2'] <- 'TOTALGRADEPTS'

#SAVE TO CSV
#First select only the needed variables
sc2 = sc[,c("GENDER", "ETHNICITY", sc_required)]
names(sc2)

#Then select only the needed courses
cg = read.csv("course_grid.csv")
course_list = as.character(cg[[toupper("Maryland")]])
valid_courses = nchar(course_list) == 7
course_list = course_list[valid_courses== TRUE]
#Check:
course_list 

sc3 = sc2[sc2$CATALOGNBR %in% course_list ,] 
dim(sc3)
write.csv(sc3, "d:/UMD LARA Data/MD_SC_SHORT.csv")

#Save modified data
#names(sc)
sqlitecon = dbConnect(SQLite(), write_db)
sc$row_names = NULL
dbWriteTable(sqlitecon, "STUDENT_COURSE", sc, overwrite = TRUE)
table(sc2$TERM)

#Create THE STUDENT_RECORD TABLE ----

#Create a table with information from the student's final term
dbSendQuery(sqlitecon, "DROP TABLE IF EXISTS LastTerm")

sqlitecon = dbConnect(SQLite(), write_db)
scode = "CREATE TABLE LastTerm AS SELECT ID, TOTCREDITS, CUM_GPA, ETHNICITY, ETHNICITY1FAC, ETHNICITY2FAC, ETHNICITY1_SHORT_FAC, Gender AS GENDER, max(TERM) from StudentTerms GROUP BY ID"
dbSendQuery(sqlitecon, scode)

dbSendQuery(sqlitecon, "DROP TABLE IF EXISTS STUDENT_RECORD")

sqlitecon = dbConnect(SQLite(), write_db)

scode = "CREATE TABLE STUDENT_RECORD AS SELECT 

Admiss.FIRST_TERM as FIRST_TERM,
Admiss.UGA_ENTRY_TYPE_CD,  		
Admiss.INST_FOUR_YEAR_IND,
Admiss.HIGH_SCHOOL,
Admiss.HS_ACAD_GPA,
Admiss.SAT_HIGH_MATH,
Admiss.SAT_HIGH_CRITICAL_READING,
Admiss.UGA_ENTRY_TYPE,
Admiss.INST_MD_SCHOOL_IND,
Admiss.INST_COMMUNITY_COLL_IND,
Admiss.LAST_TRANS_INST_CD,
Admiss.REQ_COLL_OF_MAJ,
Admiss.GEOG_ORIGIN_CD,
Admiss.firstgen1,
Admiss.TRANSFER_GPA,
Admiss.GEOG_ORIGIN,
Admiss.alum1,
Admiss.ADMIS_FULL_PART_TIME_CD,				
Admiss.ZIP5,  			

TransfersSum.TRANSFER_CREDITS_TOT,

FirstMajor.MAJOR,
FirstMajor.MAJOR_CD,
FirstMajor.FIRST_DECLARE,
FirstMajor.FIRST_DECLARE_TERM,
FirstMajor.MAJOR_STEM_CIP,

Degrees.ID,
Degrees.DEGREE_TERM_1,
Degrees.DEGREE_MAJOR_1,
Degrees.DEGREE_CIP_CD_1,
Degrees.DEGREE_STEM_CIP_1,
Degrees.DEGREE_TERM_2,
Degrees.DEGREE_MAJOR_2,
Degrees.DEGREE_CIP_CD_2,
Degrees.DEGREE_STEM_CIP_2,

LastTerm.ETHNICITY,
LastTerm.ETHNICITY1FAC,
LastTerm.ETHNICITY1_SHORT_FAC,
LastTerm.ETHNICITY2FAC,
LastTerm.GENDER,
LastTerm.CUM_GPA,
LastTerm.TOTCREDITS

FROM Admiss
LEFT JOIN TransfersSum  USING (ID) 
LEFT JOIN Degrees  USING (ID) 
LEFT JOIN FirstMajor USING (ID) 
LEFT JOIN LastTerm  USING (ID) 

WHERE Admiss.UGA_DEG_SEEKING_IND == 'Y' AND Admiss.FIRST_TERM >= 200608

ORDER BY ID"
dbSendQuery(sqlitecon, gsub("\n", " ", scode))

#Retrive it and make a few more mods
read_dir = "d:/UMD LARA Data/"
sqlitecon = dbConnect(SQLite(), write_db)
sr = dbGetQuery(sqlitecon, "SELECT * FROM STUDENT_RECORD")
sort(names(sr))

sr = sr[c("ID", "FIRST_TERM","FIRST_DECLARE","FIRST_DECLARE_TERM",
  "MAJOR", "MAJOR_CD", "MAJOR_STEM_CIP", "DEGREE_STEM_CIP_1","HIGH_SCHOOL",
  "HS_ACAD_GPA","SAT_HIGH_MATH", 
  "SAT_HIGH_CRITICAL_READING","UGA_ENTRY_TYPE","INST_MD_SCHOOL_IND",
  "INST_COMMUNITY_COLL_IND","LAST_TRANS_INST_CD",
  "GEOG_ORIGIN","GEOG_ORIGIN_CD", "firstgen1","TRANSFER_GPA",
  "alum1","ADMIS_FULL_PART_TIME_CD",
  "ZIP5","TRANSFER_CREDITS_TOT",
  "DEGREE_TERM_1","DEGREE_MAJOR_1","DEGREE_CIP_CD_1",
  "DEGREE_STEM_CIP_1","DEGREE_TERM_2","DEGREE_MAJOR_2",
  "DEGREE_CIP_CD_2","DEGREE_STEM_CIP_2", "ETHNICITY",
  "ETHNICITY1FAC","ETHNICITY1_SHORT_FAC", "ETHNICITY2FAC",
  "GENDER")]

#which(LETTERS=="A")

names(sr)[names(sr) == 'DEGREE_TERM_1'] <- 'DEGREE_TERM'
names(sr)[names(sr) == 'DEGREE_CIP_CD_1'] <- 'MAJOR1'
names(sr)[names(sr) == 'DEGREE_MAJOR_1'] <- 'MAJOR1_LONG'
names(sr)[names(sr) == 'MAJOR'] <- 'FIRST_DECLARE_LONG'

sr$TRANSFER = is.na(sr$TRANSFER_GPA)

check_missing_sr(sr)
sr_ = sr[,sr_required]
write.csv(sr_, "d:/UMD LARA Data/MD_SR.csv")
sr_required
sr_
sr$row_names=NULL
dbWriteTable(sqlitecon, "STUDENT_RECORD", sr, overwrite = TRUE)

#Calculate TermsOnCampus
TermList = c(NA, "199801", "199808", "199901", "199908", "200001", "200008", "200101", "200108",
             "200201", "200208", "200301", "200308", "200401", "200408", "200501", "200508",
             "200601", "200608", "200701", "200708", "200801", "200808", "200901", "200908",
             "201001", "201008", "201101", "201108", "201201", "201308", "201401", "201408")

sr$TermsOnCampus = which(TermList==sr$DEGREE_TERM_1) - which(TermList==sr$FIRST_TERM_1) + 1
sr$TermsOnCampus

#Now delete the unused tables
#for (i in dbListTables(sqlitecon)) 

#{if (i != "STUDENT_COURSE" & i != "STUDENT_RECORD") 
#  {
#  dbSendQuery(sqlitecon, paste("DROP TABLE ", i))  
#  }
#}


