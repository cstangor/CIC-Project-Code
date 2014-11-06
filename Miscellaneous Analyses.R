
# I've been working on a tableau-like website where people can browse their own data
# I'd like to create a shared site for us.  Can be pwd protected

Definition of transfer?
  
Also, the previous meeting, we had discussed this set of possible next steps. Perhaps you could update us on where you are with these?

# make GP by genderGP table
# Reorder charts alphabetically by course # and add course name, type
* Analyze impact of cookbook decisions (i.e., repeated courses)
* Compare across multiple sections - to see some measure of instructor dependence
* Analyze by W to see who is making these decisions by GPA
# Add N (M/F) per data point
* Prior training indicators (AP, IB, SAT scores) John sent me code but I just didn't get to it yet

# Analyze by race/ethnicity
* Matching
- other courses

setwd("c:/users/n/onedrive/documents/research/CIC project/UMD LARA Data/")
source ("c:/users/n/onedrive/documents/research/r/txt/startup.R")

#CONNECT TO SQLITE DATABASE CONTAINING ALL 6 TABLES
sqlitecon = dbConnect(SQLite(), "CIC.db")
dbListTables(sqlitecon)


# Grade Penalty by Gender Penalty ----
library ("ggplot2")
sc = read.csv("d:/UMD LARA Data/MD_SC_SHORT.csv")
sc$penalty = sc$GRADE-sc$GPAO
sc = read.csv("d:/UMD LARA Data/MD_SC_SHORT.csv")
sc$penalty = sc$GRADE-sc$GPAO
describe(sc$penalty)
out1 = aggregate(sc$penalty, by = list(sc$GENDER, sc$CATALOGNBR), FUN = c("mean"))
names(out1) = c("GENDER", "CATALOGNBR", "penalty")
out1$ID = rownames(out1)
out.wide = reshape(out1, v.names = "penalty", idvar="CATALOGNBR", timevar="GENDER", direction = "wide")
out.wide$penalty = (out.wide$penalty.Male + out.wide$penalty.Female)/2
out.wide$penalty_diff = (out.wide$penalty.Male - out.wide$penalty.Female)/2

names(out.wide)
ggplot(out.wide, aes(x=penalty, y=penalty_diff, label=out.wide$CATALOGNBR)) +
  geom_text() +
  #ylim(-2,2)  + xlim (-2,2) +
  geom_smooth(method=lm)   # Add linear regression line 


# OVerall grade penalty ----
sc$penalty = sc$GRADE-sc$GPAO
aggregate(sc$penalty, by = list(sc$GENDER), 
          FUN = c(M = "mean", "sd", "count"))

#Plus/minus grades ----
setwd("c:/users/n/onedrive/documents/research/CIC project/UMD LARA Data/")
read_dir = "d:/UMD LARA Data/"
sqlitecon = dbConnect(SQLite(), paste(read_dir, "CIC_TABLES.db", sep = ""))
scode = "select * from student_course"
sc = dbGetQuery(sqlitecon, scode)
dim(sc)
table(sc$GRADE, sc$TERM)

sc$l = sc$GRADE != floor(sc$GRADE)
table (sc$l, sc$GRADE)
tt = CrossTable(sc$TERM, sc$l, prop.t = FALSE, prop.r = TRUE, prop.c = TRUE, prop.chisq = TRUE, missing.include = TRUE) #, format = c("SPSS"))

ttt = data.frame(tt)
ttt = head(ttt, n = 16)

trimm <- function(x) return(str_sub(x, 0,4))

ggplot(data=ttt, aes(x=as.character(t.x), y = as.numeric(prop.row.Freq), stat = "identity" )) + 
  geom_histogram(stat = "identity") +
  ggtitle("Proportion of grades that are Minus or Plus") +
  scale_x_discrete(labels = trimm ) + #(LETTERS[1:16]) + #,str_sub(prop.row.Freq, 4)) +  #abbreviate
  labs(y = "Proportion", x = "Semester") +
scale_x_discrete(labels = trimm ) + #(LETTERS[1:16]) + #,str_sub(prop.row.Freq, 4)   #abbreviate
  
  
  
# COURSE PERSISTENCE ----
# Plot proportion of students at each grade in Course 1 who subsequently take Course 2
#data <- read.table("c:/users/n/downloads/sample.tab",sep="\t",header=TRUE)
#data$COURSE = paste(data$SUBJECT, data$CATALOGNBR, sep = "")

sqlitecon = dbConnect(SQLite(), "d:/UMD LARA Data/CIC_TABLES.db")
sc = dbGetQuery(sqlitecon, "SELECT * FROM STUDENT_COURSE")

courselist_1 = c("PHYS121", "CHEM231", "MATH220")
courselist_2 = c("PHYS122", "CHEM232", "MATH221")

pdf("Course Persistence.pdf")

#Loop through all of the courses and print each plot:

for( i in 1:3)
  
{
course_1 = courselist_1[i]
course_2 = courselist_2[i]

#Phys 121 and Phys 122, Chem 231 and chem 232, Math 220 and Math 221??

data1 = subset(sc, CATALOGNBR == course_1)
#choose the last taking of course_1
data1 =data1[!duplicated(data1[, "ID"], fromLAST = TRUE), ] 

data2 = subset(sc, CATALOGNBR == course_2)

#Save data into a temp SQLITE database to allow a query
sqlitecon = dbConnect(SQLite(), "d:/UMD LARA Data/TEMP.db")
data1$row_names = NULL
dbWriteTable(sqlitecon, "data1", data1, overwrite = TRUE)
data2$row_names = NULL
dbWriteTable(sqlitecon, "data2", data2, overwrite = TRUE)

#Make the query
data3 = dbGetQuery(sqlitecon, "SELECT data1.ID AS ID,  data1.GENDER, data1.GRADE as GRADE1, data2.GRADE as GRADE2  FROM data1 LEFT JOIN data2 USING (ID) ORDER BY GRADE1")

data3$completed_next_course =  ifelse(is.na(data3$GRADE2), 0, 1)

#data3_ = aggregate(data3$completed_next_course, by = list(data3$GRADE1), FUN = c("mean", "se"))
#names(data3_) = c("x", "y", "se")

data3_ = aggregate(data3$completed_next_course, by = list(data3$GRADE1, data3$GENDER), FUN = c("mean", "se"))
names(data3_) = c("x", "Gender", "y", "se")

data3_
A = paste("Grade In", course_1)
B = paste("Proportion Completed", course_2)

  #print(ggplot(data=data3_, aes(x=factor(x), y=y))  +  
  print(ggplot(data=data3_, aes(x=factor(x), y=y, group = Gender, color = Gender))  +  
  geom_errorbar(aes(ymax = y + se, ymin=y - se)) + 
  ggtitle(concat(c("Persistence from ", course_1, " to ", course_2))) + 
  labs(x = A , y = B) + 
  #theme(axis.text = element_text(size = rel(2))) +
  #theme(axis.title.x = element_text(size = rel(2))) +
  #theme(plot.title = element_text(size = rel(2))) +
  #theme(axis.title.y = element_text(size = rel(2))) +
  scale_x_discrete(labels=c("F", "D-", "D", "D+", "C-", "C", "C+", "B-", "B", "B+", "A-", "A")))

}
dev.off()

# GRADE INFLATION ---------------
# NEED TO REREAD FROM THE .CSV TO GET ALL SEMESTERS BACK TO 1998
read_dir = "d:/UMD LARA Data/original data/"
hc = read.csv(paste(read_dir, "histcrs_short.csv", sep = ""), nrows = -1) 
dim(hc)
names(hc)

#Keep only fall and spring semesters
hc = subset(hc, str_sub(hc$TERM,5,6) == "01" | str_sub(hc$TERM,5,6) == "08")
hc$SUBJECT = str_sub(hc$COURSE, 0, 4)

hc$CRS_GRADE = as.character(hc$CRS_GRADE)
hc$CRS_GRADE[hc$CRS_GRADE == "A+"] = 4
hc$CRS_GRADE[hc$CRS_GRADE == "A"] = 4
hc$CRS_GRADE[hc$CRS_GRADE == "A-"] =3.7
hc$CRS_GRADE[hc$CRS_GRADE == "B+"] = 3.3
hc$CRS_GRADE[hc$CRS_GRADE == "B"] = 3
hc$CRS_GRADE[hc$CRS_GRADE == "B-"] =2.7
hc$CRS_GRADE[hc$CRS_GRADE == "C+"] = 2.3
hc$CRS_GRADE[hc$CRS_GRADE == "C"] = 2
hc$CRS_GRADE[hc$CRS_GRADE == "C-"] = 1.7
hc$CRS_GRADE[hc$CRS_GRADE == "D+"] = 1.3
hc$CRS_GRADE[hc$CRS_GRADE == "D"] = 1
hc$CRS_GRADE[hc$CRS_GRADE == "D-"] = .7 
hc$CRS_GRADE[hc$CRS_GRADE == "F"] = 0
hc$CRS_GRADE[hc$CRS_GRADE == "*"] = NA
hc$CRS_GRADE[hc$CRS_GRADE == "AU"] = NA
hc$CRS_GRADE[hc$CRS_GRADE == "C#"] = NA
hc$CRS_GRADE[hc$CRS_GRADE == "CC"] = NA
hc$CRS_GRADE[hc$CRS_GRADE == "D#"] = NA
hc$CRS_GRADE[hc$CRS_GRADE == "I"] = NA
hc$CRS_GRADE[hc$CRS_GRADE == "P"] = NA
hc$CRS_GRADE[hc$CRS_GRADE == "W"] = NA
hc$CRS_GRADE[hc$CRS_GRADE == "WW"] = NA
hc$CRS_GRADE = as.numeric(hc$CRS_GRADE)

names(hc)[names(hc) == 'CRS_GRADE'] <- 'GRADE'
names(hc)[names(hc) == 'CRS_CREDIT'] <- 'CREDITS'

#SELECT ONLY SOME SUBJECTS
hc_small = subset(hc, SUBJECT  == 'PSYC' | SUBJECT == 'PHYS' | SUBJECT == "MATH")
table (hc_small$SUBJECT)
hc_small$TERM2 = str_sub(hc_small$TERM,0,4)

out2 = aggregate(hc_small$GRADE, by = list(hc_small$TERM2, hc_small$SUBJECT), FUN = c("mean", "sd", "se", "count"))

names(out2) = c("Term", "Subject", "Course_Grade","sd", "se","N")

out3 = aggregate(hc_small$GRADE, by = list(hc_small$TERM2), FUN = c("mean", "sd", "se", "count"))
names(out3)
names(out3) = c("Term",  "Course_Grade","sd", "se","N")

#out2$Term2 = str_sub(out2$Term,0,4)
#out2$Semester = str_sub(out2$Term,5)

ggplot(data=out2, aes(x=as.character(Term), y=Course_Grade, group=Subject, colour=Subject))  + 
    geom_line(size=1) + 
    labs(x = "Semester", y = "Course Grade", subtitle="Grade Inflation") + 
    ggtitle("Mean Assigned Grades By Semester, 1998-2014") +
    geom_line(data=out3, size = 1, aes(y=Course_Grade, group = 1, colour = "Overall")) +
  scale_color_manual(values=c("red", "black","green","blue"))
  
  
hc_small$GRADE
