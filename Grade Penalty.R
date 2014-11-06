#Enter the name of your school below:
current_school = "Maryland"

#Set your working directory.  All data is read and written here:
setwd("c:/users/n/onedrive/documents/research/CIC project/UMD LARA Data/")

#Put this source code in the working directory.
source ("startup.R")

#CONNECT TO SQLITE DATABASE CONTAINING ALL 6 TABLES
#THESE CREATED USING CIC_SETUP.R
#The name of the output SQlite database for the individual tables:
read_dir = "d:/UMD LARA Data/"
sqlitecon = dbConnect(SQLite(), paste(read_dir, "CIC_TABLES.db", sep = ""))
dbListTables(sqlitecon)

#Now connect to the student_course table and read it:

#Select everyone:
#scode = "select * from student_course "

#Select only courses that have not been repeated
scode = "select * from student_course group by ID, CATALOGNBR HAVING  count(*) = 1"
sc = dbGetQuery(sqlitecon, scode)
names(sc)

#REMOVE ANYONE FOR WHOM WE DON"T KNOW GENDER
sc = subset(sc, ! is.na(GENDER))
dim(sc)

#WHAT ARE THE BIGGEST COURSES?
#ta = table(sc$CATALOGNBR)
#cl = tail(ta[order(ta)], n = 30) 
#course_list= rev(cl)
#course_list =names(course_list)

# USE FOR A SINGLE COURSE (e.g c("PSYC100"):
#course_list = c("CHEM131")

#find the relevant course names and short codes from the Course Grid
cg = read.csv("course_grid.csv")
course_list = as.character(cg[[toupper(current_school)]])
valid_courses = nchar(course_list) == 7
course_list = course_list[valid_courses== TRUE]
course_list 

#Get the corresponding Short Code
short_codes = as.character(cg$SHORT_CODE)
short_codes = short_codes[valid_courses== TRUE]
short_codes = sort(short_codes)

#check: should be TRUE
length(short_codes) == length(course_list)

#if you're going to sort you need to do this:
#d2 = data.table(course_list, short_codes)
#d2 = d2[with(d2, order(course_list)), ]
#course_list = df$course_list
#short_codes = df$short_codes

#All of the plots in the next section will go to this file:

pdf("gender_penalty.pdf")

#Loop through all of the courses and print each plot:

for( i in 1:sort(length(course_list)))
    
{

#CHOOSE A COURSE TO ANALYZE
cn = course_list[i]
short_cn = short_codes[i]
wc = subset(sc, CATALOGNBR == cn)
wc = subset(wc, GPAO_GRANULAR >=0 & GPAO_GRANULAR <=4)
#Could be a course with no enrollment
if (dim(wc)[1] > 50)

{

    print(paste("Printing ", cn))
    #Calculate Summary Statistics
    out = aggregate(wc$GRADE, by = list(wc$GENDER, wc$GPAO_GRANULAR ), 
        FUN = c("mean", "sd", "se", "count"))
    
    names(out) = c("Gender", "Adj_Cum_GPA","Course_Grade","sd", "se","N")
    
    #percents
      d = aggregate(out$N, by = list(out$Gender), FUN = c("sum"))
      female_pct = paste(round(d[1,2]/sum(d[1,2]+d[2,2]) * 100, digits=0), "%", sep = "")
      male_pct =   paste(round(d[2,2]/sum(d[1,2]+d[2,2]) * 100, digits = 0), "%", sep = "")
      classsize = format(dim(wc)[1], big.mark = ",")
      title = concat(c(short_cn, " (N = ", classsize, ")\n ",  
          female_pct, " Females ", male_pct, " Males"))
      #Gender Comparisons by Course Grade
      d = aggregate(wc$GRADE, by = list(wc$GENDER), FUN = c("mean"))
      female_mn = paste(" Mean = ", round(d[1,2], digits=2))
      male_mn =   paste(" Mean = ", round(d[2,2], digits = 2))
      c_text = concat(c("Females: ", female_mn, " \n Males: ", male_mn))
      
      #Grade difference d value
      t = t.test(wc$GRADE~wc$GENDER)
      #Cohen's d = 2t /???(df)
      dval = round (2 * t$statistic * -1 / t$parameter^.5, digits = 2)
      c_text = concat(c(c_text, " \n d = ", dval, "\n"))
      c_text
    
      #Gender Comparisons by Grade Penalty
      wc$penalty = wc$GRADE-wc$GPAO
      d = aggregate(wc$penalty, by = list(wc$GENDER), FUN = c("mean"))
      female_mn = paste(" Mean = ", round(d[1,2], digits=2))
      male_mn =   paste(" Mean = ", round(d[2,2], digits = 2))
      c_text2 = concat(c("Females: ",  female_mn, " \n Males: ", male_mn))
      c_text2
      
      #Grade Penalty D Value
      t = t.test(wc$penalty~wc$GENDER)
      dval = round (abs(2 * t$statistic * -1/ t$parameter^.5), digits = 2)
      c_text2 = concat(c(c_text2, " \n d = ", dval))
      c_text2
    
      c_text3 = concat(c("GRADES:\n", c_text, "\nGRADE PENALTY:\n", c_text2) )
    
    #Create Plot
    limits <- aes(ymax = Course_Grade + se, ymin=Course_Grade - se)
    print (ggplot(data=out, aes(x=Adj_Cum_GPA, y=Course_Grade, group=Gender, colour=Gender))  + 
        #geom_point() + 
        geom_bar(data = out, aes(x = Adj_Cum_GPA, y = N/sum(N)*2, 
            colour = Gender, fill = Gender), stat = "identity") +
        geom_errorbar(limits, size = 1, width = 0.2) + 
        #geom_text(aes(label = N, y = Course_Grade - se*2), size = 3, colour = "black") + 
        geom_line(aes(y=Course_Grade-sd),  size = 1, linetype = "dotted")  + 
        geom_line(aes(y=Course_Grade+sd),  size = 1, linetype = "dotted")  + 
        ggtitle(concat(c(title, "\n\n", c_text3))) +
        theme(plot.title = element_text(size = rel(1))) + 
        geom_abline(slope=1, intercept=0, size = 1, colour = "black", linetype = "solid") + 
        ylim(0,4)  + xlim (-0.2,4.2) +
        labs(y = "Course Grade", x = "Adjusted Cumulative GPA", subtitle=""))
        #annotate("text", x = 1, y = 3.5, label = c_text3))
        #coord_cartesian(ylim=c(0,4))
    

} #if course size > n
  
} #Main Loop
dev.off()
