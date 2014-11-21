getwd()
basic.persistence()
basic.persistence <- function()
{
  #Read the STUDENT-COURSE table in. This is my local path.
  #data <- read.delim("~/REBUILD/CICcode/student.course.sample.tab")

  print('reading data')
  workdir <- "d:/umd lara data/"
  data <- read.delim(paste(workdir,"md_sr_nb_2.csv",sep=""), sep = ",")
  sr   <- read.delim(paste(workdir,"md_sc_short_nb_2.csv",sep=""), sep = ",")
  
  #Now merge them so that we can split on demography if we want.
  #Note that due to strict definitions of the student-course and student-record table, 
  #not ALL rows in the student-course table will have a matching
  #student-record. This is because we cut off at FIRST_TERM == FAll 2006
  #in the SR, and TERM=Fall 2006 in the SC table. Thus, people who took a
  #course as a senior in Fall 2006 will have unknown gender, ethinicity, major, etc.
  #In R, these people will have a gender of NA of the merge. Of course these problems will
  #be mitigated if locally, you decide on an earlier cutoff date in the student-record.
  
  print('merging STUDENT-COURSE with STUDENT-RECORD table')
  data <- merge(data,sr,by='ID',all.x=TRUE)
  
  print(names(data))
  print('read table')
  pdf(paste(workdir,'persistence.by.grade.pdf',sep=""),width=11,height=7)
  plot.aggregate.persistence(data,workdir) 
  plot.standard.grade.persistence(data)
  dev.off()
  
  
  pdf(paste(workdir,'persistence.by.gpen.pdf',sep=""),width=11,height=7)
  #Now bin the grade penalties for plotting purposes
  data$GRADE <- data$GPAO-data$GRADE
  disgp      <- discrete.grade.penalty(data)
  plot.aggregate.persistence(data,workdir,GPEN=TRUE) 
  data$GRADE <- disgp
  plot.standard.grade.persistence(data,GPEN=TRUE)
  dev.off()
  
}

##########################################
#PLOT.STANDARD.GRADE.PERSISTENCE
#This makes the familiar probability as a function of grade 
#plot for all existing course-pairs in the student-course table.
plot.standard.grade.persistence <- function(data,GPEN=FALSE)
{ 
  #Now compare the local student-course table to the master course grid
  #To find out which "standard" sequences are found in the STUDENT-COURSE table.
  course_grid <- local.sequence.matches(data)
  npairs      <- dim(course_grid)[1]
  if (is.null(npairs))
    {
      npairs <- 1
      ctemp <- course_grid
      course_grid <- mat.or.vec(1,2)
      course_grid[1,1] <- ctemp[1]
      course_grid[1,2] <- ctemp[2]
    }
  
  #Now loop over all of the those pairs and create the plot with error bars.
  for (j in 1:npairs)
  {
    print(paste(course_grid[j,1],course_grid[j,2],sep=" "))
    
    m <- which(data$GENDER == 'M')
    outm <- compute.persistence(data[m,],course_grid[j,1],course_grid[j,2])
    f <- which(data$GENDER == 'F')
    outf <- compute.persistence(data[f,],course_grid[j,1],course_grid[j,2])
    
    if (GPEN == FALSE)
    {
      xlab  <- paste('Grade in ',course_grid[j,1],sep="")
      title <- paste('Persistence from',course_grid[j,1],'to',course_grid[j,2],sep=" ")
    }
    if (GPEN == TRUE)
    {
      xlab  <- paste('GPAO-Grade in ',course_grid[j,1],sep="")
      title <- paste('Persistence from',course_grid[j,1],'to',course_grid[j,2],sep=" ")
    }
    
    plot(outf$grade,outf$gfrac,ylim=c(0,1.0),
         xlab=xlab,pch=19,ylab='Fraction',
         main=title)
    points(outm$grade,outm$gfrac,pch=19,col='red')
    #add error bars
    for (k in 1:length(outf$gfrac))
    {
      arrows(outf$grade[k],outf$gfrac[k]-outf$gse[k],outf$grade[k],outf$gfrac[k]+outf$gse[k],code=0)
    }
    for (k in 1:length(outm$gfrac))
    {
      arrows(outm$grade[k],outm$gfrac[k]-outm$gse[k],outm$grade[k],outm$gfrac[k]+outm$gse[k],code=0,col='red')
    }
  }
  
  #and done.
  
}

#########################################
#PLOT.AGGREGATE.PERSISTENCE
#Instead of binning by grade, just look at the mean grade and mean persistence fraction
#so that we may look at several courses on one plot.
plot.aggregate.persistence <- function(data,workdir,label=TRUE,GPEN=FALSE)
{
  #Now check the local student-course table to find out which pairs are represented.
  course_grid <- local.sequence.matches(data)
  npairs      <- dim(course_grid)[1]
  
  if (is.null(npairs))
  {
    npairs <- 1
    ctemp <- course_grid
    course_grid <- mat.or.vec(1,2)
    course_grid[1,1] <- ctemp[1]
    course_grid[1,2] <- ctemp[2]
  }
  
  #Set up the table to write to
  if (GPEN == FALSE){fname  <- 'persistence.by.grade.txt'}
  if (GPEN == TRUE){fname   <- 'persistence.by.gpen.txt'}
  
  fname  <- paste(workdir,fname,sep="")
  header <- paste('SEQUENCE','FEMALE_GRADE','FEMALE_GRADE_SE','FEMALE_PROB','FEMALE_PROB_SE',
                         'MALE_GRADE','MALE_GRADE_SE','MALE_PROB','MALE_PROB_SE',sep=",")
  write((header),file=fname,append=FALSE,sep=",") #,quote=FALSE,row.names=FALSE)
  
  for (j in 1:npairs)
  {
    print(paste(course_grid[j,1],course_grid[j,2],sep=" "))
    m     <- which(data$GENDER == 'M')
    datam <- data[m,]
    f     <- which(data$GENDER == 'F')
    dataf <- data[f,]
    print('computing all genders')
    out  <- compute.persistence(data,course_grid[j,1],course_grid[j,2],aggregate=TRUE)
    print('computing females')
    outf <- compute.persistence(dataf,course_grid[j,1],course_grid[j,2],aggregate=TRUE)
    print('computing males')
    outm <- compute.persistence(datam,course_grid[j,1],course_grid[j,2],aggregate=TRUE)
    
    if (GPEN == FALSE)
    {
      xlab <- 'Mean Grade in Class I'
      xlim <- c(2.6,3.2)
    }
    if (GPEN == TRUE)
    {
      xlab <- 'Mean Grade Penalty in Class I'
      xlim <- c(0,1.0)
    }
    
    if (j == 1)
    {
      plot(outf$grade,outf$gfrac,ylim=c(0,1),xlim=xlim,
          xlab=xlab,pch=19,ylab='Persistence Fraction',
          main='Aggregrate Persistence for Standard Sequences' ) 
      points(outm$grade,outm$gfrac,pch=19,col='red')
    } else
    {
      points(outf$grade,outf$gfrac,pch=19)
      points(outm$grade,outm$gfrac,pch=19,col='red')
    }
    #add error bars
    for (k in 1:length(out$gfrac))
    {
      if (label == TRUE)
      {
        text(outf$grade[k],outf$gfrac[k],srt=90,
             paste(course_grid[j,1],course_grid[j,2],sep="/"),pos=4,cex=0.5)
        text(outm$grade[k],outm$gfrac[k],srt=90,
             paste(course_grid[j,1],course_grid[j,2],sep="/"),pos=4,cex=0.5,col='red')
      }
      seq  <- (paste(course_grid[j,1],course_grid[j,2],sep="/"))
      nvec <- paste(seq,outf$grade[k],outf$grade_se[k],outf$gfrac[k],outf$gse[k],
                        outm$grade[k],outm$grade_se[k],outm$gfrac[k],outm$gse[k],sep=",")
      #write.table(nvec,file=fname,append=TRUE,quote=FALSE,sep=",",row.names=FALSE)
      write(nvec,file=fname,append=TRUE,sep=",") #,row.names=FALSE,quote=FALSE)
      
      arrows(outf$grade[k],outf$gfrac[k]-outf$gse[k],outf$grade[k],outf$gfrac[k]+outf$gse[k],code=0)
      arrows(outf$grade[k]-outf$grade_se[k],outf$gfrac[k],outf$grade[k]+outf$grade_se[k],outf$gfrac[k],code=0)
      arrows(outm$grade[k],outm$gfrac[k]-outm$gse[k],outm$grade[k],outm$gfrac[k]+outm$gse[k],code=0,col='red')
      arrows(outm$grade[k]-outm$grade_se[k],outm$gfrac[k],outm$grade[k]+outm$grade_se[k],outm$gfrac[k],code=0,col='red')
    }
  }
  if (GPEN == FALSE){legend(2.6,0.8,c("Males","Females"),text.col=c('red','black'))}
  if (GPEN == TRUE) {legend(0.8,0.8,c("Males","Females"),text.col=c('red','black'))}
}

#########################################
#COMPUTE.PERSISTENCE
#This is the core function.
#Persistence here is defined as the probability a student took course 2, given that
#they took course 1. No explicit time dependence...
compute.persistence <- function(data,crse1,crse2,NONSTD=FALSE,pair,aggregate=FALSE)
{
  #First,pick out course 1 and handle duplicates
  if (NONSTD == FALSE){ee   <- data$COURSE_CODE == crse1}
  if (NONSTD == TRUE) {ee <- data$SUBJECT == pair[1] & data$CATALOGNBR == pair[2]}
  sub1 <- data[which(ee),]
  keep <- remove.duplicates(sub1,keep="LAST",verbose=TRUE)
  sub1 <- sub1[keep,]
  
  #Then,pick out course 2 and handle duplicates
  if (NONSTD == FALSE){ee   <- data$COURSE_CODE == crse2}
  if (NONSTD == TRUE) {ee   <- data$SUBJECT == pair[3] & data$CATALOGNBR == pair[4]}
  sub2 <- data[which(ee),]
  keep <- remove.duplicates(sub2,keep='NONE',verbose=TRUE)
  sub2 <- sub2[,names(sub2) %in% c("ID","TERM")]
  names(sub2) <- c("ID","TERM2") #Don't want to confuse two TERM columns!

  #Now use the merge function to match these (LEFT JOIN on sub1)

  sub <- merge(sub1,sub2,by='ID',all.x=TRUE)
  
  #Now pre-sort for efficiency.
  sub       <- sub[order(sub$GRADE), ]
  sub$count <- sequence(rle(as.vector(sub$GRADE))$lengths)
  ntot   <- length(sub$GRADE)
  nid    <- length(sub$GRADE[!duplicated(sub$GRADE)])
  nstart <- which(sub$count == 1)
  
  grade  <- mat.or.vec(nid,1)
  grade_se <- NA
  n1     <- mat.or.vec(nid,1)
  n2     <- mat.or.vec(nid,1)
  term1  <- mat.or.vec(nid,1)
  term2  <- mat.or.vec(nid,1)
  
  #Check each grade class for 1) taking crse1/crse2 and count. 
  #There is no time-depenendence built-in, but we keep
  #term1 and term2 so that it might be built-in later.
  print('sorting by grade')
  for (i in 1:nid)
  {
    start_ind <- nstart[i]
    if (i < nid){stop_ind  <- nstart[i+1]-1}
    if (i == nid){stop_ind <- ntot}
    ind       <- c(start_ind:stop_ind)
    n1[i] <- length(ind)
    n2[i] <- sum(!is.na(sub$TERM2[ind]))
    grade[i] <- sub$GRADE[start_ind]
    term1[i] <- sub$TERM[start_ind]
    term2[i] <- sub$TERM2[start_ind]
}
  print('finished grade sort')
  gfrac <- n2/n1  #fraction that continue
  gse   <- sqrt(gfrac[i]*(1-gfrac[i]))/sqrt(n1) #binomial standard error

  #for the aggregate option
  if (aggregate == TRUE)
  {
    gfrac <- sum(n2)/sum(n1)                 #Overall persistence from 1 ---> 2
    gse   <- sqrt(gfrac*(1-gfrac)/sqrt(sum(n1))) #Standard error
    grade <- mean(sub1$GRADE)                #Mean grade in course 1
    grade_se <- sd(sub1$GRADE)/sqrt(sum(n1)) #Grade standard error in course 1
  }
  
  out <- data.frame(gfrac,gse,grade,grade_se)
  return(out)
}

#####################################
#REMOVE.DUPLICATES
#One of many things to deal with: students that take classes multiple times.
#This function keeps a specific duplicate or removes the ID altogether (default).
#You may keep NONE, the FIRST grade, or the LAST grade among the dupicates
#This assumes you have already selected a subject/catnum and returns indices of the records to KEEP.
remove.duplicates <- function(data,keep='NONE',verbose=FALSE)
{
  
  #Sort grades by ID, then TERM.
  data       <- data[order(data$ID,data$TERM), ]
  data$count <- sequence(rle(as.vector(data$ID))$lengths)
  
  ntot   <- length(data$ID)
  
  #Keep the first recorded grade of the duplicates
  if (keep == "FIRST")
  { 
    good <- which(data$count == 1)
    ngood <- length(good)
    if (verbose == TRUE)
    {
      print('keeping the first grade only')
      print(paste('kept ',ngood,' records of ',ntot,sep=""))
    }
  }    
  else
  {
    nid    <- length(data$ID[!duplicated(data$ID)])
    nstart <- which(data$count == 1)
    
    kdup   <- mat.or.vec(ntot,1)
    kfirst <- mat.or.vec(ntot,1)
    klast  <- mat.or.vec(ntot,1)
    
    for (i in 1:nid)
    {
      start_ind <- nstart[i]
      if (i < nid){stop_ind  <- nstart[i+1]-1}
      if (i == nid){stop_ind <- ntot}
      ind <- c(start_ind:stop_ind)
      kdup[ind] <- length(ind)
      kfirst[ind] <- start_ind
      klast[ind]  <- stop_ind
    }
    
    #Don't keep records for ANY duplicates.
    if (keep == "NONE") 
    {
      good  <- which(klast == kfirst)
      ngood <- length(good)
      if (verbose == TRUE)
      {
        print('discarding duplicate students')
        print(paste('kept ',ngood,' students of ',nid,sep=""))
      }
    }
    
    #Keep only the last grade among duplicates
    if (keep == "LAST") 
    {
      good <- which(data$count == kdup)
      ngood <- length(good)
      if (verbose == TRUE)
      {
        print('keeping the last grade only')
        print(paste('kept ',ngood,' records of ',ntot,sep=""))
      }   
    }
  }
  
  return(good)
  
}

#######################################
#LOCAL.SEQUENCE.MATCHES
#
#parse the student-record table for matching standard sequences
local.sequence.matches <- function(data)
{
  #First, find all courses in the table that are part of a sequence.
  course_grid <- make.course.grid()
  e           <- !is.na(course_grid[,2])
  course_grid <- course_grid[e,] #The course grid only for courses in sequence.
  
  #Now select those pairs that appear in the local table. 
  npairs <- dim(course_grid)[1]
  keep   <- mat.or.vec(npairs,1)
  for (i in 1:npairs)
  {
    e1 <- as.character(data$COURSE_CODE) == course_grid[i,1]
    e2 <- as.character(data$COURSE_CODE) == course_grid[i,2]
    if (sum(e1,na.rm=TRUE) > 0 & sum(e2,na.rm=TRUE) > 0)
    {
      keep[i] <- 1
    }
    
  }
  e <- keep == 1
  course_grid <- course_grid[e,]
  print(paste('found ',sum(e),' matching sequences',sep=""))
  return(course_grid)
}

#######################################
#MAKE.COURSE.GRID: 
#This is copied from the course grid spreadsheet.
#Column 1 is the first course, and column 2 is the second, or NA if it has no following course.
#
make.course.grid <- function()
{
   crse <- c('PHYS_I','PHYS_II',
            'PHYS_II',NA,	
            'PHYS_I_E','PHYS_II_E',
            'PHYS_II_E',NA, #'PHYS_II_S',
            'PHYS_I_S','PHYS_II_S',
            'PHYS_II_S',NA,	
            'CHEM_I','CHEM_II',
            'CHEM_I_LAB',NA,
            'CHEM_II',NA,
            'CHEM_II_LAB',NA,  
            'CHEM_III','CHEM_IV',
            'CHEM_IV',NA,
            'CHEM_IV_L',NA,	
            'CHEM_III_LAB',NA,	
            'CHEM_I_E',NA,
            'BIOL_I','BIOL_II',
            'BIOL_II',NA,	
            'BIOL_II_LAB',NA,	
            'BIOL_I_LAB',NA,
            'MATH_I','MATH_II',
            'MATH_II','MATH_IV',
            'MATH_IV','MATH_III',
            'MATH_III',NA,	
            'MATH_I_E','MATH_II_E',
            'MATH_II_E','MATH_IV_E',	
            'MATH_IV_E','MATH_III_E',
            'MATH_III_E',NA,
            'STAT_I',NA,	
            'STAT_II',NA,	
            'STAT_III',NA,
            'PSYC_I',NA,	
            'ECON_I','ECON_II',
            'ECON_II',NA,
            'SOCL_I',NA)
   
  #Now put this in a nicer format to view and handle
  carray   <- array(crse,dim=c(2,length(crse)/2))
  carray   <- t(carray)
  
}

discrete.grade.penalty <- function(data)
{
  low   <- c(-100,-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5)
  high  <- c(-1.5,-1.0,-0.5,0.0,0.5,1.0,1.5,100)
  nbins <- length(low)
  dgpen <- mat.or.vec(length(data$GRADE),1) 
  
  for (i in 1:nbins)
  {
    d1 <- which(data$GRADE >= low[i] & data$GRADE < high[i])
    dgpen[d1] <- high[i]
    if (high[i] > 5){dgpen[d1] <- high[i-1]+0.5}
  }
  return(dgpen)
}