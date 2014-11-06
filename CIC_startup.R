GetSTEM = function(data1)
{
  #Use CIP codes:
  #Based on this list: https://www.ice.gov/doclib/sevis/pdf/stem-list.pdf
  
  stem_cip_ = read.csv("STEM_CIP.csv")
  stem_cip = c(as.character(stem_cip_$stem_cip))
  stem_cip_ = NULL
  
  #See if the degrees are stem majors:

  data1$MAJOR1_STEM = with(data1, MAJOR1 %in% as.character(stem_cip))
  data1$MAJOR1_STEM = ifelse(is.na(data1$MAJOR1), NA, data1$MAJOR1_STEM)
  table(data1$MAJOR1_STEM, useNA= "always")
  
  data1$MAJOR2_STEM = with(data1, MAJOR2 %in% as.character(stem_cip))
  data1$MAJOR2_STEM = ifelse(is.na(data1$MAJOR2), NA, data1$MAJOR2_STEM)

  data1$FIRST_DECLARE_STEM = with(data1, FIRST_DECLARE %in% as.character(stem_cip))
  data1$FIRST_DECLARE_STEM = ifelse(is.na(data1$FIRST_DECLARE), NA, data1$FIRST_DECLARE_STEM)
  table(data1$FIRST_DECLARE_STEM, useNA = "always")
  return(data1)
}

AddCOURSE_CODE = function(sc, current_school)
{
  #CREATE THE NEW VARIABLE "COURSE_CODE" IF IT DOESN'T EXIST
  #The file course_grid.csv needs to be in the current directory:
  cg = read.csv("course_grid.csv")
  
  #Get the list of valid courses for the current school (current_school)
  course_list = as.character(cg[[toupper(current_school)]])
  valid_courses = nchar(course_list) == 7
  course_list = course_list[valid_courses== TRUE]
  #Check:
  course_list 
  
  #Get the corresponding Short Code
  short_codes = as.character(cg$SHORT_CODE)
  short_codes = short_codes[valid_courses== TRUE]
  short_codes
  
  #Add COURSE_CODE Variable
  for (i in 1:length(short_codes))
    sc[sc$CATALOGNBR == course_list[i], "COURSE_CODE"] = short_codes[i]  
  sc[,c("CATALOGNBR", "COURSE_CODE")]
  
  #Check
  table(sc$COURSE_CODE)
  return(sc)
}

sc_required = c("ID","SUBJECT","CATALOGNBR","COURSE_CODE","GRADE","GPAO","CUM_GPA","TOTALCREDITS",
                "TOTALGRADEPTS","COURSECREDIT","TERM")
sc_datatypes = c("F", "N", "F", "F", "N", "N", "N", "N", "N", "N", "F")
sr_required=c("ID","GENDER","ETHNICITY","FIRST_TERM","DEGREE_TERM","TRANSFER","MAJOR1","MAJOR1_LONG","MAJOR2","MAJOR2_LONG", "FIRST_DECLARE","FIRST_DECLARE_LONG","FIRST_DECLARE_TERM")

#CHECK MISSING IN SR AND SC ----
check_missing_sc = function(sc)
{
  if (is.null(names(sc)) )
    return ("All variables are present")
  
  missing = sc_required[which (!sc_required  %in% names(sc))]
  return(paste (c("MISSING: " , missing)))
}

check_missing_sr = function(sr)
{
  
  if (is.null(names(sr)) )
    return ("All variables are present")

  missing = sr_required[which (!sr_required  %in% names(sr))]
  return(paste (c("MISSING: " , missing)))
}
