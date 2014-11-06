concat = function(listtoconcatenate)
{
  out= ""
  for(i in 1:length(listtoconcatenate))
  {
    out = paste(out, listtoconcatenate[i], sep = "")
    #print(listtoconcatenate[i])
  }
  return (out)
}
?curveGrob
#INSTALL PACKAGES AND LIBRARIES:
mypackages = c("data.table", "epicalc", "stats","xtable", "odfWeave", "stringr", "gtools", "Hmisc", "psych", "DBI", "RSQLite", "car",  "gdata", "gmodels", "plyr", "ggplot2") #"Rcmdr"
for(i in 1:length(mypackages)) 
 {
 if (! mypackages[[i]] %in% rownames(installed.packages())) install.packages(mypackages[[i]])
 }
 
for(i in mypackages) 
{
  library(i, character.only = TRUE)
}

