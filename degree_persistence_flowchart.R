MakeCurve = function(name, size, color, curvature, direction)
{
  x1 = 0
  x2 = 0
  y1 = 1
  y2 = 1
  if (direction == "down")
  {
    x1 = 1
    x2 = 0
    y1 = 0
    y2 = 1
  }
  t=curveGrob(x1, x2, y1, y2, default.units = "npc",
              curvature = curvature, angle = 60, ncp = 10, shape = 1,
              square = FALSE, squareShape = 1,
              inflect = FALSE, arrow = NULL, open = TRUE,
              debug = FALSE,
              name = name, gp=gpar(lwd = size,col=color), vp = NULL) 
  return(t)
}
dp_flow_chart = function (tt, out_type = "percents", title = NULL, center_title = NULL, print_center = "None")
{
  
library(ggplot2)
library(gmodels)
library(grid)
library(grDevices)
print (out_type)

#Raw counts for each cell in the tt table.  Order how we want to print them
#order across rows
cnts =  c(tt$t[1],tt$t[3],tt$t[5],tt$t[2],tt$t[4], tt$t[6])

center_labels =  c(tt$t[1],tt$t[3],tt$t[5],
                           tt$t[2],tt$t[4],tt$t[6])

#Column Totals
c_cnts = NULL
c_cnts[1] = sum(c(tt$t[1], tt$t[2] ))
c_cnts[2] = sum(c(tt$t[3], tt$t[4] ))
c_cnts[3] = sum(c(tt$t[5], tt$t[6] ))
c_lab = c("STEM", "NONSTEM", "NO DEGREE")

if (out_type == "percents") {
  c_cnts = c_cnts/sum(c_cnts)
}
c_cc = paste(c_lab, paste("\n",round(c_cnts,digits = 2)))
#c_cc

#Row Totals
r_cnts = NULL
r_cnts[1] = sum(c(tt$t[1], tt$t[3], tt$t[5]) )
r_cnts[2] = sum(c(tt$t[2], tt$t[4], tt$t[6]) )
r_lab = c("STEM", "NONSTEM")

if (out_type == "percents") {
  r_cnts = r_cnts/sum(r_cnts)
}
r_cc = paste(r_lab, paste("\n",round(r_cnts,digits = 2)))
#r_cc
size = c(c_cnts[1]/sum(c_cnts), c_cnts[2]/sum(c_cnts), c_cnts[3]/sum(c_cnts), 
         r_cnts[1]/sum(r_cnts), r_cnts[2]/sum(r_cnts))

counts = cnts/sum(cnts)*100


df = data.frame(c(0,5),c(0,3))
names(df) = c ("X","Y")

#size  = c(.25, .25, .25, .25, .25)


g = ggplot(data=df, aes(x=X, y=Y, size = 1)) +
  annotation_custom(MakeCurve("1-1", counts[1], "green", -.3, "up"),    0.5, 4.5,  2.0, 2.5) +
  annotation_custom(MakeCurve("1-2", counts[2], "yellow", .3, "down"),     0.5, 4.5,  2.0, 1.5) +
  annotation_custom(MakeCurve("1-3", counts[3], "red", .3, "down"),     0.5, 4.5,  2.0, .5) +
  annotation_custom(MakeCurve("2-1", counts[4], "green", .3, "up"),    0.8,4.5,  1.0, 2.5) + 
  annotation_custom(MakeCurve("2-2", counts[5], "yellow", .3, "up"),    0.5,4.5,  1.0, 1.5) +
  annotation_custom(MakeCurve("2-3", counts[6], "red", -.3, "down"),    0.5,4.5,   1.0, .5) +
  annotate("rect", xmin = 0, xmax = 1, ymin = 1 - size[5], ymax = 1 + size[5],
             alpha = 1, fill = "white") +
  annotate("rect", xmin = 0, xmax = 1, ymin = 2 - size[4], ymax = 2 + size[4],
           alpha = 1, fill = "gray") +
  annotate("rect", xmin = 4, xmax = 5, ymin = .5 - size[3], ymax = .5 + size[3],
           alpha = 1, fill = "red") +
  annotate("rect", xmin = 4, xmax = 5, ymin = 1.5 - size[2], ymax = 1.5 + size[2],
           alpha = 1, fill = "yellow") +
  annotate("rect", xmin = 4, xmax = 5, ymin = 2.5 - size[1], ymax = 2.5 + size[1],
           alpha = 1, fill = "green") +
  annotate("text", x = .5, y = 1, label = r_cc[2]) +  
  annotate("text", x = .5, y = 2, label = r_cc[1]) +  
  annotate("text", x = 4.5, y = 2.5, label = c_cc[1]) +  
  annotate("text", x = 4.5, y = 1.5,   label = c_cc[2]) +  
  annotate("text", x = 4.5, y = .5, label = c_cc[3]) 
g = g + ggtitle(title) 
g = g + theme(plot.title = element_text(size = 24))
g = g + theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
g = g + annotate("text", x = .5, y = 3.0, label = "Initial Major", size = 8) 
g = g + annotate("text", x = 4.5, y = 3.0, label = "Degree", size =8)
#g = g + annotate("text", x = 2.5, y = 3.0, label = center_title, size =8)

lab_x_centers = c(2.5, 2.4, 2.5,2.5, 2.5, 2.5)
lab_y_centers = rev(c(.5, 1, 1.25, 1.70, 2, 2.5))


if (out_type == "percents") {
center_labels =  round(center_labels/sum(cnts), digits = 2)
}


#AddCenterNotations
  for (i in 1:6)
  {
    g = g + annotate("rect", xmin = lab_x_centers[i]-.2, xmax = lab_x_centers[i]+.2, 
                     ymin = lab_y_centers[i]-.1, ymax = lab_y_centers[i]+.1,
                     alpha = 1, fill = "white") +
      annotate("text", x = lab_x_centers[i], y = lab_y_centers[i], label = center_labels[i])     
    
  }    

print(g)
} # function

