library(tidyverse)

pal <- function(col, border = "light gray",...){
  
  n <- length(col)
  plot(0,0,type="n",xlim=c(0,1), ylim = c(0,1),
       axes = F, xlab = "",ylab="",...)
  rect(0,0:(n-1)/n,1,1:n/n,col = rev(col),border = border)
  text(.5, ((0:(n-.01)/n)+(1:n/n))/2,labels=rev(col))
  
}

list(
zg_colors = c("#222831","#393e46","#d65A31","#EEEEEE","#FFCF99","#FFCF99"),

corp_colors = c("#88BdBc","#254e58","#112d32","#4f4a41","#6E6658"),

clean_mod = c("#17252A","#2B7A78","#3AAFa9","#DEF2f1","#FEFFFF"),

nature = c("#687864","#31708E","#5085A5","#8FC1E3","#F7F9FB"),

peaceful_mod = c("#844D36","#474853","#86B3D1","#AAA0A0","#8E8268"),

corp_serious = c("#265077","#022140","#494B68","#1E4258","#2D5F5D")
) %>% 
  map(pal)

