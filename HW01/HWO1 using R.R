# HW01 using R

# set width of output
options("width"=120, "digits"=8)

#####  Check to see if packages are downloaded, install if not, then load  #####

library(readxl) # To import xls or xlsx data as table
library(expss) # To add variable and value labels, sort
library(psych) # To add descriptive summary functions
library(multcomp) # To do glht linear combinations in GLMs
library(TeachingDemos) # To create text output files
library(tidyverse)
library(dplyr)

###################################################################################

# Define variables for working directory and data name -- CHANGE THESE
filesave = "C:/Users/Lenovo/OneDrive - University of Iowa/PhD portfolio/2. Courses/Spring 2022/PSQF62700001 Generalized Linear Models (Lesa Hoffman)/4. Homeworks/HW01/"
filename = "HW01_01445225.xlsx"
setwd(dir=filesave)

# Import homework data and add labels
HW01 = read_excel(paste0(filesave,filename)) 
# Convert to data frame to use in analysis
HW01 = as.data.frame(HW01)

# Here are the variable labels (not to be included in data used in lm)
#PersonID=   "PersonID: Person ID number"
#group=      "group: 1=control, 2=mini-golf, 3=regular-golf, 4=both-golf"
#experience= "experience: Previous mini-golf experience (1-7 scale)"
#enthusiasm= "enthusiasm: Previous mini-golf enthusiasm (M~0, SD~1)"
#score=      "score: #Strokes on 18-Hole Mini-Golf Test (higher is worse score)"

# This is where you will write code to center/recode predictors
HW01 = HW01%>%
  mutate(exp_m = experience-4,
         g2 = ifelse(group == 2, 1, 0),
         g3 = ifelse(group == 3, 1, 0),
         g4 = ifelse(group == 4, 1, 0))
attach(HW01)

# Open output text file -- run this last if you want to save output
txtStart(file=paste0(filesave,"PSQF6270_HW01_Output.txt"))

# Your models go here
m1 = lm(score~1)
summary(m1)

m2 = lm(score ~ 1 + enthusiasm + exp_m + g2 + g3 + g4)
summary(m2)

m3 = lm(score ~ 1 + enthusiasm + exp_m + g2 + g3 + g4 + exp_m:g2 + exp_m:g3 + exp_m:g4)
summary(m3)

summary(glht(model=m3, linfct=rbind(
  "Exp Slope at g = 2" = c(0,0,1,0,0,0,1,0,0),
  'Exp Slope at g = 3' = c(0,0,1,0,0,0,0,1,0),
  'Exp Slope at g = 4' = c(0,0,1,0,0,0,0,0,1))),test=adjusted("none"))

summary(glht(model=m3, linfct=rbind(
  "Ctrl Slope vs g = 2 at exp = 4" = c(0,0,0,1,0,0,0,0,0),
  'Ctrl Slope vs g = 3 at exp = 4' = c(0,0,0,0,1,0,0,0,0))),test=adjusted("none"))

summary(glht(model=m3, linfct=rbind(
  "g = 2 Slope vs g = 4 at exp = 4" = c(0,0,0,-1,0,1,0,0,0),
  'g = 3 Slope vs g = 4 at exp = 4' = c(0,0,0,0,-1,1,0,0,0))),test=adjusted("none"))

summary(glht(model=m3, linfct=rbind(
  "Interac exp ctrl and g = 2" = c(0,0,0,0,0,0,1,0,0),
  "Interac exp ctrl and g = 3" = c(0,0,0,0,0,0,0,1,0),
  'Interac exp g = 2 and g = 4' = c(0,0,0,0,0,0,-1,0,1),
  "Interac exp g = 3 and g = 4" = c(0,0,0,0,0,0,0,-1,1))),test=adjusted("none"))

# Omnibus tests
groupF = glht(model=m2, linfct=c('enthusiasm=0', 'exp_m=0', "g2=0","g3=0", 'g4=0'))
summary(groupF, test=Ftest())

dummyF = glht(model=m2, linfct=c("g2=0","g3=0", 'g4=0'))
summary(dummyF, test=Ftest())

dummyF2 = glht(model=m3, linfct=c('exp_m:g2=0', 'exp_m:g3=0', 'exp_m:g4=0'))
summary(dummyF2, test=Ftest())

# Close output text file
txtStop()

