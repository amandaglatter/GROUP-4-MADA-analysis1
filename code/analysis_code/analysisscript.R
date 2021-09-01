###############################
# analysis script
#
#this script loads the processed, cleaned data, does a simple analysis
#and saves the results to the results folder

#load needed packages. make sure they are installed.
library(ggplot2) #for plotting
library(broom) #for cleaning up output from lm()
library(here) #for data loading/saving
library(tidyverse)


#path to data
#note the use of the here() package and not absolute paths
data_location <- here::here("data","processed_data","processeddata.rds")

#load data. 
mydata <- readRDS(data_location)

######################################
#Data exploration/description
######################################
#I'm using basic R commands here.
#Lots of good packages exist to do more.
#For instance check out the tableone or skimr packages

#summarize data 
mysummary = summary(mydata)

#look at summary
print(mysummary)

#do the same, but with a bit of trickery to get things into the 
#shape of a data frame (for easier saving/showing in manuscript)
summary_df = data.frame(do.call(cbind, lapply(mydata, summary)))

#save data frame table to file for later use in manuscript
summarytable_file = here("results", "summarytable.rds")
saveRDS(summary_df, file = summarytable_file)

#boxplot
p <- mydata %>%
  ggplot(aes(x=CHD_status, y=Height)) + 
  geom_boxplot()
plot(p)

#make a scatterplot of data
#we also add a linear regression line to it
p1 <- mydata %>% ggplot(aes(x=Height, y=Weight)) + geom_point() + geom_smooth(method='lm')

p2 <- mydata %>% ggplot(aes(x=Weight, y=BMI)) + geom_point() + geom_smooth(method='lm')

#look at figure
plot(p1)
plot(p2)

#save figures
figure_file_boxlot = here("results","resultfigure_boxplot.png")
ggsave(filename = figure_file_boxlot, plot=p) 

figure_file_scatter1 = here("results","resultfigure_scatter1.png")
ggsave(filename = figure_file_scatter1, plot=p1) 

figure_file_scatter2 = here("results","resultfigure_scatter2.png")
ggsave(filename = figure_file_scatter2, plot=p2) 

######################################
#Data fitting/statistical analysis
######################################

# fit linear model
lmfit <- lm(Weight ~ Height, mydata)  

# place results from fit into a data frame with the tidy function
lmtable <- broom::tidy(lmfit)

#look at fit results
print(lmtable)

# save fit results table  
table_file = here("results", "resulttable.rds")
saveRDS(lmtable, file = table_file)

  