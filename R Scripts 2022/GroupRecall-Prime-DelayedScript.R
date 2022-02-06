library("scales")
library(stringr)
library("plyr")
library("reshape2")
library(dplyr)

setwd("F:/jaspe/Documents/Participant Data 2020-20210630T145418Z-001/Participant Data 2020/")
processed_list <- list.files('48 ms (short) duration/Updated Processing')
Plotting_data <- data.frame();

for (initial_file in processed_list){
  processed_sheet = read.csv(paste0('48 ms (short) duration/Updated Processing/', initial_file))
  #this is Day_One_Only
  counts2 <- table(processed_sheet$First_Prime_Condition[processed_sheet$Recalled_Immediate&!processed_sheet$delayed_recall])
  #this is the Both_Day [proportion]
  counts3 <- table(processed_sheet$First_Prime_Condition[processed_sheet$Recalled_Immediate&processed_sheet$delayed_recall])
  #removed because we obviously cannot get day one prime condition of words that were not recalled day one
  #counts4 <- table(processed_sheet$First_Prime_Condition[!(processed_sheet$Recalled_Immediate)&processed_sheet$delayed_recall])
  
  Day_One_OnlyIdent <- counts2[names(counts2)=="Identical"]
  Both_DayIdent <- counts3[names(counts3)=="Identical"]
  #Day_Two_OnlyIdent <- counts4[names(counts4)=="Identical"]
  Day_One_OnlyRel <- counts2[names(counts2)=="Related"]
  Both_DayRel <- counts3[names(counts3)=="Related"]
  #Day_Two_OnlyRel <- counts4[names(counts4)=="Related"]
  Day_One_OnlyUnrel <- counts2[names(counts2)=="Unrelated"]
  Both_DayUnrel <- counts3[names(counts3)=="Unrelated"]
  #Day_Two_OnlyUnrel <- counts4[names(counts4)=="Unrel"]
  Day_One_OnlyNop <- counts2[names(counts2)=="NoPrime"]
  Both_DayNop <- counts3[names(counts3)=="NoPrime"]
  #Day_Two_OnlyNop <- counts4[names(counts4)=="NoPrime"]
  
  if(length(Day_One_OnlyIdent)==0){
    Day_One_OnlyIdent = 0
  }
  if(length(Both_DayIdent)==0){
    Both_DayIdent = 0
  }
  #if(length(Day_Two_OnlyIdent)==0){
  #  Day_Two_OnlyIdent = 0
  #}
  
  if(length(Day_One_OnlyRel)==0){
    Day_One_OnlyRel = 0
  }
  if(length(Both_DayRel)==0){
    Both_DayRel = 0
  }
  #if(length(Day_Two_OnlyRel)==0){
  #  Day_Two_OnlyRel = 0
  #}
  
  if(length(Day_One_OnlyUnrel)==0){
    Day_One_OnlyUnrel = 0
  }
  if(length(Both_DayUnrel)==0){
    Both_DayUnrel = 0
  }
  #if(length(Day_Two_OnlyUnrel)==0){
  #  Day_Two_OnlyUnrel = 0
  #}
  
  if(length(Day_One_OnlyNop)==0){
    Day_One_OnlyNop = 0
  }
  if(length(Both_DayNop)==0){
    Both_DayNop = 0
  }
  #if(length(Day_Two_OnlyNop)==0){
  #  Day_Two_OnlyNop = 0
  #}
  
  curentCounts <- data.frame(Day_One_Only_Ident = Day_One_OnlyIdent[[1]], Both_Day_Ident = Both_DayIdent[[1]], Day_One_Only_Related = Day_One_OnlyRel[[1]], Both_Day_Related = Both_DayRel[[1]], Day_One_Only_Unrelated = Day_One_OnlyUnrel[[1]], Both_Day_Unrelated = Both_DayUnrel[[1]], Day_One_Only_NoPrime = Day_One_OnlyNop[[1]], Both_Day_NoPrime = Both_DayNop[[1]])
  Plotting_data <- rbind(Plotting_data, curentCounts)
  
}

means <- (colMeans(Plotting_data))
sumIdent <- sum(means[1:2])
sumRel <- sum(means[3:4])
sumUnrel <- sum(means[5:6])
sumNop <- sum(means[7:8])
percents <- c(means[1]/sumIdent, means[2]/sumIdent, means[3]/sumRel, means[4]/sumRel, means[5]/sumUnrel, means[6]/sumUnrel, means[7]/sumNop, means[8]/sumNop)
relPerc <- scales::percent(percents)  
library(ggplot2)

# create a dataset
Prime_Condition <- c(rep("Identical" , 2) , rep("Related" , 2), rep("Unrelated" , 2), rep("NoPrime" , 2) )
Recall_Condition <- rep(c("Only Recalled Day One" , "Recalled Both Days") , 4)
value <- c(means)
data <- data.frame(Prime_Condition,Recall_Condition,value)

# Stacked
ggplot(data, aes(fill=Recall_Condition, y=value, x=Prime_Condition, label = relPerc)) + 
  geom_bar(position="stack", stat="identity")+
  geom_text(size = 3, position = position_stack(vjust = 0.5))+
  ggtitle("48 ms (short)")

write.csv(Plotting_data, "48 ms (short) RecallData.csv")