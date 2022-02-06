setwd("F:/jaspe/Documents/Participant Data 2020-20210630T145418Z-001/Participant Data 2020")
processed_sheet = read.csv("64 ms (medium) duration/Updated Processing/AC_Processed.csv")
library(ggplot2)
library(dplyr)

counts <- table(processed_sheet$Recog_Condition)
barplot(counts, main="Recognition Condition Distribution",
        xlab="Recognition Condition")

#this is total
counts2 <- table(processed_sheet$First_Prime_Condition[processed_sheet$Recalled_Immediate])
barplot(counts2)

#this is the correct [proportion]
counts3 <- table(processed_sheet$First_Prime_Condition[processed_sheet$Recalled_Immediate&processed_sheet$delayed_recall])
barplot(counts3)


counts4 <- table(processed_sheet$Recalled_Immediate[processed_sheet$delayed_recall])
barplot(counts4)

counts5 <- table(processed_sheet$First_Prime_Condition[processed_sheet$Recalled_Immediate&!processed_sheet$delayed_recall])
barplot(counts5)


ggplot(subset(processed_sheet, !is.na(First_Prime_Condition)) %>% count(First_Prime_Condition, delayed_recall) %>%    
         mutate(pct=n/sum(n),               # Calculate percent within each region
                ypos = cumsum(n) - 0.5*n),  # Calculate label positions
       aes(First_Prime_Condition, n, fill=delayed_recall)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=paste0(sprintf("%1.1f", pct*100),"%")), 
            position=position_stack(vjust=0.5)) +
  labs(x = "Initial Prime Condition", y = "Count", title = "Proportional Recall in Delayed Experiment by Prime Condition") +
  guides(fill=guide_legend(title="Recalled Delayed Portion"))



#recog
counts6 <- table(processed_sheet$Recog_Decision[processed_sheet$Recalled_Immediate],exclude = c("", NA))
barplot(counts6)
counts7 <- table(processed_sheet$Recog_Decision[!processed_sheet$Recalled_Immediate],exclude = c("", NA))
barplot(counts7)
counts8 <- table(processed_sheet$First_Prime_Condition[processed_sheet$Recog_Decision == "z"])
barplot(counts8)
counts9 <- table(processed_sheet$First_Prime_Condition[processed_sheet$Recog_Decision == "slash"])
barplot(counts9)
counts10 <- table(processed_sheet$Recog_Decision[processed_sheet$Recalled_Immediate&processed_sheet$First_Prime_Condition=="Identical"],exclude = c("", NA))
barplot(counts10)
counts11 <- table(processed_sheet$Recog_Decision, processed_sheet$First_Prime_Condition,exclude = c("", NA))
ll <- as.array(counts11)