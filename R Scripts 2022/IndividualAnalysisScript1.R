library("plyr")
library("reshape2")
library(dplyr)

setwd("F:/jaspe/Documents/Participant Data 2020-20210630T145418Z-001/Participant Data 2020")

full_word_list = read.csv("FINAL-fullwordlist.csv")
raw_immediate = read.csv("64 ms (medium) duration/Raw Data (Immediate and Delayed)/AF_M2.2ConstantTotalFinal_2020-10-29_16h40.16.972.csv")
delayed = read.csv("64 ms (medium) duration/Raw Data (Immediate and Delayed)/AF_DelayedExp_2020-10-31_13h56.09.597.csv")
full_word_list <- full_word_list %>% 
  rename(
    related_word = Prime,
  )

initial <- melt(full_word_list, id.vars = c("Item.Number", "FSG", "LgSUBTLEX","BSG"),value.name = "Words")
initial <- mutate_all(initial, .funs=toupper)
initial <- as.data.frame(apply(initial,              # Remove blanks
                               2,
                               function(x) gsub("\\s+", "", x)))

initial$Item.Number <- NULL
initial$FSG <- NULL
initial$LgSUBTLEX <- NULL
initial$BSG <- NULL
initial$variable <- NULL

#add participant and experiment name data
initial$Participant <- raw_immediate[1,1]
initial$expName <- raw_immediate[1,"expName"]


#collapse all the rows in the immediate data and add related words
raw_immediate$row_num <- seq.int(nrow(raw_immediate))
raw_immediate <- raw_immediate %>% transmute(raw_immediate, cap = pmax(captured_string, captured_string_block2, 
                                                                       captured_string_block3, captured_string_block4))
raw_immediate <- raw_immediate %>% transmute(raw_immediate, react = pmax(Text4_RT, text32_RT, text47_RT, text60_RT, na.rm = TRUE))
raw_immediate <- raw_immediate %>% transmute(raw_immediate, prime = pmax(selectedPrimeblock1, 
                                                                         selectedPrimeblock2, selectedPrimeblock3, selectedPrimeblock4))
raw_immediate <- raw_immediate %>% transmute(raw_immediate, Primed_Immidiate_Presented_Condition = pmax(PrimeConditionblock1, 
                                                                                                        primeConditionblock2, primeConditionblock3, primeConditionblock4))
raw_immediate$adjusted_prime <- c("", head(raw_immediate['prime'], dim(raw_immediate)[1] - 1)[[1]])
raw_immediate$adjusted_prime_condition <- c("", head(raw_immediate['Primed_Immidiate_Presented_Condition'], dim(raw_immediate)[1] - 1)[[1]])
raw_immediate <- merge(raw_immediate, full_word_list, by.x = "cap", by.y = "Target", all.x = TRUE)

#need to check if backupword exists
if(!"BackupWord" %in% colnames(raw_immediate)){
  raw_immediate <- raw_immediate %>% transmute(raw_immediate, BackupWord = WordItem) 
}

#clean strings
raw_immediate <- raw_immediate %>% mutate(adjusted_prime = gsub('[[:digit:]]+', "", adjusted_prime))
raw_immediate <- raw_immediate %>% mutate(prime = gsub('[[:digit:]]+', "", prime))

raw_immediate <- raw_immediate %>% mutate(cap = gsub('[[:digit:]]+', "", cap))
raw_immediate <- raw_immediate %>% mutate(cap = gsub(' ', "", cap))
raw_immediate <- raw_immediate %>% mutate(BackupWord = gsub(' ', "", BackupWord))



#get actual prime conditions
raw_immediate <- raw_immediate %>%
  mutate(prime_con_actual = if_else((adjusted_prime == "" ),"NoPrime", 
                                    if_else(cap == adjusted_prime, "Identical", 
                                            if_else(adjusted_prime == related_word,"Related","Unrelated"))))

raw_immediate$adjusted_prime_condition[raw_immediate$adjusted_prime_condition == ""] <- "NoPrime"
raw_immediate$adjusted_prime[raw_immediate$adjusted_prime == ""] <- "NoPrime"

#ad blocks and trial numbers to raw data to eliminate unrelated tails
raw_immediate$recall_block <- ceiling(raw_immediate$row_num/104)
raw_immediate <- raw_immediate %>%
  mutate(recall_position = if_else(recall_block == 1, row_num-62, if_else(recall_block == 2, row_num - 146, 
                                                                          if_else(recall_block == 3, row_num - 230, row_num - 314))))


#remove rows without primes left from raw data
raw_immediate <-raw_immediate[order(raw_immediate$row_num),]
list_used <- list()
cumul_list_used <- list ()
current_block <- 0
current_length <- 0


for (i in 1:nrow(raw_immediate)){
  if(!is.na(raw_immediate$cap[i]) && (raw_immediate$cap[i] != "")){
    
    if(current_block != raw_immediate$recall_block[i]){
      current_block <- raw_immediate$recall_block[i]
      current_length <- 0
    }
    
    if(current_length == 42){
      raw_immediate$cap[i] <- ""
      raw_immediate$adjusted_prime_condition[i] <- ""
      raw_immediate$adjusted_prime[i] <- ""
      raw_immediate$prime[i-1] <- ""
      
    }
    else{
      if((raw_immediate$adjusted_prime_condition[i] == "Identical" || raw_immediate$adjusted_prime_condition[i] == "Related") && !(raw_immediate$adjusted_prime[i] %in% list_used)){
        list_used <- append(list_used, raw_immediate$adjusted_prime[i])
        current_length <- current_length + 1
      }
      if(!(raw_immediate$cap[i] %in% list_used) && (raw_immediate$cap[i] %in% raw_immediate$BackupWord) && !(raw_immediate$related_word[i] %in% list_used)){
        list_used <- append(list_used, raw_immediate$cap[i])
        current_length <- current_length + 1
      }
    }
    
    
  }
  
  if(i>1){
    if(raw_immediate$recall_position[i-1] == 42){
      raw_immediate$cap[i] <- ""
      raw_immediate$adjusted_prime_condition[i] <- ""
      raw_immediate$adjusted_prime[i] <- ""
      raw_immediate$prime[i-1] <- ""
    }
  }
  
  #(length(list_used)>=42 && i< 200){
  # print(initial$recall_block[i])
  #print(initial$recall_position[i])
  #}
}

#add studied words and their positions
initial$Studied_Immediate <- initial$Words %in% raw_immediate$BackupWord
initial <- merge(initial, raw_immediate[,c("BackupWord", "row_num")], by.x = "Words", by.y = "BackupWord", all.x = TRUE)
initial$block <- ceiling(initial$row_num/90)
#remove two so that the first index is 1 and not 0
initial <- initial %>%
  mutate(position = if_else(block == 1, row_num-20, 
                            if_else(block == 2, row_num - 104,if_else(block == 3, row_num - 188, row_num - 272))))
initial$row_num <- NULL

#add recalled words and their positions
initial$Recalled_Immediate <- initial$Words %in% raw_immediate$cap
initial <- merge(initial, raw_immediate[,c("cap", "row_num", "adjusted_prime", "adjusted_prime_condition", "prime_con_actual", "react")], by.x = "Words", 
                 by.y = "cap", all.x = TRUE)
initial$recall_block <- ceiling(initial$row_num/104)
initial <- initial %>%
  mutate(recall_position = if_else(recall_block == 1, row_num-62, if_else(recall_block == 2, row_num - 146, 
                                                                          if_else(recall_block == 3, row_num - 230, row_num - 314))))
initial$row_num <- NULL

#add relaxed prime
initial$identical_anywhere <- initial$Words %in% raw_immediate$adjusted_prime

initial <- merge(initial, full_word_list[,c("Target", "related_word")], by.x = "Words", by.y = "Target", all.x = TRUE)
initial$related_anywhere <- initial$related_word %in% raw_immediate$adjusted_prime


#add primed words and their positions, note this uses prime and not adjust prime because I did it out of order, don't change
initial$Primed_immediate <- initial$Words %in% raw_immediate$prime
initial <- merge(initial, raw_immediate[,c("prime", "row_num")], by.x = "Words", by.y = "prime", all.x = TRUE)
initial <- merge(initial, raw_immediate[,c("prime", "Primed_Immidiate_Presented_Condition")], by.x = "Words", by.y = "prime", all.x = TRUE)
initial$prime_block <- ceiling(initial$row_num/104)
initial <- initial %>%
  mutate(prime_position = if_else(prime_block == 1, row_num-61, if_else(prime_block == 2, row_num - 145, 
                                                                        if_else(prime_block == 3, row_num - 229, row_num - 313))))

initial$row_num <- NULL


#remove out of bound primes at position 43
which_overbound <- which(initial$prime_position==43)
initial[which_overbound, "Primed_immediate"]<-FALSE
initial[which_overbound, c("Primed_Immidiate_Presented_Condition", "prime_block", "prime_position")]<-NA

#create counts of correct primes before smooshing to keep data simple
b<-table(initial$adjusted_prime_condition[initial$Studied_Immediate & initial$Recalled_Immediate])


initial <-initial[order(initial$recall_position),]
initial <-initial[order(initial$recall_block),]




#smoosh if recalled twice initially
initial <- initial %>% group_by(Words) %>% 
  mutate(recall_position = paste(recall_position, collapse=",")) %>%
  mutate(recall_block = paste(recall_block, collapse=",")) %>%
  mutate(react = paste(react, collapse=",")) %>%
  mutate(adjusted_prime = paste(adjusted_prime, collapse=",")) %>%
  mutate(adjusted_prime_condition = paste(adjusted_prime_condition, collapse=",")) %>%
  mutate(prime_con_actual = paste(prime_con_actual, collapse=","))
initial <- unique(initial)

#delayed
initial$delayed_recall <- initial$Words %in% delayed$captured_string
initial <- merge(initial, delayed[,c("captured_string", "trials.thisRepN", "Text20_RT")], 
                 by.x = "Words", by.y = "captured_string", all.x = TRUE)
initial$trials.thisRepN <- initial$trials.thisRepN + 1
initial <- merge(initial, delayed[,c("RecogWord", "key_resp_3.keys", "key_resp_3.rt", "trialsRecognition.thisTrialN")], 
                 by.x = "Words", by.y = "RecogWord", all.x = TRUE)
#modify position and reaction time
initial$trialsRecognition.thisTrialN <- initial$trialsRecognition.thisTrialN + 1
initial$key_resp_3.rt <- initial$key_resp_3.rt * 1000

#smoosh rows if recalled twice delayed
initial <- initial %>% group_by(Words) %>% 
  mutate(trials.thisRepN = paste(trials.thisRepN, collapse=",")) %>%
  mutate(Text20_RT = paste(Text20_RT, collapse=","))
initial <- unique(initial)

##adding recog prime column
initial <- initial %>%
  mutate(recog_condition = if_else(key_resp_3.keys == 'slash',
                                   if_else(Studied_Immediate == TRUE,
                                           if_else(Primed_immediate == TRUE,'TP','TPS'),
                                           if_else(Primed_immediate == TRUE,'TPP','FP')),
                                   if_else(Studied_Immediate == TRUE,
                                           if_else(Primed_immediate == TRUE,'FN','FNS'),
                                           if_else(Primed_immediate == TRUE,'FNP','TN'))))





#smoosh rows if recog conditions are different
initial <- initial %>% group_by(Words) %>% 
  mutate(recog_condition = paste(recog_condition, collapse=",")) 
initial <- unique(initial)



#change column names
colnames(initial)[which(names(initial) == "block")] <- "Studied_Block"
colnames(initial)[which(names(initial) == "position")] <- "Studied_Position"
colnames(initial)[which(names(initial) == "adjusted_prime")] <- "Prime_Word"
colnames(initial)[which(names(initial) == "recall_block")] <- "Recalled_Immediate_Block"
colnames(initial)[which(names(initial) == "recall_position")] <- "Recalled_Immediate_Position"
colnames(initial)[which(names(initial) == "react")] <- "Recalled_Immediate_RT"
colnames(initial)[which(names(initial) == "prime_con_actual")] <- "Prime_Condition"
colnames(initial)[which(names(initial) == "trials.thisRepN")] <- "Recalled_Delayed_Position"
colnames(initial)[which(names(initial) == "Text20_RT")] <- "Recalled_Delayed_RT"
colnames(initial)[which(names(initial) == "key_resp_3.keys")] <- "Recog_Decision"
colnames(initial)[which(names(initial) == "key_resp_3.rt")] <- "Recog_RT"
colnames(initial)[which(names(initial) == "trialsRecognition.thisTrialN")] <- "Recog_Position"
colnames(initial)[which(names(initial) == "recog_condition")] <- "Recog_Condition"

#add first recalled words
initial['First_Prime_Word'] <- NA
initial['First_Prime_Condition'] <- NA
initial['First_Recall_RT'] <- NA
initial['First_Recall_Block'] <- NA
initial['First_Recall_Position'] <- NA


for (i in 1:nrow(initial)){
  PrimeWordAsList <- as.list(strsplit(initial$Prime_Word[i], ",")[[1]])
  innerLength = length(PrimeWordAsList)
  
  bestPrimeWord = initial$Prime_Word[i]
  bestPrimeCondition = initial$adjusted_prime_condition[i]
  bestRecallRT = initial$Recalled_Immediate_RT[i]
  bestRecallBlock = initial$Recalled_Immediate_Block[i]
  bestRecallPosition = initial$Recalled_Immediate_Position[i]
  
  if(innerLength>1){
    bestPosition = 600
    PrimeConditionAsList <- as.list(strsplit(initial$adjusted_prime_condition[i], ",")[[1]])
    RecalledImmediateRTAsList <- as.list(strsplit(initial$Recalled_Immediate_RT[i], ",")[[1]])
    RecalledImmediateBlockAsList <- as.list(strsplit(initial$Recalled_Immediate_Block[i], ",")[[1]])
    RecalledImmediatePositionAsList <- as.list(strsplit(initial$Recalled_Immediate_Position[i], ",")[[1]])
    for (j in 1:innerLength){
      comparePosition = strtoi(RecalledImmediateBlockAsList[j]) * 100 + strtoi(RecalledImmediatePositionAsList[j])
      if(comparePosition<bestPosition){
        bestPosition = comparePosition;
        
        bestPrimeWord = PrimeWordAsList[j]
        bestPrimeCondition = PrimeConditionAsList[j]
        bestRecallRT = RecalledImmediateRTAsList[j]
        bestRecallBlock = RecalledImmediateBlockAsList[j]
        bestRecallPosition = RecalledImmediatePositionAsList[j]
        
      }
    }
    
  }
  initial$First_Prime_Word[i] = unlist(bestPrimeWord)
  initial$First_Prime_Condition[i] = unlist(bestPrimeCondition)
  initial$First_Recall_RT[i] = unlist(bestRecallRT)
  initial$First_Recall_Block[i] = as.integer(unlist(bestRecallBlock))
  initial$First_Recall_Position[i] = as.integer(unlist(bestRecallPosition))
}



write.csv(initial,paste0(file_path_output, initials, "_Processed.csv"), row.names = FALSE)
a<-table(initial$Primed_Immidiate_Presented_Condition)
#b<-table(initial$adjusted_prime_condition[initial$Studied_Immediate & initial$Recalled_Immediate])    moved up for simplicity

correct_recog_count <- (delayed$captured_string_Thresh == delayed$selectedPrimeThresh & delayed$captured_string_Thresh != "")
all_recog_count <- ( delayed$selectedPrimeThresh != "")
datavals <- data.frame(Total_NoPrime = 4, Total_Identical = a[[1]], Total_Related = a[[2]], Total_Unrelated = a[[3]], Correct_NoPrime = b[[1]], Correct_Identical = b[[2]], Correct_Related = b[[3]], Correct_Unrelated = b[[4]], Proportion_NoPrime = b[[1]]/4, Proportion_Identical = b[[2]]/a[[1]], Proportion_Related = b[[3]]/a[[2]], Proportion_Unrelated = b[[4]]/a[[3]], Proportion_Recognized_Threshold = sum(correct_recog_count == TRUE)/sum(all_recog_count == TRUE))

totMean <- mean(as.numeric(initial$First_Recall_RT), na.rm = TRUE)
stdDev <- sd(as.numeric(initial$First_Recall_RT), na.rm = TRUE)



initial <- initial %>%
  mutate(trialZ = (as.numeric(First_Recall_RT) - totMean)/stdDev, na.rm=TRUE)


MNP <- mean(as.numeric(initial$First_Recall_RT[initial$First_Prime_Condition=='NoPrime']))
MI <- mean(as.numeric(initial$First_Recall_RT[initial$First_Prime_Condition=='Identical']))
MR <- mean(as.numeric(initial$First_Recall_RT[initial$First_Prime_Condition=='Related']))
MU <- mean(as.numeric(initial$First_Recall_RT[initial$First_Prime_Condition=='Unrelated']))
MZ <- mean(initial$trialZ , na.rm=TRUE)
MZNP <- mean(initial$trialZ[initial$First_Prime_Condition=='NoPrime'])
MZI <- mean(initial$trialZ[initial$First_Prime_Condition=='Identical'])
MZR <- mean(initial$trialZ[initial$First_Prime_Condition=='Related'])
MZU <- mean(initial$trialZ[initial$First_Prime_Condition=='Unrelated'])
RTvals <- data.frame(MeanRT_NoPrime = MNP, MeanRT_Identical = MI, MeanRT_Related = MR, MeanRT_Unrelated = MU, MeanRT_Total = totMean, StDevRT_Total = stdDev, MeanZ = MZ, MeanZ_NP = MZNP, MeanZ_Identical = MZI, MeanZ_Related = MZR , MeanZ_Unrelated = MZU)

