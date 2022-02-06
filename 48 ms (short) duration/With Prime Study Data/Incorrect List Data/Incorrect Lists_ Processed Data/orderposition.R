## primed recall experiment: summer 2020 scripts

studied_words = read.csv("test_studiedwords.csv", header = TRUE, sep = ",")
subjectfile = read.csv("test_initial.csv", header = TRUE, sep = ",")

library(dplyr)
merged_file = left_join(subjectfile, studied_words, by = c("RecalledWord", "Block"))

merged_file$Studied = ifelse(is.na(merged_file$StudyPosition), NA, "Studied")
merged_file$PrimeRecalledCondition = NA

# also adding info about whether the word was studied, a prime or random

## primes can be "identical", "related" or "unrelated"
## how do we know which of these it is?
## we need a way of tracking the "related" primes -- i.e., if a "related" prime showed up, then
## then we need to find the word it was the prime for

fullwordlist = read.csv("fullwordlist_old.csv", header = TRUE, sep = ",")

## indices of rows in fullwordlist where the RelatedPrime is in the mergedfile list
fullwordindices = which(fullwordlist$Prime %in%  merged_file$Prime )
## we extract the "target" and "prime" for these indices
target_indices = fullwordlist[fullwordindices, c("Target", "Prime")]
## we merge this with the merged_file
colnames(target_indices) = c("PrimeFor", "Prime")
merged_file = left_join(merged_file, target_indices)

prime_list = merged_file$Prime
primefor_list = merged_file$PrimeFor

## now we know which word it was the prime for when the prime was related

## next we want to know if the word recalled by participant was random, a prime, or studied
merged_file$PrimeRecalledCondition = NA
for(i in 1:nrow(merged_file)){
  if(as.character(merged_file[i,7]) %in% prime_list){
    ## if recalledword is in the prime list
    ## then RecalledPrime contains the Prime Condition
    
    ## first we find the position of the match
    position = which(prime_list %in% merged_file[i,7])
    merged_file[i,16] = merged_file[position,10]
    
  }
}


write.csv(merged_file, file = "test_final.csv", row.names = FALSE)
