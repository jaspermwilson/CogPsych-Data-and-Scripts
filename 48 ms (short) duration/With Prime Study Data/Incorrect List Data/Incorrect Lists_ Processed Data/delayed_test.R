studied_words = read.csv("CN_studiedwords.csv", header = TRUE, sep = ",")
subjectfile = read.csv("CN_final.csv", header = TRUE, sep = ",")
delayedfile = read.csv("CN_delayedrecall.csv", header = TRUE, sep = ",")

library(dplyr)

# omit all XXXXX
delayedfile_subset = delayedfile

delayedfile_subset$Studied = NA
delayedfile_subset$StudyPosition = NA
delayedfile_subset$Block = NA
delayedfile_subset$PrimeCondition = NA
delayedfile_subset$Day1OutputPosition = NA
delayedfile_subset$PrimedRelated = NA



for (i in 1:nrow(delayedfile_subset)){
  ## if the delayed recalled word is in the studied_words
  ## then "Studied" is true, and we get position and block
  
  if(as.character(delayedfile_subset[i,1]) %in% studied_words$RecalledWord){
    delayedfile_subset[i,3] = "Studied"
    position = which(studied_words$RecalledWord %in% delayedfile_subset[i,1])
    delayedfile_subset[i,4] = studied_words[position,2]
    delayedfile_subset[i,5] = studied_words[position,3]
  }
  
  ## if the delayed recalled word is in the prime list
  ## then PrimeRecalled contains the condition of that prime
  ## this will tell us if the participant reported any of the primes they saw on day 1
  ## identical/unrelated/related
  
  if(as.character(delayedfile_subset[i,1]) %in% subjectfile$Prime){
    primeposition = which(subjectfile$Prime %in% delayedfile_subset[i,1])
    delayedfile_subset[i,6] = (subjectfile[primeposition, 10])
  }
  
  ## now we want to check if this delayed recalled word was recalled on day 1
  ## if so, we want to know the output position in which it was recalled
  
  if(as.character(delayedfile_subset[i,1]) %in% subjectfile$RecalledWord){
    if(as.character(delayedfile_subset[i,1])!=""){
      wordposition = which(subjectfile$RecalledWord %in% delayedfile_subset[i,1])
      delayedfile_subset[i,7] = paste( unlist(subjectfile[c(wordposition),12]), collapse=',')
      
    }
  }
  
  ## finally, we can also check if any of the delayed recalled words
  ## had been "primed" by a related prime, i.e., maybe the word
  ## recalled was studied AND semntically primed
  ## if so, we store its related prime 
  ## and change prime condition to "Related"
  
  if(as.character(delayedfile_subset[i,1]) %in% subjectfile$PrimeFor){
    primeforposition = which(subjectfile$PrimeFor %in% delayedfile_subset[i,1])
    delayedfile_subset[i,8] =  subjectfile[primeforposition, 9]
    delayedfile_subset[i,6] = "Related"
  }
}

# merge this file with the full file

## if there are XXXXXs in the delayed file use this:

delayedfile_final = left_join(delayedfile, delayedfile_subset)

## if there are no XXXXXs in the delayed file use this:

#delayedfile_final = delayedfile_subset

write.csv(delayedfile_final, file = "CN_delayed_final.csv", row.names = FALSE )
