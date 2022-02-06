studied_words = read.csv("CW_studiedwords.csv", header = TRUE, sep = ",")
subjectfile = read.csv("CW_final.csv", header = TRUE, sep = ",")
recogfile = read.csv("CW_recog.csv", header = TRUE, sep = ",")
delayedfile = read.csv("CW_delayed_final.csv", header = TRUE, sep = ",")

recogfile$Studied = NA
recogfile$StudyPosition = NA
recogfile$Block = NA
recogfile$PrimeCondition = NA
recogfile$RelatedPrime = NA
recogfile$Day1Position = NA
recogfile$Day2Position = NA


for (i in 1:nrow(recogfile)){
  ## if the recogword is in the studied_words list
  ## then Studied is True and we record the study and block position
  
  if(as.character(recogfile[i,1]) %in% studied_words$RecalledWord){
    recogfile[i,10] = "Studied"
    position = which(studied_words$RecalledWord %in% recogfile[i,1])
    recogfile[i,11] = studied_words[position,2]
    recogfile[i,12] = studied_words[position,3]
  }
  
  ## if recogword is a prime (Unrelated or Identity)
  ## then we record the prime condition
  
  if(as.character(recogfile[i,1]) %in% subjectfile$Prime){
    primeposition = which(subjectfile$Prime %in% recogfile[i,1])
    recogfile[i,13] = subjectfile[primeposition,10]
  }
  
  ## it is also possible that the recogword is studied and RELATED primed
  ## if so, we change PrimeCondition to Related
  ## and store the RelatedPrime
  
  if(as.character(recogfile[i,1]) %in% subjectfile$PrimeFor){
    primeforposition = which(subjectfile$PrimeFor %in% recogfile[i,1])
    recogfile[i,13] = "Related"
    recogfile[i,14] = subjectfile[primeforposition,9]
    
  }
  
  ## also want to know if the recogword was recalled on day1/day2
  ## and at which position
  
  ## if recalled on day1, we store its output position from day 1
  ## if it is recalled more than once, we store all the positions
  
  if(as.character(recogfile[i,1]) %in% subjectfile$RecalledWord){
    day1position = which(subjectfile$RecalledWord %in% recogfile[i,1])
    recogfile[i,15] = paste( unlist(subjectfile[c(day1position),12]), collapse=',')
    
  }
  
  ## if recalled on day2, we store its output position from day 2
  
  if(as.character(recogfile[i,1]) %in% delayedfile$DelayedWord){
    day2position = which(delayedfile$DelayedWord %in% recogfile[i,1])
    recogfile[i,16] = paste(unlist(day2position), collapse = ",")
    
  }
  
  
}

write.csv(recogfile, file = "CW_recog_final.csv", row.names = FALSE )
