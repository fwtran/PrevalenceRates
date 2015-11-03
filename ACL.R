# Americans' Changing Lives Wave 4 (2002) 

rm(list=ls(all=T))
library('foreign')
library('weights')
library('plyr')
library('descr')
library('Hmisc')

# H1A, B, C, D, E, F, G, H, K, M, N
# H1A - "I felt depressed" V1002
# H1B - "I felt that everything I did was an effort" V1003
# H1C - "My sleep was restless" V1004
# H1D - "I was happy" V1005
# H1E - "I felt lonely" V1006
# H1F - "People were unfriendly" V1007
# H1G - "I enjoyed life" V1008
# H1H - "I did not feel like eating. My appetite was poor" V1009
# H1K - "I felt sad" V1011
# H1M - "I felt that people disliked me" V1012
# H1N - "I could not get 'going.'" V1013

# Score of 2 or 3 on at least 6 items is considered a diagnosis of depression
# For most of the items, the more frequently reported each item, the higher
#   the score. For 2 items, the LESS frequently reported each item, the higher 
#   the score.

# http://www.icpsr.umich.edu/icpsrweb/NACDA/studies/4690

setwd("//HP9500-2/My Documents/FELIX Tran/Comfort_Level/MentalHealth")

acl <- read.spss("04690-0001-Data.sav", to.data.frame=T)



CodeUp <- function (response) {
  if( is.na(response) || response == "HARDEVER")
    return(FALSE)
  else
    return(TRUE)
}

CodeDown <- function(response) {
  if(is.na(response) || response == "MOSTTIME")
    return(FALSE)
  else
    return(TRUE)
}

diagnosis <- vector()
diagnosis.index <- 1

rows <- nrow(acl)
col.start <- which(colnames(acl) == "V1002")
col.end <- which(colnames(acl) == "V1013")

for(row.index in 1:rows) {
  answer.vector <- c(NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA)
  answer.vector.index <- 1
  
  for(col.index in col.start:col.end) {
    if(col.index == 373)
      next
    else if(col.index == 368 || col.index == 371) {
      answer.vector[answer.vector.index] <- CodeDown(acl[row.index, col.index])
      answer.vector.index <- answer.vector.index + 1
    }
    else {
      answer.vector[answer.vector.index] <- CodeUp(acl[row.index, col.index])
      answer.vector.index <- answer.vector.index + 1
    }
  }
  
  if(length(which(answer.vector==TRUE)) >= 6)
    diagnosis[diagnosis.index] <- TRUE
  else
    diagnosis[diagnosis.index] <- FALSE
  
  diagnosis.index <- diagnosis.index + 1
}

positive <- length(which(diagnosis==T))
positive / nrow(acl)