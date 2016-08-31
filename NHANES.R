#NHANES 09, 11 - DEPRESSION datasets

rm(list=ls(all=T))
library('foreign')
library('weights')
library('plyr')
library('Hmisc')
library('psych')
library('stats')

# FUNCTIONS ---------------------------------------------------------------

Clean <- function(small.ID, big.ID, old.weight) {
  # Obtains the needed weight values from the original weights 
  # 
  # Args:
  #   small.ID: A vector containing the ID numbers for the respondents who 
  #     were assessed for depression in NHANES
  #   big.ID: A vector containing the ID numbers for all NHANES respondents
  #   old.weight: A vector containing the weights for all NHANES respondents   
  # 
  # Returns: 
  #   A vector containing only the weights for the depression module respondents
  
  new.weight <- vector()
  index <- 1
  i.1 <- 1
  i.2 <- 1
  limit <- length(big.ID)
  
  # Finds and obtains the needed weights
  while (i.2 <= limit) {
    if (is.na(small.ID[i.1] == big.ID[i.2]))
      i.2 <- i.2 + 1
    else if (small.ID[i.1] == big.ID[i.2]) {
      new.weight[index] <- old.weight[i.2]
      index <- index + 1
      i.1 <- i.1 + 1
      i.2 <- i.2 + 1
    } else 
      i.2 <- i.2 + 1
  }
  
  return(new.weight)
}

Rename <- function(dataframe, name.vector) {
  # Renames columns in depression dataframe to reflect SIGECAPS symptoms
  # 
  # Args:
  #   dataframe: The dataframe with study participants (NHANES)
  #   name.vector: A vector containing the new column names
  # 
  # Returns: 
  #   An updated dataframe 
  
  length <- ncol(dataframe)
  
  for(i in 1:length)
    colnames(dataframe)[i] <- name.vector[i]
  
  return(dataframe)
}

CalcPhq <- function(dataframe) {
  # Adds a col to the depression dataframe with the calculated PHQ scores for
  #   all respondents
  # 
  # Args:
  #   dataframe: The dataframe with study participants (NHANES)
  # 
  # Returns: 
  #   An updated dataframe with a new score col
  
  
  size <- ncol(dataframe)
  dummy.dataframe <- dataframe[2:size]
  
  length.dataframe <- ncol(dummy.dataframe)
  length.col <- length(dummy.dataframe[,1])
  
  score.vector <- vector()
  
  # Temporarily assings a value of 0 to missing entries
  for(i in 1:length.dataframe)
  {
    for(i.2 in 1:length.col)
    {
      if(is.na(dummy.dataframe[i.2,i]) == T)
        dummy.dataframe[i.2,i] <- 0
    }
  }
  
  # Calculates PHQ score
  for(i.3 in 1:length.col)
  {
    score.vector[i.3] <- sum(dummy.dataframe[i.3,])
  }
  
  # Adds score col
  dataframe$score <- score.vector
  
  return(dataframe)
}

CalcDepression <- function(dataframe, score.vector) {
  # Adds a col to the depression dataframe with a diagnosis of depression based
  #   on respondent's PHQ score
  # 
  # Args:
  #   dataframe: The dataframe with study participants (NHANES)
  # 
  # Returns: 
  #   An updated dataframe with a new diagnosis col
  
  diagnosis.vector <- vector()
  length <- length(score.vector)
  
  # Determines if respondent has depression or not (PHQ > 9)
  for(i in 1:length)
  {
    score <- score.vector[i]
    if(is.na(score))
      score <- 0
    
    if (score > 9)
      diagnosis.vector[i] <- TRUE
    else
      diagnosis.vector[i] <- FALSE
  }
  
  dataframe$diagnosis <- diagnosis.vector
  
  return(dataframe)
  
}

Fix <- function(dataframe, name.vector) {
  # Calls Rename, CalcPhq, and CalcDepression to modify the depression datasets
  # 
  # Args:
  #   dataframe: The dataframe with study participants (NHANES)
  #   name.vector: A vector of strings for the col names used in Rename
  # 
  # Returns: 
  #   An updated dataframe 
  
  dataframe <- Rename(dataframe, name.vector)
  dataframe <- CalcPhq(dataframe)
  dataframe <- CalcDepression(dataframe, dataframe$score)
  
  return(dataframe)
}

Weigh <- function(dataframe, vector, big.ID, original.weight) {
  # Calls Rename, CalcPhq, and CalcDepression to modify the depression datasets
  # 
  # Args:
  #   dataframe: The dataframe with study participants (NHANES)
  #   name.vector: A vector of strings for the col names used in Rename
  # 
  # Returns: 
  #   An updated dataframe 
  
  small.ID <- as.integer(dataframe$ID[!(is.na(vector))])
  
  new.weight <- Clean(small.ID, big.ID, original.weight)
}

AllWeigh <- function(dataframe1, ID.1, weight.1, dataframe2, ID.2, weight.2) {
  # Calls Weigh to get weighted prevalence estimates for all symptoms
  # 
  # Args:
  #   dataframe1: A dataframe with study participants for one year (NHANES)
  #   ID.1: A vector with case ID's for the corresponding dataframe
  #   weight.1: A vector with weights for all the respondents for the 
  #     corresponding dataframe
  #
  #   dataframe2: A dataframe with study participants for a different year
  #   ID.2: A vector with case ID's for the corresponding dataframe
  #   weight.2: A vector with weights for all the respondents for the 
  #     corresponding dataframe
  
  # Returns: 
  #   Weighted prevalence estimates for all symptoms
  
  depression <- rbind(dataframe1, dataframe2)
  old.weights <- append(weight.1, weight.2)
  all.ID <- append(ID.1, ID.2)
  new.weights <- vector()
  
  rows <- nrow(depression)
  index <- 1
  remove <- F
  
  for(i in 1:rows) {
    remove <- T
    
    for(i.2 in 2:10) {
      if( !(is.na(depression[i,i.2])) )
        remove <- F
    }
    
    if (remove == T)
      depression <- depression[-c(i),]
  }
  
  rows <- nrow(depression)
  
  new.weights <- Clean(depression[,1], all.ID, old.weights)
  
  for(i in 2:ncol(depression)) 
  {
    if (colnames(depression)[i] != "score") {
      category <- depression[,i]
      weight.col <- new.weights
      dummy.dataframe <- data.frame(category, weight.col)
      
      weight <- dummy.dataframe$weight.col[!(is.na(dummy.dataframe$category))]
      category <- depression[,i][!(is.na(depression[,i]))]
      
      print(colnames(depression)[i])
      print(wpct(category,weight,na.rm=T))
      cat("\n")
    }
  }
}

AllWeigh2 <- function(dataframe1, ID.1, weight.1, dataframe2, ID.2, weight.2) {
  # Calls Weigh to get weighted prevalence estimates for all symptoms. Better
  # version of AllWeigh b/c it removes NA rows 
  # 
  # Args:
  #   dataframe1: A dataframe with study participants for one year (NHANES)
  #   ID.1: A vector with case ID's for the corresponding dataframe
  #   weight.1: A vector with weights for all the respondents for the 
  #     corresponding dataframe
  #
  #   dataframe2: A dataframe with study participants for a different year
  #   ID.2: A vector with case ID's for the corresponding dataframe
  #   weight.2: A vector with weights for all the respondents for the 
  #     corresponding dataframe
  #
  # Returns: 
  #   Weighted prevalence estimates for all symptoms 
  
  depression <- rbind(dataframe1, dataframe2)
  old.weights <- append(weight.1, weight.2)
  all.ID <- append(ID.1, ID.2)
  new.weights <- vector()
  
  rows <- nrow(depression)
  index <- 1
  remove <- F
  
  for(i in 1:rows) {
    remove <- T
    
    for(i.2 in 2:10) {
      if( !(is.na(depression[i,i.2])) )
        remove <- F
    }
    
    if (remove == T)
      depression <- depression[-c(i),]
  }
  
  rows <- nrow(depression)
  
  new.weights <- Clean(depression[,1], all.ID, old.weights)
  
  for(i in 2:ncol(depression)) 
  {
    if (colnames(depression)[i] != "score") {
      category <- depression[,i]
      weight.col <- new.weights
      dummy.dataframe <- data.frame(category, weight.col)
      
      weight <- dummy.dataframe$weight.col[!(is.na(dummy.dataframe$category))]
      category <- depression[,i][!(is.na(depression[,i]))]
      
      print(colnames(depression)[i])
      print(wpct(category,weight,na.rm=T))
      cat("\n")
    }
  }
  
  score <- depression$score
  return(depression)
}


# CONSOLE -----------------------------------------------------------------

# Pull weights and depression numbers from the files
download(url = "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DEMO_F.XPT",
         destfile = "NHANES 09.XPT", mode = "wb")
NHANES.09 <- read.xport('NHANES 09.XPT')

download(url = "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DEMO_H.XPT",
         destfile = "NHANES 11.XPT", mode = "wb")
NHANES.11 <- read.xport("NHANES 11.XPT")

weight.09 <- NHANES.09$WTMEC2YR
weight.11 <- NHANES.11$WTMEC2YR

ID.09 <- as.integer(NHANES.09$SEQN)
ID.11 <- as.integer(NHANES.11$SEQN)

download(url = "https://wwwn.cdc.gov/Nchs/Nhanes/2009-2010/DPQ_F.XPT",
         destfile = "DEPRESSION 09.XPT", mode = "wb")
depression.09 <- read.xport("DEPRESSION 09.XPT")

download(url = "https://wwwn.cdc.gov/Nchs/Nhanes/2013-2014/DPQ_H.XPT",
         destfile = "DEPRESSION 11.XPT", mode = "wb")
depression.11 <- read.xport("DEPRESSION 11.XPT")

# New col names
names <- c("ID", "anhedonia", "dysphoria", "sleep", "energy", 
           "appetite", "guilt", "concentration", "psychomotor", 
           "suicide", "functioning")

# Updates the dataframes
depression.09 <- Fix(depression.09, names)
depression.11 <- Fix(depression.11, names)

# AllWeigh(depression.09, ID.09, weight.09, depression.11, ID.11, weight.11)

depression <- AllWeigh2(depression.09, ID.09, weight.09, 
                           depression.11, ID.11, weight.11)