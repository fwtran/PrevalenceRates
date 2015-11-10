#National Comorbidity Survey replication (NCS-R) (2001-2003)

# rm(list=ls(all=T))
library('sas7bdat')
library('foreign')
library('weights')
library('plyr')
library('descr')
library('Hmisc')
library('psych')
library('GPArotation')

setwd("C:/Users/Felix/Desktop/Recent/Project/NCS-R")


# Part A - Symptoms
# Part B - Duration
# Part C - Impairment
# Part D - Medical condition
# Part E - Bereavement


# TO DO -------------------------------------------------------------------

# Merge Symptoms and Diagnosis2 so that a positive case will be screened 
#   and added to a dataset to be returned

# FACTOR ANALYSIS ---------------------------------------------------------

# Helper functions #
Symptoms <- function(dataframe, case.num) {
  # Keeps track of which symptoms respondent have or does not have
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   A dataframe with 1/0 values indicating if respondent has certain symptoms

  symptom.vector <- vector()
  
  if(Dys(dataframe, case.num))
    symptom.vector[1] <- 1
  else
    symptom.vector[1] <- 0
  
  if(Anh(dataframe, case.num))
    symptom.vector[2] <- 1
  else
    symptom.vector[2] <- 0
  
  if(Sleep(dataframe, case.num))
    symptom.vector[3] <- 1
  else
    symptom.vector[3] <- 0
  
  if(Guilt(dataframe, case.num))
    symptom.vector[4] <- 1
  else
    symptom.vector[4] <- 0
  
  if(Energy(dataframe, case.num))
    symptom.vector[5] <- 1
  else
    symptom.vector[5] <- 0
  
  if(Concen(dataframe, case.num))
    symptom.vector[6] <- 1
  else
    symptom.vector[6] <- 0
  
  if(App(dataframe, case.num))
    symptom.vector[7] <- 1
  else
    symptom.vector[7] <- 0
  
  if(Psych(dataframe, case.num))
    symptom.vector[8] <- 1
  else
    symptom.vector[8] <- 0
  
  if(Suicide(dataframe, case.num))
    symptom.vector[9] <- 1
  else
    symptom.vector[9] <- 0
    
  return(symptom.vector)
}

Diagnose2 <- function(dataframe) {
  # Diagnoses study participants with major depressive episode or not 
  # based on DSM-IV criteria as specified in documentation specifically for
  # FactorAnalysis
  # 
  # Args:
  #   dataframe: The dataframe with study participants (NCS-R)
  # 
  # Returns: 
  #   Dataframe with only study participants diagnosed with MDE

  symptom.vector <- vector()
  dataframe2 <- data.frame()
  dataframe2 <- setNames(as.data.frame(matrix(nrow=1055, ncol = 9)), 
                         c('Dysphoria','Anhedonia','Sleep','Guilt','Energy',
                           'Concentration','Appetite','Psychomotor','Suicide'))
  index <- 1
  
  # Checks if case meets all the requirments 
  for(case.num in 1:nrow(dataframe)) {
    if(Sigecaps(dataframe, case.num) & Duration(dataframe, case.num) & 
         Impairment(dataframe, case.num) & Confound(dataframe, case.num) &
         Bereavement(dataframe, case.num)) {
      symptom.vector <- Symptoms(dataframe, case.num)
      dataframe2[index,] <- symptom.vector
      index <- index + 1
    }  
  }
  
  return(dataframe2)
}

# Factor Analysis
FactorAnalysis <- function(dataframe) {
  # Conducts a factor analysis of the SIGECAPS symptoms using only respondents
  # diagnosed with major depressive episode
  # 
  # Args:
  #   dataframe: The dataframe with study participants (NCS-R)
  # 
  # Returns: 
  #   A factor analysis of the depression symptoms
  
  dataframe2 <- Diagnose2(dataframe)

  print(str(dataframe2))

  print(fa(dataframe2[1:9], nfactors=3))
}

FactorAnalysis(ncs)

# DIAGNOSE ----------------------------------------------------------------
Diagnose <- function(dataframe) {
  # Diagnoses study participants with major depressive episode or not 
  # based on DSM-IV criteria as specified in documentation. 
  # 
  # Args:
  #   dataframe: The dataframe with study participants (NCS-R)
  # 
  # Returns: 
  #   Weighted prevalence of MDD in NCS sample
  
  cases <- vector()

  # Checks if case meets all the requirments 
  for(case.num in 1:nrow(dataframe)) {
    if(Sigecaps(dataframe,case.num) & Duration(dataframe,case.num) & 
         Impairment(dataframe,case.num) & Confound(dataframe,case.num) &
         Bereavement(dataframe,case.num))
      cases[case.num] <- T
    
    else
      cases[case.num] <- F    
  }
  
  # Weighs diagnoses
  print(wpct(cases,dataframe$NCSRWTSH))
  print(wtd.table(cases,dataframe$NCSRWTSH))
}

# PART A -- >= 5 OF THE FOLLOWING SYMPTOMS (1 MUST BE DysPHORIA OR AnhEDONIA)

# Helper function #
FixBoolean <- function(case) {
  # Changes survey respones("YES/"NO"/NA) with T/F
  # 
  # Args:
  #   case: A binary string response ("YES"/"NO") for a survey question
  # 
  # Returns: 
  #   T/F
  
  new.value <- (case == "YES")
  if(is.na(new.value))
    new.value <- F
  return(new.value)
}

# Diagnosis #
Sigecaps <- function(dataframe, case.num) {
  # Determines if subject has >= 5 of the SIGECAP symptoms
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   T/F
  
  #Subject must have dysphoria or anhedonia to meet DSM-IV criteria
  if(!Dys(dataframe,case.num) & !Anh(dataframe,case.num))
    return(F)
  
  #Subject must have at least 5 symptoms to meet DSM-IV criteria
  count <- 0
  
  if(Dys(dataframe,case.num))
    count <- count + 1
  
  if(Anh(dataframe,case.num))
    count <- count + 1
  
  if(Sleep(dataframe,case.num))
    count <- count + 1
  
  if(Guilt(dataframe,case.num))
    count <- count + 1
  
  if(Energy(dataframe,case.num))
    count <- count + 1
  
  if(Concen(dataframe,case.num))
    count <- count + 1
  
  if(App(dataframe,case.num))
    count <- count + 1
  
  if(Psych(dataframe,case.num))
    count <- count + 1
  
  if(Suicide(dataframe,case.num))
    count <- count + 1
  
  return(count >= 5)
}

# Symptoms #
Dys <- function(dataframe, case.num) {
  # Determines if respondent has Dysphoria 
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   T/F
  
  # 1 of 4 questions screening for dysphoria must be T
  for(i in 1:4) {
    index <- 160 + i
    symptom <- FixBoolean(dataframe[case.num,index])

    if(symptom)
      return(T)
  }
  
  return(F)
}

Anh <- function(dataframe, case.num) {
  # Determines if respondent has Anhedonia 
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   T/F
  
  # 1 of 2 questions screening for anhedonia must be T
  for(i in 1:2) {
    index <- 164 + i
    symptom <- FixBoolean(dataframe[case.num,index])
    
    if(symptom)
      return(T)
  }
  
  return(F)
}

Sleep <- function(dataframe, case.num) {
  # Determines if respondent has sleep disturbances
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   T/F
  
  #1 of 2 questions screening for sleep disturbance must be T
  for(i in 1:2) {
    index <- 171 + i
    symptom <- FixBoolean(dataframe[case.num,index])
    
    if(symptom)
      return(T)
  }
  
  return(F)
}

Guilt <- function(dataframe, case.num) {
  # Determines if respondent has excessive guilt
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   T/F
  
  # Question screening for excessive guilt must be T
  symptom <- FixBoolean(dataframe$D26V[case.num])
  
  if(!symptom)
    return(F)
  else
    return(T)
}

Energy <- function(dataframe, case.num) {
  # Determines if respondent has low energy
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   T/F
  
  # Question screening for low energy must be T
  symptom <- FixBoolean(dataframe$D26J[case.num])
  
  if(!symptom)
    return(F)
  else
    return(T)
}

Concen <- function(dataframe, case.num) {
  # Determines if respondent has concentration problems
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   T/F
  
  # 1 of 3 questions screening for concentration problems must be T
  for(i in 1:2) {
    index <- 182 + i
    symptom <- FixBoolean(dataframe[case.num,index])
    
    if(symptom)
      return(T)
  }
  
  symptom <- FixBoolean(dataframe$D26P[case.num])
  
  if(!symptom)
    return(F)
  else
    return(T) 
}

App <- function(dataframe, case.num) {
  # Determines if respondent has had excessive weight gain or loss
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   T/F
  
  # Respondent must have had gained or lost at least 10 lbs.
  dummy.vector <- vector()
  
  # weight loss 
  dummy.vector[1] <- FixBoolean(dataframe$D26A[case.num])  # T/F
  dummy.vector[2] <- dataframe$D26F[case.num]  # Amount gained
  
  # Weight gain
  dummy.vector[3] <- FixBoolean(dataframe$D26B[case.num])  # T/F
  dummy.vector[4] <- dataframe$D26D[case.num]  # Amount lost
  
  index <- 1
  
  # Checks if subjected gained or lost 10 lbs.
  while(index < 5) {
    symptom <- dummy.vector[index]
    
    if ((index %% 2) == 1) {
      if(!symptom)
        index <- index + 2
      else
        index <- index + 1
    } else {
      if(is.na(symptom) | symptom < 10)
        index <- index + 1
      else
        return(T)
    }
  }
  
  return(F)
}

Psych <- function(dataframe, case.num) {
  # Determines if respondent has psychomotor issues
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   T/F
  
  # 1 of 2 questions screening for psychomotor issues must be T
  dummy.vector <- vector()
  
  dummy.vector[1] <- FixBoolean(dataframe$D26M[case.num])
  dummy.vector[2] <- FixBoolean(dataframe$D26O[case.num])
  
  for(i in 1:2) {
    symptom <- dummy.vector[i]

    if(symptom)
      return(T)
  }
  
  return(F)
}

Suicide <- function(dataframe, case.num) {
  # Determines if respondent has suicidal thoughts/ideation or 
  # has attempted Suicide
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   T/F
  
  # 1 of 5 questions screening for suicidal ideation or attempt must be T
  for(i in 1:5) {
    index <- 191 + i
    symptom <- FixBoolean(dataframe[case.num,index])
    
    if(symptom)
      return(T)
  }
  
  return(F)
}


# PART B -- SYMPTOMS PERSIST FOR AT LEAST 2 WEEKS -------------------------
# Helper function #
FixUnitsDuration <- function(time.unit) {
  # Converts unit of time into an integer value for the duration function
  # 
  # Args:
  #   time.unit - A string indiciating unit of time (i.e. "days"/"weeks"/etc.)
  # 
  # Returns: 
  #   new.value - An integer value corresponding to the unit of time
  
  # 1 day has a value of 1
  if(is.na(time.unit))
    new.value <- 0
  else if(time.unit == "DAYS")
    new.value <- 1
  else if(time.unit == "WEEKS")
    new.value <- 7
  else if(time.unit == "MONTHS")
    new.value <- 30
  else
    new.value <- 365
  
  return(new.value)
}

# Duration #
Duration <- function(dataframe, case.num) {
  # Determines if respondent has had symptoms for at least 2 consecutive weeks
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   T/F
  
  length.vector <- vector()
  unit.vector <- vector()
  
  # 1 of 3 questions screening for duration of worst/most recent/ep. with comorbid
  #   disease "sad ep." must be at least 2 weeks long.
  # Function converts all durations into days. 1 question must be at least 14 days long
  
  #Numerical duration
  length.vector[1] <- dataframe$D22B[case.num]
  length.vector[2] <- dataframe$D22D[case.num]
  length.vector[3] <- dataframe$D39[case.num]
  
  #Unit of duration (i.e. days, weeks, etc.)
  unit.vector[1] <- FixUnitsDuration(dataframe$D22B1[case.num])
  unit.vector[2] <- FixUnitsDuration(dataframe$D22D1[case.num])
  unit.vector[3] <- FixUnitsDuration(dataframe$D39A[case.num])
  
  score.vector <- vector()
  
  #Compiles durations of worst/most recent/ep. with comorbid disease "sad ep."
  for(i in 1:3) {
    if(is.na(length.vector[i]))
      score.vector[i] <- 0
    else
      score.vector[i] <- length.vector[i] * unit.vector[i]
  }
  
  #Gets longest duration
  largest.score.index <- which(rank(score.vector,ties.method="first") == 
                                 length(score.vector))
  
  #At least 2 weeks == at least 14 days
  if(score.vector[largest.score.index] >= 14)
    return(T)
  else
    return(F)
  
}


# PART C - CLINICALLY SIGNIFICANT DISTRESS/IMPAIRMENT ---------------------
# Helper functions #
FixRankingImpairment <- function(ranking) { 
  # Converts ranking into an integer value for the impairment function
  # 
  # Args:
  #   ranking: A string denoting a response to a question with ranked options
  #     (i.e. "NEVER"/"RARELY"/"SOMETIMES"/etc.)
  # 
  # Returns: 
  #   new.value: An integer corresponding to the ranking
  
  
  if(is.na(ranking))
    new.value <- 0
  else if(ranking == "MODERATE" | ranking == "RARELY" | 
            ranking == "SOME")
    new.value <- 4
  else if(ranking == "SEVERE" | ranking == "SOMETIMES" | 
            ranking == "A LOT")
    new.value <- 5
  else if(ranking == "VERY SEVERE" | ranking == "OFTEN" |
            ranking == "EXTREMELY")
    new.value <- 6
  else
    new.value <- 3
  
  return(new.value)
}

FixRankingImpairmentSpecial <- function(ranking) {
  # Converts ranking into an integer value for the impairment function
  #   because this is survey is confusing af and has different thresholds
  #   for different questions
  # 
  # Args:
  #   ranking: A string denoting a response to a question with ranked options
  #     (i.e. "NEVER"/"RARELY"/"SOMETIMES"/etc.)
  # 
  # Returns: 
  #   new.value: An integer corresponding to the ranking
  
  if(is.na(ranking))
    new.value <- 0
  else if(ranking == "SOMETIMES")
    new.value <- 5
  else if(ranking == "OFTEN")
    new.value <- 6
  else
    new.value <- 3
  
  return(new.value)
}

# Impairment #
Impairment <- function(dataframe, case.num) {
  # Determines if respondent's symptoms have caused clinically significant 
  #   distress/impairment
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   T/F
  
  # 1 of 10 questions screening for significant distress/impairment 
  #   must reach certain threshold
  
  # Clinically significant distress
  for(i in 1:3) {
    index <- 148 + i
    
    if(index == 150)
      impair.answer <- FixRankingImpairmentSpecial(dataframe[case.num, index])
    else
      impair.answer <- FixRankingImpairment(dataframe[case.num, index])

    if(impair.answer >= 4)
      return(T)
  }
  
  # Clinically significant impairment in occupational/Psychosocial functioning
  for(i in 1:2) {
    index <- 200 + i
    
    impair.answer <- FixRankingImpairment(dataframe[case.num, index])
    
    if(impair.answer >= 4)
      return(T)
  }
  
  # Subjective rating of severity of impairment due to "sad ep." (?)
  for(i in 1:4) {
    index <- 241 + i
    
    impair.answer <- dataframe[case.num, index]
    
    if(is.na(impair.answer))
      next
    else if(impair.answer >= 4)
      return(T)
  }
  
  # Clinically significant distress
  impair.answer <- FixBoolean(ncs$D24B[case.num])
  
  if(impair.answer)
    return(T)
  else
    return(F)
}


# PART D - NOT BECAUSE OF OTHER MEDICAL CONDITION -------------------------
Confound <- function(dataframe, case.num) {
  # Determines if respondent's symptoms were due to a medical condition
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   T/F
  
  confound.answer <- FixBoolean(dataframe$D29A[case.num])
  
  if(confound.answer) 
    return(F)
  else
    return(T)
}


# PART E - NOT BECAUSE OF BEREAVEMENT -------------------------------------
# Helper functions #
FixRankingBereavement <- function(ranking) {
  # Determines if response meets threshold for symptoms not being caused 
  #   by bereavement
  # 
  # Args:
  #   ranking: A string denoting a response to a question with ranked options
  #     (i.e. "NEVER"/"RARELY"/"SOMETIMES"/etc.)
  # 
  # Returns: 
  #   T/F
  
  if(is.na(ranking))
    return(F)
  else if(ranking == "OFTEN" | ranking == "A LOT" | 
            ranking == "EXTREMELY" | ranking == "SOMETIMES")
    return(T)
  else
    return(F) 
}

FixRankingBereavementSpecial <- function(ranking) {
  # Determines if response meets threshold for symptoms not being caused 
  #   by bereavement because this survey is confusing af and has different
  #   thresholds for different questions
  # 
  # Args:
  #   ranking: A string denoting a response to a question with ranked options
  #     (i.e. "NEVER"/"RARELY"/"SOMETIMES"/etc.)
  # 
  # Returns: 
  #   T/F
  
  if(is.na(ranking) | ranking != "OFTEN")
    return(F)
  else
    return(T) 
}

# Bereavement #
Bereavement <- function(dataframe, case.num) {
  # Determines if respondent's symptoms cannot be explained solely
  #   by bereavement alone
  # 
  # Args:
  #   dataframe: A dataframe with subject respondents (NCS-R)
  #   case.num: An integer indicating the row number for one respondent
  # 
  # Returns: 
  #   T/F
  
  
  # Symptoms must last for at least 2 months to still qualify for diagnosis
  #   All durations are converted to days
  #   See Part B duration function for more info
  unit.vector <- vector()
  length.vector <- vector()
  
  length.vector[1] <- dataframe$D22B[case.num]
  length.vector[2] <- dataframe$D22D[case.num]
  
  unit.vector[1] <- FixUnitsDuration(dataframe$D22B1[case.num])
  unit.vector[2] <- FixUnitsDuration(dataframe$D22D1[case.num])
  
  score.vector <- vector()
  
  for(i in 1:2) {
    score.vector[i] <- length.vector[i] * unit.vector[i]
  }
  
  largest.score <- which((rank(score.vector)) == length(score.vector))
  
  # At least 2 months == 60 days
  if(largest.score >= 60)
    return(T)
  
  # Presence of particular symptoms #
  # Marked functional impairment
  bereave.answer <- FixRankingBereavementSpecial(dataframe$D19[case.num])
  if(bereave.answer)
    return(T)
  
  for(i in 1:2) {
    index <- 200 + i
    bereave.answer <- FixRankingBereavement(dataframe[case.num,index])
    
    if(bereave.answer)
      return(T)
  }
  
  for(i in 1:4) {
    index <- 241 + 1
    bereave.answer <- dataframe[case.num,index]
    
    if(is.na(bereave.answer))
      next
    else if(bereave.answer >= 7)
      return(T)
  }
  
  bereave.answer <- dataframe$D68[case.num] 
  
  if(!is.na(bereave.answer) & bereave.answer >= 10)
    return(T)
  
  # Morbid preoccupation with worthlessness / Suicidal ideation / 
  #   Psychomotor retardation
  bereave.vector <- c(dataframe$D26V[case.num], dataframe$D26CC[case.num],
                      dataframe$D26DD[case.num], dataframe$D26EE[case.num],
                      dataframe$D26L[case.num], dataframe$D26M[case.num])
  
  for(i in 1:length(bereave.vector)) {
    bereave.answer <- FixBoolean(bereave.vector[i])
    
    if(bereave.answer)
      return(T)
  }

  return(F)
}


# CONSOLE -----------------------------------------------------------------

ncs <- read.spss("NCS-Data.sav", to.data.frame=T)
# FactorAnalysis(ncs)

Diagnose(ncs)
