#Project Name: Studing the Dementia Caregiver Sleep
#File Name: Main program
#Author: Reza Sadeghi
#Email: sadeghi.2@wright.edu; reza@knoesis.org
#Ver:1.2
#Last modification:6/15/2018

#Project Name: Studing the Dementia Caregiver Sleep
#File Name: Catch Data From E4 for caregiver sleep study
#Author: Reza Sadeghi
#Email: sadeghi.2@wright.edu; reza@knoesis.org
#Ver:1.2
#Last modification:07/01/2021 by Charan Pinninty (pinninty.2@wright.edu)

memory.limit(size = 16000)


person=c(1:18)
week=c(1:2)
day=c(1:9)
signal=list('ACC','BVP','EDA','HR','IBI','Temp')
data=list(person,week,day,signal)


data <- vector(mode = "list", length = 8)
for (p_i in 1:18){
  data[[p_i]] <- vector(mode="list", length = 2)
  for (w_i in 1:2){
    data[[p_i]][[w_i]] <- vector(mode="list", length = 7)
    for (d_i in 1:9){
      data[[p_i]][[w_i]][[d_i]] <- structure(vector(mode="list", length = 6), names= signal)
    }
  }
}

root="C:\\Sleep-quality-in-caregivers-master\\Examine_your_sleep_with_our_model\\Data\\";
for (p_i in 1:18){
  for (w_i in 1:2){
    for (d_i in 1:9){
      possibleError <- tryCatch({
        direction=paste(root,'Participant ',as.character(p_i),'\\Week ', as.character(w_i),'\\',as.character(d_i), sep = '')
        setwd(direction)
        Flage<<-TRUE
      },error= function(e){
        print("There is no such directory")
        print(direction)
        Flage<<-FALSE
      })
      if(inherits(possibleError, "error")) next
      #setwd("C:\\Users\\Reza Sadeghi\\Desktop\\Dementia Caregiver Sleep Dataset\\Dataset\\Participant 1\\Week 1\\1")
      if(!Flage)
      {
        next
      }
      print(direction)
      ACC <- read.csv("ACC.csv",header = F,as.is = T)
      data [[p_i]][[w_i]][[d_i]]$ACC <- ACC
      
      BVP <- read.csv("BVP.csv",header = F,as.is = T)
      data [[p_i]][[w_i]][[d_i]]$BVP <- BVP
      
      EDA <- read.csv("EDA.csv",header = F,as.is = T)
      data [[p_i]][[w_i]][[d_i]]$EDA <- EDA
      
      HR <- read.csv("HR.csv",header = F,as.is = T)
      data [[p_i]][[w_i]][[d_i]]$HR <- HR
      
      IBI <- read.csv("IBI.csv",header = F,as.is = T)
      data [[p_i]][[w_i]][[d_i]]$IBI <- IBI
      
      Temp <- read.csv("Temp.csv",header = F,as.is = T)
      data [[p_i]][[w_i]][[d_i]]$Temp <- Temp
    }
  }
}




library(readxl)
survey<-read_xlsx("C:\\Sleep-quality-in-caregivers-master\\Examine_your_sleep_with_our_model\\Data\\survey_excel.xlsx")
# specify the work space
setwd("C:\\Sleep-quality-in-caregivers-master\\Examine_your_sleep_with_our_model\\Data")

#>>> Catch Data From E4 for caregiver sleep study
#CatchDataFromE4_CaregiverSleep

#>>> Catch Data From the surveys of caregiver sleep study and cleaning the data
#CatchDataFromSurvery_CaregiverSleep

#>>> Data set description (Survay analysis)
#Survey Analysis

#>>> Feature computation
survey$swsLengthHR <- NA
survey$swsTimeHR <- NA
survey$swsLengthT <- NA
survey$swsTimeT <- NA
survey$decreasePercentageT <- NA
survey$swsTimeM <- NA
survey$swsLengthM <- NA
survey$decreasePercentageM <- NA
survey$amountAsleep <- NA
survey$amountAwake <- NA
survey$sleepEfficiency <- NA
survey$timesAwoken <- NA
survey$epochCapacity <- NA
survey$epochPeak <- NA
survey$epochPeakCounter <- NA
survey$stormPeak <- NA
survey$largestStorm <- NA
survey$timesEdaStorm <- NA
survey$meanEdaStorm <- NA
survey$lengthEdaStorm <- NA

#>>>>>>> connecting to the local matlab for EDA features
#install.packages("R.matlab")
library(R.matlab)
Matlab$startServer()
matlab <- Matlab()
isOpen <- open(matlab)
print(matlab)

root = "C:\\Sleep-quality-in-caregivers-master\\Source code\\"

source(paste(root,'ACC_Sleep.R', sep = ''))
source(paste(root,'EDA.R', sep = ''))
source(paste(root,'HRV_SWS.R', sep = ''))
source(paste(root,'Temp_Sleep.R', sep = ''))

for (p_i in 1:18){
  for (w_i in 1:2){
    for (d_i in 1:9){
      
      #>>>>>>>>>Computing the SWS drived from HRV
      print(paste('participant:',p_i, 'weak:',w_i,'day:', d_i))
      input<-data[[p_i]][[w_i]][[d_i]]$IBI
      if(is.null(input)){
        print("no data is recorded")
        next
        }
      input <- input[2:length(input[,1]),]# removing the the unwanted header
      sws<-HRV_SWS(input)
      survey$swsLengthHR[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- sws[[1]]
      survey$swsTimeHR[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- sws[[2]]
      
      #>>>>>>>>>Computing the temperture trend from the data
      input <- data[[p_i]][[w_i]][[d_i]]$Temp
      input <- input[3:length(input[,1]),1]# removing the the unwanted header
      temp<-Temp_Sleep(input)
      survey$swsLengthT[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- temp[[1]]
      survey$swsTimeT[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- temp[[2]]
      survey$decreasePercentageT[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- temp[[3]]
      
      #>>>>>>>>>Computing the movement trend from the data
      input <- data[[p_i]][[w_i]][[d_i]]$ACC
      input <- input[3:length(input[,1]),]# removing the the unwanted header
      ACC<-ACC_Sleep(input)
      survey$swsLengthM[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- ACC[[1]]
      survey$swsTimeM[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- ACC[[2]]
      survey$decreasePercentageM[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- ACC[[3]]
      #>>>>>>>>>Importing the EDA features catched from the matlab files
      input1 <- data[[p_i]][[w_i]][[d_i]]$ACC
      input2 <- data[[p_i]][[w_i]][[d_i]]$EDA
      EDA_sleep<-EDA(input1, input2)
      survey$amountAsleep[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- EDA_sleep$amountAsleep
      survey$amountAwake[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- EDA_sleep$amountAwake
      survey$sleepEfficiency[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- EDA_sleep$sleepEfficiency
      survey$timesAwoken[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- EDA_sleep$timesAwoken
      survey$epochCapacity[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- EDA_sleep$epochCapacity
      survey$epochPeak[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- EDA_sleep$epochPeak
      survey$epochPeakCounter[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- EDA_sleep$epochPeakCounter
      survey$stormPeak[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- EDA_sleep$stormPeak
      survey$largestStorm[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- EDA_sleep$largestStorm
      survey$timesEdaStorm[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- EDA_sleep$timesEdaStorm
      survey$meanEdaStorm[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- EDA_sleep$meanEdaStorm
      survey$lengthEdaStorm[which(survey$`Particiapnt #`==p_i & survey$Week==w_i & survey$`Survey Number`==d_i)] <- EDA_sleep$lengthEdaStorm
      
    }
  }
}

close(matlab)

#library(xlsx)
#dirtext<-c("C:\\Users\\Reza Sadeghi\\Desktop\\Dementia Caregiver Sleep Dataset")
#file <- paste(survey, "/Output.xlsx", sep="")
#res<- write.xlsx(survey, file, row.names=T)
saveRDS(survey, "survey.rds")
library(writexl)
write_xlsx(survey,"./Output.xlsx")

#>>> Classification
#Classification.R
