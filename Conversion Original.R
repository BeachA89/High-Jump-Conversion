#library(SparkR)
library(rCAT)
library(DT)
library(dplyr)
library(tibble)
library(tidyr)
library(plyr)
library(data.table)
library(stringr)

data_folder <- "C:/Users/aaron.beach/OneDrive - nswis.com.au/GitHub/High-Jump-Conversion/datafolder2"
Processed_data <- "C:/Users/aaron.beach/OneDrive - nswis.com.au/GitHub/High-Jump-Conversion/Processed data/"
filenames = list.files(data_folder, pattern = "*.csv", full.names = T)
dataname = basename(filenames)
dataname <-  str_remove_all(dataname, ".csv")


table1 <-  lapply(filenames, fread, header=FALSE, stringsAsFactors=FALSE)

#imported_data <- read.csv("C:/Users/aaron.beach/OneDrive - nswis.com.au/R/Hammer Conversion/sample2.csv", header=FALSE, stringsAsFactors=FALSE)

labels <-  t(data.frame(strsplit(dataname, "_")))
fullnames <-  data.frame(dataname, labels)


Collateddata = NULL
for (k in 1:length(table1)){
  fullnames1 <- fullnames[k,]
  data1 = as.data.frame(table1[[k]])
  
  
  NumberJumps <-  as.numeric(data1[10,2])
  NumberSteps <-  as.numeric(data1[11,2])
  lastrow <-  NumberSteps +8
  PenultimateStep <-  NumberSteps-1+6
  ThirdLastContact <- NumberSteps-2
  SecondLastContact <- NumberSteps-1
  LastContact <- NumberSteps
  
  
  SamplingRate <-  as.numeric(data1[16,1])
  ConversionFactor <-  as.numeric(data1[16,2])
  
  ncols <- max.col(!is.na(data1[4,]), 'last')
  Footdata <- data.matrix(data1[6:lastrow, 7:ncols])
  Footdata <-  Footdata/ConversionFactor
  
  
  numrows <- nrow(Footdata)
  
  `Height cleared` <- t(data1[2,7:ncols])
  Height <-  t(data1[3,7:ncols])
  Cleared <-  t(data1[4,7:ncols])
  FootPlants <-  data.matrix(Footdata[c(1:(numrows-5),numrows-3, numrows-1), 1:ncol(Footdata)])
  FootToeOffs <-  data.matrix(Footdata[c(numrows-4, numrows-2, numrows),1:ncol(Footdata)])
  
  
  
  
  
  StepTimes = data.frame()
  ContactTimes = data.frame()
  Cadencev1 = data.frame()
  Cadencev2 = data.frame()

  for (l in 1:NumberJumps){

    ContactTimes[1,l] <- (FootToeOffs[1,l] - FootPlants[ThirdLastContact,l])
    ContactTimes[2,l] <- (FootToeOffs[2,l] - FootPlants[SecondLastContact,l])
    ContactTimes[3,l] <- (FootToeOffs[3,l] - FootPlants[LastContact,l])
    
    
    for (j in 2:NumberSteps){
      
      StepTimes[j-1,l] <- FootPlants[j,l] - FootPlants[(j-1),l]
    }
    
    
    Cadencev1[1,l] <- NumberSteps/(FootPlants[LastContact,l])
    Cadencev2[1,l] <- NumberSteps/(FootPlants[LastContact,l])
    
    for (k in 2:(NumberSteps)){
      Cadencev1[k,l] <- (k-1)/FootPlants[k,l]}
    for (k in 2:(NumberSteps)){
      Cadencev2[k,l] <- (1)/StepTimes[k-1,l]}
    
  }
  colnames_Footplant = NULL
  colnames_ContactTimes = c("Contact Time 3rd last", "Contact Time 2nd last", "Contact Time last")
  colnames_StepTimes = NULL
  colnames_Cadencev1 = NULL
  colnames_Cadencev2 = NULL
  
  for (h in 1:(NumberSteps)){
    colnames_Footplant[h] <- paste0("Foot Plant Step ",h)
  }
  colnames_Footplant[NROW(colnames_Footplant)] <- "Foot Plant Take-Off"
  colnames_Cadencev1[1] = "Cadencev1Avg"
  colnames_Cadencev2[1] = "Cadencev2Avg"
  
  
  for (h in 1:(NumberSteps-1)){
    colnames_StepTimes[h] <- paste0("Step Time Step ",h)
    colnames_Cadencev1[h+1] <- paste0("Cadencev1 Step ",h)
    colnames_Cadencev2[h+1] <- paste0("Cadencev2 Step ",h)
  }
  
  
  
  
  
  FootPlantsT <-  t(FootPlants)
  
  colnames(FootPlantsT) <- colnames_Footplant
  
  ContactTimesT <-  t(ContactTimes)
  colnames(ContactTimesT) <- colnames_ContactTimes
  
  
  
  StepTimesT <-  t(StepTimes)
  colnames(StepTimesT) <- colnames_StepTimes
  
  Cadencev1T <-  t(Cadencev1)
  colnames(Cadencev1T) <- colnames_Cadencev1
  
  Cadencev2T <-  t(Cadencev2)
  colnames(Cadencev2T) <- colnames_Cadencev2
  Jump <- (c(1:NumberJumps))
  
  ExportData <-  cbind.data.frame(fullnames1, NumberSteps, Jump, `Height cleared`, Height, Cleared, FootPlantsT, ContactTimesT, StepTimesT,Cadencev1T, Cadencev2T)
  #rownames(ExportData) <- row_names
  ExportData <-  ExportData %>% dplyr::rename(Name = X1, Competition = X2, `Height cleared` = `2`, Height = `3`, Cleared = `4`)
  
  filename = paste0(ExportData[1,1], "_processed")
  write.csv(ExportData, paste0(Processed_data,filename, ".csv"),row.names=FALSE)
  
  Collateddata[[filename]] <- ExportData  
  
}





#   for (i in 1:NumberJumps){
#     
#     
#     FlightTimes <- 
#     
#     FootPlantdata$DUTurn1[i] <- as.numeric(round((as.numeric(FootPlant[3,i]) - as.numeric(FootFalls[2,i]))*ConversionFactor,2))
#     
#     
#     
#     
#     
#     
#     
#     Footfalldata$DUTurn1[i] <- as.numeric(round((as.numeric(FootFalls[3,i]) - as.numeric(FootFalls[2,i]))*ConversionFactor,2))
#     Footfalldata$DUTurn2[i] <- as.numeric(round((as.numeric(FootFalls[6,i]) - as.numeric(FootFalls[5,i]))*ConversionFactor,2))
#     Footfalldata$DUTurn3[i] <- as.numeric(round((as.numeric(FootFalls[9,i]) - as.numeric(FootFalls[8,i]))*ConversionFactor,2))
#     Footfalldata$DUTurn4[i] <- as.numeric(round((as.numeric(FootFalls[13,i]) - as.numeric(FootFalls[11,i]))*ConversionFactor,2))
#     
#     
#     Footfalldata$Turn1Single[i] <- round((as.numeric(FootFalls[2,i]) - as.numeric(FootFalls[1,i]))*ConversionFactor,2)
#     Footfalldata$Turn1Double[i] <- round((as.numeric(FootFalls[4,i]) - as.numeric(FootFalls[2,i]))*ConversionFactor,2)
#     Footfalldata$Turn2Single[i] <- round((as.numeric(FootFalls[5,i]) - as.numeric(FootFalls[4,i]))*ConversionFactor,2)
#     Footfalldata$Turn2Double[i] <- round((as.numeric(FootFalls[7,i]) - as.numeric(FootFalls[5,i]))*ConversionFactor,2)
#     Footfalldata$Turn3Single[i] <- round((as.numeric(FootFalls[8,i]) - as.numeric(FootFalls[7,i]))*ConversionFactor,2)
#     Footfalldata$Turn3Double[i] <- round((as.numeric(FootFalls[10,i]) - as.numeric(FootFalls[8,i]))*ConversionFactor,2)
#     Footfalldata$Turn4Single[i] <- round((as.numeric(FootFalls[11,i]) - as.numeric(FootFalls[10,i]))*ConversionFactor,2)
#     Footfalldata$Turn4Double[i] <- round((as.numeric(FootFalls[13,i]) - as.numeric(FootFalls[11,i]))*ConversionFactor,2)
#     
#     Footfalldata$Turn1[i] <-  Footfalldata$Turn1Single[i] + Footfalldata$Turn1Double[i]
#     Footfalldata$Turn2[i] <-  Footfalldata$Turn2Single[i] + Footfalldata$Turn2Double[i]
#     Footfalldata$Turn3[i] <-  Footfalldata$Turn3Single[i] + Footfalldata$Turn3Double[i]
#     Footfalldata$Turn4[i] <-  Footfalldata$Turn4Single[i] + Footfalldata$Turn4Double[i]
#     Footfalldata$TotalTime[i] <-  sum(Footfalldata$Turn1[i],Footfalldata$Turn2[i], Footfalldata$Turn3[i], Footfalldata$Turn4[i],na.rm = TRUE)
#     
#     
#     Footfalldata$Turn1SinglePC[i] <-  round(Footfalldata$Turn1Single[i]/Footfalldata$TotalTime[i]*100,0)
#     Footfalldata$Turn1DoublePC[i] <-  round(Footfalldata$Turn1Double[i]/Footfalldata$TotalTime[i]*100,0)
#     Footfalldata$Turn2SinglePC[i] <-  round(Footfalldata$Turn2Single[i]/Footfalldata$TotalTime[i]*100,0)
#     Footfalldata$Turn2DoublePC[i] <-  round(Footfalldata$Turn2Double[i]/Footfalldata$TotalTime[i]*100,0)
#     Footfalldata$Turn3SinglePC[i] <-  round(Footfalldata$Turn3Single[i]/Footfalldata$TotalTime[i]*100,0)
#     Footfalldata$Turn3DoublePC[i] <-  round(Footfalldata$Turn3Double[i]/Footfalldata$TotalTime[i]*100,0)
#     Footfalldata$Turn4SinglePC[i] <-  round(Footfalldata$Turn4Single[i]/Footfalldata$TotalTime[i]*100,0)
#     Footfalldata$Turn4DoublePC[i] <-  round(Footfalldata$Turn4Double[i]/Footfalldata$TotalTime[i]*100,0)
#     
#     
#     Footfalldata$Turn1PC[i] <-  round(Footfalldata$Turn1[i]/Footfalldata$TotalTime[i]*100,0)
#     Footfalldata$Turn2PC[i] <-  round(Footfalldata$Turn2[i]/Footfalldata$TotalTime[i]*100,0)
#     Footfalldata$Turn3PC[i] <-  round(Footfalldata$Turn3[i]/Footfalldata$TotalTime[i]*100,0)
#     Footfalldata$Turn4PC[i] <-  round(Footfalldata$Turn4[i]/Footfalldata$TotalTime[i]*100,0)
#     
#   }
#   
#   Footfalldata <- data.frame(Footfalldata)
#   Footfalldatameans <-  as.numeric(round(numcolwise(mean,na.rm = TRUE)(Footfalldata),2))
#   Footfalldata_withmean <- rbind.data.frame(Footfalldata, Footfalldatameans)
#   
#   Turn1Single_Combined <- paste0(Footfalldata_withmean$Turn1Single, " (",Footfalldata_withmean$Turn1SinglePC,"%)") 
#   Turn1Double_Combined <- paste0(Footfalldata_withmean$Turn1Double, " (",Footfalldata_withmean$Turn1DoublePC,"%)") 
#   Turn2Single_Combined <- paste0(Footfalldata_withmean$Turn2Single, " (",Footfalldata_withmean$Turn2SinglePC,"%)") 
#   Turn2Double_Combined <- paste0(Footfalldata_withmean$Turn2Double, " (",Footfalldata_withmean$Turn2DoublePC,"%)") 
#   Turn3Single_Combined <- paste0(Footfalldata_withmean$Turn3Single, " (",Footfalldata_withmean$Turn3SinglePC,"%)") 
#   Turn3Double_Combined <- paste0(Footfalldata_withmean$Turn3Double, " (",Footfalldata_withmean$Turn3DoublePC,"%)") 
#   Turn4Single_Combined <- paste0(Footfalldata_withmean$Turn4Single, " (",Footfalldata_withmean$Turn4SinglePC,"%)") 
#   Turn4Double_Combined <- paste0(Footfalldata_withmean$Turn4Double, " (",Footfalldata_withmean$Turn4DoublePC,"%)") 
#   Turn4Double_Combined <- paste0(Footfalldata_withmean$Turn4Double, " (",Footfalldata_withmean$Turn4DoublePC,"%)") 
#   
#   Turn1_Combined <- paste0(Footfalldata_withmean$Turn1, " (",Footfalldata_withmean$Turn1PC,"%)") 
#   Turn2_Combined <- paste0(Footfalldata_withmean$Turn2, " (",Footfalldata_withmean$Turn2PC,"%)") 
#   Turn3_Combined <- paste0(Footfalldata_withmean$Turn3, " (",Footfalldata_withmean$Turn3PC,"%)") 
#   Turn4_Combined <- paste0(Footfalldata_withmean$Turn4, " (",Footfalldata_withmean$Turn4PC,"%)") 
#   
#   
#   ############################
#   Throwdata <- NULL
#   ThrowsCols <-  seq(1,ncol(Throws),2)
#   for(i in 1:RoundCount){
#     
#     
#     j = i*2-1
#     Throwdata$InHandx[i] <- as.numeric(Throws[1,j])
#     Throwdata$InHandy[i] <- as.numeric(Throws[1,j+1])
#     
#     Throwdata$OutHand1x[i] <- as.numeric(Throws[2,j])
#     Throwdata$OutHand1y[i] <- as.numeric(Throws[2,j+1])
#     
#     Throwdata$OutHand2x[i] <- as.numeric(Throws[3,j])
#     Throwdata$OutHand2y[i] <- as.numeric(Throws[3,j+1])
#     
#     Throwdata$OutHand3x[i] <- as.numeric(Throws[4,j])
#     Throwdata$OutHand3y[i] <- as.numeric(Throws[4,j+1])
#     
#     Throwdata$Angle1[i] <- round(rad2deg(atan2((Throwdata$OutHand1y[i]-Throwdata$InHandy[i]), (Throwdata$OutHand1x[i]-Throwdata$InHandx[i]))),2)
#     Throwdata$Angle2[i] <- round(rad2deg(atan2((Throwdata$OutHand2y[i]-Throwdata$OutHand1y[i]), (Throwdata$OutHand2x[i]-Throwdata$OutHand1x[i]))),2)
#     Throwdata$Angle3[i] <- round(rad2deg(atan2((Throwdata$OutHand3y[i]-Throwdata$OutHand2y[i]), (Throwdata$OutHand3x[i]-Throwdata$OutHand2x[i]))),2)
#     Throwdata$AngleAvg[i] <- round(mean(c(Throwdata$Angle1[i], Throwdata$Angle2[i], Throwdata$Angle3[i]),na.rm = TRUE),2)
#     
#     Throwdata$HVel1[i] <- round((Throwdata$OutHand1x[i]-Throwdata$InHandx[i])*ScaleFactorx/SamplingRate,2)
#     Throwdata$VVel1[i] <- round((Throwdata$OutHand1y[i]-Throwdata$InHandy[i])*ScaleFactory/SamplingRate,2)
#     Throwdata$HVel2[i] <- round((Throwdata$OutHand2x[i]-Throwdata$OutHand1x[i])*ScaleFactorx/SamplingRate,2)
#     Throwdata$VVel2[i] <- round((Throwdata$OutHand2y[i]-Throwdata$OutHand1y[i])*ScaleFactory/SamplingRate,2)
#     Throwdata$HVel3[i] <- round((Throwdata$OutHand3x[i]-Throwdata$OutHand2x[i])*ScaleFactorx/SamplingRate,2)
#     Throwdata$VVel3[i] <- round((Throwdata$OutHand3y[i]-Throwdata$OutHand2y[i])*ScaleFactory/SamplingRate,2)
#     Throwdata$HVelAvg[i] <- round(mean(c(Throwdata$HVel1[i], Throwdata$HVel2[i], Throwdata$HVel3[i]),na.rm = TRUE),2)
#     Throwdata$VVelAvg[i] <- round(mean(c(Throwdata$VVel1[i], Throwdata$VVel2[i], Throwdata$VVel3[i]),na.rm = TRUE),2)
#     
#     Throwdata$RVel1[i] <- round(Throwdata$HVel1[i]/cos(deg2rad(Throwdata$Angle1[i])),2)
#     Throwdata$RVel2[i] <- round(Throwdata$HVel2[i]/cos(deg2rad(Throwdata$Angle2[i])),2)
#     Throwdata$RVel3[i] <- round(Throwdata$HVel3[i]/cos(deg2rad(Throwdata$Angle3[i])),2)
#     Throwdata$RVelAvg[i] <- round(mean(c(Throwdata$RVel1[i], Throwdata$RVel2[i], Throwdata$RVel3[i]),na.rm = TRUE),2)
#     
#     Throwdata$RVelfromAvg[i] <- round(Throwdata$HVelAvg[i]/cos(deg2rad(Throwdata$AngleAvg[i])),2)
#   }
#   Throwdata <- data.frame(Throwdata)
#   
#   RoundCount2 <- c(1:RoundCount)
#   
#   ExportData = cbind.data.frame(fullnames1, RoundCount2,Distance, Throwdata, Footfalldata)
#   ExportData[,c(1:3,5)] <- sapply(ExportData[,c(1:3,5)],as.character)
#   ExportData <-  ExportData %>% dplyr::rename(Round = RoundCount2, Name = X1, Competition = X2)
#   
#   
#   filename = paste0(ExportData[1,1], "_processed")
#   write.csv(ExportData, paste0(Processed_data,filename, ".csv"),row.names=FALSE)
#   
#   Collateddata[[filename]] <- ExportData
# }
# Collateddatadf = bind_rows(Collateddata)
