#Classifcations
#Cole Adams

setwd("~/NCSU Baseball/Datasets/SQL Datasets")

library(dplyr)
library(ggplot2)
library(mclust)
library(plotly)

set.seed(2)

# Initizialing the combined SQL Data
SQLData <- read.csv("bama2021seasonSQL.csv", header = TRUE, row.names = NULL, sep = ";", na.strings = "0.0")

#####
# Ordering the data sequentially based on the order in the season
GameNum <- 0
SQLData$GameNo <- NA
for(i in 1:length(SQLData$PitchNo))
{
  if(SQLData[i,"PitchNo"] == 1)
  {
    GameNum <- GameNum + 1
  }
  SQLData[i,"GameNo"] <- GameNum
}

SQLData <- SQLData %>% arrange(desc(GameNo))
SQLData <- SQLData[,-76]
#####

#Setting up a column that will allow for reordering after classification process
SQLData$sorter <- seq(from=1, to=length(SQLData[,1]), by=1)
#Setting up a column that will serve as the correct pitch classifications
SQLData$TruePitch <- NA

#Filtering based on the opposing team's pitchers
pitchers <- SQLData %>% filter(RelSpeed > 0 & SpinRate > 0 & PitcherTeam == "ALA_CRI")
other <- SQLData %>% filter(PitcherTeam != "ALA_CRI" & RelSpeed > 0 & SpinRate > 0)

#Simplifying pitches that were autotagged by Trackman. Not useful for classifications but for visualization if needed
for(i in 1:length(pitchers$Pitcher))
{
  if(pitchers[i, "TaggedPitchType"]  == "Fastball"){
    pitchers[i, 'TruePitch'] <- 'Fastball'
  }else if(pitchers[i, 'TaggedPitchType'] == "Curveball"){
    pitchers[i, 'TruePitch'] <- 'Curveball'
  }else if(pitchers[i, 'TaggedPitchType'] == "Slider"){
    pitchers[i, 'TruePitch'] <- 'Slider'
  }else if(pitchers[i, 'TaggedPitchType'] == "ChangeUp"){
    pitchers[i, 'TruePitch'] <- 'Changeup'
  }else if(pitchers[i, 'TaggedPitchType'] == "Cutter"){
    pitchers[i, 'TruePitch'] <- 'Fastball'
  }else if(pitchers[i, 'TaggedPitchType'] == "Sinker"){
    pitchers[i, 'TruePitch'] <- 'Fastball'
  }else if(pitchers[i, 'TaggedPitchType'] == "Undefined"){
    pitchers[i, 'TruePitch'] <- 'Undefined'
  }
}

###
# Split based on lefty/ righty

PL <- pitchers %>% filter(PitcherThrows == "Left")
PR <- pitchers %>% filter(PitcherThrows == "Right")
# Tabulating based on if a pitcher has enough pitches to be effective for classifciations
tabR <- table(PR$Pitcher)
tabL <- table(PL$Pitcher)
PR <- PR[PR$Pitcher %in% names(tabR)[tabR>15],]
PL <- PL[PL$Pitcher %in% names(tabL)[tabL>15],]
#

# Forms a list to run for the for loop to classify all pitchers at once
kTrainR <- unique(as.character(PR$Pitcher))
kTrainL <- unique(as.character(PL$Pitcher))
####### Function

classifyR <- function(playerName)
{
  #Setting up a temp dataset
  player <- pitchers %>% filter(Pitcher == playerName)
  # K-Means Classifcation below, using RelSpeed, HorzBreak & Vert Break. ALT: InducedVertBreak/ Spin
  mcplayer <- Mclust(player[,c("RelSpeed", "HorzBreak", "VertBreak")], G= c(1,2,3,4))
  player$TruePitch <- mcplayer$classification
  
  # Assigning fastball tag to fastball group
  high <- player[which.max(player$RelSpeed),"TruePitch"]
  for(i in 1:length(player$TruePitch))
  {
    if(player[i,"TruePitch"] == high)
    {
      player[i,"TruePitch"] <- "Fastball"
    }
  }
  
  # Remain functions to show remaining data classified but not grouped
  remain <- unique(player$TruePitch)
  remain <- remain[remain != "Fastball"]
  print(remain)
  
  temprelF <- 0
  
  #Currently, this serves to assign a different fastball type under a single fastball umbrella
  for(i in remain)
  {
    if(abs(mean(subset(player, TruePitch == "Fastball")$RelSpeed)) -abs(mean(subset(player, TruePitch == i)$RelSpeed)) <= 3)
    {
     temprelF <- i
    }
  }
  
  for(i in 1:length(player$TruePitch))
  {
    if(player[i,"TruePitch"] == temprelF)
    {
      player[i,"TruePitch"] <- "Fastball"
    }
  }
  
  # Updating remain post-fastballs
  
  remain <- unique(player$TruePitch)
  remain <- remain[remain != "Fastball"]

  #Changeups
  
  chCount <- 1
  chtemp <- c()
  for(i in remain)
  {
    if(mean(subset(player, TruePitch == "Fastball")$HorzBreak) -mean(subset(player, TruePitch == i)$HorzBreak) < 5)
    {
      chtemp[chCount] <- i 
    }else
    {
      chtemp[chCount] <- 0
    }
    chCount <- chCount + 1
  }
  
  for(i in 1:length(player$TruePitch))
  {
    if(player[i,"TruePitch"] %in% chtemp)
    {
      player[i,"TruePitch"] <- "ChangeUp"
    }
  }
  
  # Updating remain post-changeups
  
  remain <- unique(player$TruePitch)
  remain <- remain[!(remain %in% c("Fastball", "ChangeUp"))]
  
  #This checks if that there are more than two more groups, something went wrong
  if(length(remain) >= 3)
  {
    print("Problem")
    for(i in 1:length(player$TruePitch))
    {
      player[i,"TruePitch"] <- player[i, "TaggedPitchType"]
    }
  }else if(length(remain) == 2)
  {
    fastertemp <- remain[which.max(c(mean(subset(player, TruePitch == remain[1])$RelSpeed), mean(subset(player, TruePitch == remain[2])$RelSpeed)))]
    slowertemp <- remain[which.min(c(mean(subset(player, TruePitch == remain[1])$RelSpeed), mean(subset(player, TruePitch == remain[2])$RelSpeed)))]
  }
  
  #Slider/ Curves when there are both slider/ curve
  if(length(remain) == 2)
  {
    for(i in 1:length(player$TruePitch))
    {
      if(player[i,"TruePitch"] == fastertemp)
      {
        player[i,"TruePitch"] <- "Slider"
      }else if(player[i,"TruePitch"] == slowertemp)
      {
        player[i,"TruePitch"] <- "Curveball"
      }
    }
  }
  
  # Updating remain post-two breakers
  
  remain <- unique(player$TruePitch)
  remain <- remain[!(remain %in% c("Fastball", "ChangeUp", "Slider", "Curveball"))]
  
  #Final breaking pitch
  if(length(remain) == 1)
  {
    for(i in 1:length(player$TruePitch))
    {
      if(mean(subset(player, TruePitch == remain[1])$VertBreak) < -45 & player[i,"TruePitch"] == remain[1])
      {
        player[i,"TruePitch"] <- "Curveball"
      }else if(mean(subset(player, TruePitch == remain[1])$VertBreak) > -45 & player[i,"TruePitch"] == remain[1])
      {
        player[i,"TruePitch"] <- "Slider"
      }
    }
  }
  
  #
  
  allplayers <- rbind(allplayers, player)
  allplayers <<- allplayers
}

#Similar to classifyR except inverted when necessary
classifyL <- function(playerName)
{
  player <- pitchers %>% filter(Pitcher == playerName)
  mcplayer <- Mclust(player[,c("RelSpeed", "HorzBreak", "VertBreak")], G= c(1,2,3,4))
  player$TruePitch <- mcplayer$classification
  #player <- cbind(player, mcplayer$classification)
  #
  high <- player[which.max(player$RelSpeed),"TruePitch"]
  for(i in 1:length(player$TruePitch))
  {
    if(player[i,"TruePitch"] == high)
    {
      player[i,"TruePitch"] <- "Fastball"
    }
  }
  
  remain <- unique(player$TruePitch)
  remain <- remain[remain != "Fastball"]
  print(remain)
  
  temprelF <- 0
  
  for(i in remain)
  {
    if(abs(mean(subset(player, TruePitch == "Fastball")$RelSpeed)) -abs(mean(subset(player, TruePitch == i)$RelSpeed)) <= 3)
    {
      temprelF <- i
    }
  }
  
  for(i in 1:length(player$TruePitch))
  {
    if(player[i,"TruePitch"] == temprelF)
    {
      player[i,"TruePitch"] <- "Fastball"
    }
  }
  
  # Updating remain post-fastballs
  
  remain <- unique(player$TruePitch)
  remain <- remain[remain != "Fastball"]
  
  #Changeups
  
  chCount <- 1
  chtemp <- c()
  for(i in remain)
  {
    if(mean(subset(player, TruePitch == i)$HorzBreak) -mean(subset(player, TruePitch == "Fastball")$HorzBreak) < 5)
    {
      chtemp[chCount] <- i 
    }else
    {
      chtemp[chCount] <- 0
    }
    chCount <- chCount + 1
  }
  
  for(i in 1:length(player$TruePitch))
  {
    if(player[i,"TruePitch"] %in% chtemp)
    {
      player[i,"TruePitch"] <- "ChangeUp"
    }
  }
  
  # Updating remain post-changeups
  
  remain <- unique(player$TruePitch)
  remain <- remain[!(remain %in% c("Fastball", "ChangeUp"))]
  
  if(length(remain) >= 3)
  {
    print("Problem")
    for(i in 1:length(player$TruePitch))
    {
      player[i,"TruePitch"] <- player[i, "TaggedPitchType"]
    }
  }else if(length(remain) == 2)
  {
    fastertemp <- remain[which.max(c(mean(subset(player, TruePitch == remain[1])$RelSpeed), mean(subset(player, TruePitch == remain[2])$RelSpeed)))]
    slowertemp <- remain[which.min(c(mean(subset(player, TruePitch == remain[1])$RelSpeed), mean(subset(player, TruePitch == remain[2])$RelSpeed)))]
  }
  
  if(length(remain) == 2)
  {
    for(i in 1:length(player$TruePitch))
    {
      if(player[i,"TruePitch"] == fastertemp)
      {
        player[i,"TruePitch"] <- "Slider"
      }else if(player[i,"TruePitch"] == slowertemp)
      {
        player[i,"TruePitch"] <- "Curveball"
      }
    }
  }
  # Updating remain post-two breakers
  
  remain <- unique(player$TruePitch)
  remain <- remain[!(remain %in% c("Fastball", "ChangeUp", "Slider", "Curveball"))]
  
  if(length(remain) == 1)
  {
    for(i in 1:length(player$TruePitch))
    {
      if(mean(subset(player, TruePitch == remain[1])$VertBreak) < -45 & player[i,"TruePitch"] == remain[1])
      {
        player[i,"TruePitch"] <- "Curveball"
      }else if(mean(subset(player, TruePitch == remain[1])$VertBreak) > -45 & player[i,"TruePitch"] == remain[1])
      {
        player[i,"TruePitch"] <- "Slider"
      }
    }
  }
  
  #
  
  allplayers <- rbind(allplayers, player)
  allplayers <<- allplayers
}

#Set-up of holding dataset and final dataset: classFinal
allplayers <- pitchers[1,]
allplayers <- allplayers[-1,]

for (i in kTrainR) {
  classifyR(i)
}

rightClass <- allplayers
allplayers <- pitchers[1,]
allplayers <- allplayers[-1,]

for (i in kTrainL) {
  classifyL(i)
}

leftClass <- allplayers
allplayers <- pitchers[1,]
allplayers <- allplayers[-1,]

classFinal <- rbind(leftClass, rightClass)

# Visual Checks on opposing pitches
for (i in kTrainL) {
  print(ggplot(classFinal %>% filter(Pitcher == i), aes(x= RelSpeed, y = HorzBreak)) + geom_point(aes(color = (factor(TruePitch)))) + ggtitle(i))
  print(ggplot(classFinal %>% filter(Pitcher == i), aes(x= RelSpeed, y = VertBreak)) + geom_point(aes(color = (factor(TruePitch))))+ ggtitle(i))
  print(ggplot(classFinal %>% filter(Pitcher == i), aes(x= HorzBreak, y = VertBreak)) + geom_point(aes(color = (factor(TruePitch))))+ ggtitle(i))
  print(ggplot(classFinal %>% filter(Pitcher == i), aes(x= HorzBreak, y = InducedVertBreak)) + geom_point(aes(color = (factor(TruePitch))))+ ggtitle(i))
  print(ggplot(classFinal %>% filter(Pitcher == i), aes(x= RelSpeed, y = SpinRate)) + geom_point(aes(color = (factor(TruePitch))))+ ggtitle(i))
}

#Final sorting of data and then output
safe <- classFinal %>% filter(!is.na(SpinRate))
other$TruePitch <- other$AutoPitchType

classFinal <- rbind(classFinal, other)
classFinal <- classFinal %>% arrange(sorter)
setwd("~/NCSU Baseball/Datasets/Classified Datasets")
write.csv(classFinal, file = "bamatotalClassified.csv")

rm(allplayers)
rm(leftClass)
rm(rightClass)
