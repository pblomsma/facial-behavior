####################################################################################################
# Author: Peter Blomsma
# Date: December 2018
# Description: Imports the different datasets that are used for the experiments as described in the following paper:
#
# Pieter A Blomsma, Julija Vaitonyte, Maryam Alimardani, and Max M Louwerse.2020. Spontaneous Facial 
# Behavior Revolves Around Neutral Facial Displays.In Proceedings of the 20th ACM International Conference 
# on Intelligent Virtual Agents.
# 
# This research has been funded by a grant No.: PROJ-007246 fromthe European Union, OP Zuid, the Ministry of
# Economic affairs,the Province of Noord-Brabant, and the municipality of Tilburg awarded to Max M. Louwerse.
####################################################################################################
# 
# The script uses the PAIN dataset as an example. Of course you can analyse your own dataset with this script
# as well.
#
# Pain Dataset:
#
# Lucey, Patrick, et al. "Painful data: The UNBC-McMaster shoulder pain expression archive database." Face and Gesture 2011. IEEE, 2011.
#
####################################################################################################

#Libraries
library(reshape)
library(reshape2)
library(stringr)
library(dplyr)
library(ggplot2)
library(sjPlot)
library(data.table)


read_pain <- function()
{
  files <- dir(choose.dir(default = "", caption = "Select PAIN's FACS dir folder"), recursive = TRUE, full.names = TRUE)

  FACS <- data.frame(matrix(0, ncol = 60, nrow = 0))
  colnames(FACS) <- factor(c(1:60))

  for(i in 1:length(files))
  {
    FACS[nrow(FACS)+1,] <- NA
    FACS[nrow(FACS), 1 ] <- files[i]
    
    if(file.info(files[i])$size > 0)
    {
      current <- read.table(files[i])
      
      for(x in 1:nrow(current))
      {
        if(current[x,1] > 0)
        {
          FACS[nrow(FACS), current[x,1]] <- current[x,2] 
        }
      }
    }
  }

  #throw away AU columns that do not occur in the dataset:
  FACS <- FACS[, colSums(is.na(FACS)) != nrow(FACS)]
  setnames(FACS, old=c("4", "6","7","9","10","12", "15" ,"20","25","26","27","43", "50"), new= PAIN_AUs)
  colnames(FACS)[1] <- "Filename"
  
  FACS$Filename <- as.character(FACS$Filename)
  FACS$Sequence_id <- NA
  FACS$Subject_id <- NA
  FACS$Frame_id <- NA
  
  for(i in 1:nrow(FACS))
  {
    tmpList <- unlist(strsplit(unlist(FACS[i, ]$Filename), "/"))
    FACS[i, ]$Frame_id <- tmpList[length(tmpList)]  
    FACS[i, ]$Frame_id <- substr(FACS[i, ]$Frame_id, 0, nchar(FACS[i, ]$Frame_id) - 9)
    FACS[i, ]$Sequence_id <- tmpList[length(tmpList)-1]  
    FACS[i, ]$Subject_id <- tmpList[length(tmpList)-2]  
  }
  FACS$Filename <- NULL
  
  #WHATS THIS NUMBER?
  FACS[is.na(FACS)] <- 0
  return(FACS)
}

analyze <- function(dataset_summary, dataset, title)
{
  dataset_summary <- dataset_summary %>% arrange(desc(n))
  dataset_summary$rank <-  seq.int(nrow(dataset_summary))
  
  #Idiosyncracy Graph
  plot(ggplot(dataset_summary, aes(x=subjects)) + geom_histogram(binwidth=1) + xlab("Number of subjects") + ylab("Number of distinct facial expressions"))  
  readline(prompt = "Pause. Press <Enter> to continue...")
  
  #Idiosyncracy Frames Graph
  plot(ggplot(dataset_summary %>% filter(n < 500), aes(x=n)) + geom_histogram() + xlab("Number of frames") + ylab("Number of distinct facial expressions"))
  readline(prompt = "Pause. Press <Enter> to continue...")
  
  #Frequency graph
  selection <- dataset_summary %>% filter(rank < 11)
  selection$rank <- as.factor(selection$rank)
  plot(ggplot(selection, aes(rank, freq)) + geom_bar(stat = "identity") + xlab("Rank order") + ylab("Facial Configuration Frequency in %") + ggtitle(title) + scale_y_continuous(limits=c(0,100)))
  
  readline(prompt = "Pause. Press <Enter> to continue...")

  #Histogram over neutral expression frequency
  most.prominent.expression <- dataset_summary[which(dataset_summary$freq == max(dataset_summary$freq)),]$Face_state_id
  freqs <- dataset %>% group_by(Subject_id)  %>% summarise(freq = 100 * (sum(Face_state_id == most.prominent.expression)/n())) #aes(y=..density..*400), color = "red"
 
  plot(ggplot(freqs, aes(x=freq)) + geom_histogram(binwidth=10) + xlab("Neutral configuration frequency (%)") + ggtitle(title) + ylab("Number of subjects") + geom_density(aes(y=..count..*10), color = "red", size = 2) + scale_x_continuous(limits=c(0,100)) + ggtitle(title))  
  readline(prompt = "Pause. Press <Enter> to continue...")
 
  #Proportion of subjects: 
  require(scales)
  plot(ggplot(freqs, aes(x=freq)) + geom_histogram(aes(y=(..count..)/sum(..count..)*100), binwidth=10)  + geom_density(aes(y=..count..*41), color = "red", size = 2) + xlab("Neutral configuration frequency (%)") + ggtitle(title) + ylab("Percentage of subjects (%)")  + scale_x_continuous(limits=c(0,100)) + ggtitle(title))  
  readline(prompt = "Pause. Press <Enter> to continue...")
  
  #Number of idiosyncratic faces:
  print(paste("Mean frequency of neutral:", mean(freqs$freq)))
  print(paste("SD of frequency of neutral:", sd(freqs$freq)))  
  
  #Number of idiosyncratic faces:
  print(paste("Number idiosyncratic facial expressions:", count(dataset_summary %>% filter(subjects < 2))))
  
  #Number of idiosyncratic faces - frames:
  print(paste("Number of frames that contain idiosyncratic facial expressions: ", sum((dataset_summary %>% filter(subjects < 2))$n)))
  
  #Number of idiosyncratic + 1 time only faces:
  print(paste("Number of idiosyncratic facial expressions  that only appeared in one frame: ", count(dataset_summary %>% filter(n < 2))))

  #Frequency table
  table_content <-  dataset_summary %>% filter(rank < 21)
  specify_decimal <- function(x, k) trimws(format(round(x, k), nsmall=k))
  table_content$freq <- specify_decimal(table_content$freq, 2)
  return(table_content)
}

pain <- read_pain()

#Add Face_state_id to discren unique facial configurations.
pain <-  mutate(pain, Face_state_id = paste(au4, au6, au7, au9, au10, au12, au20, au25,au26 ,au27,au43 , au50, sep = '_'))

#Summarize over distinct facial configurations.
pain_summary <-  pain %>% group_by(Face_state_id) %>% summarise(subjects = n_distinct(Subject_id), n = n(), freq = 100 * n()/nrow(pain), au4 = max(au4),au6  = max(au6),au7= max(au7), au9 =  max(au9), au10 = max(au10), au12 = max(au12),  au20 =  max(au20), au25 = max(au25), au26 = max(au26), au27 = max(au27), au43 = max(au43) )

#Do the analysis, produce the the different graphs to gain inside into the dataset.
pain_table_content <- analyze(pain_summary, pain, "PAIN")

#Odd construction to create a WORD compliant table.
tab_df(pain_table_content %>% select(freq, au4, au6, au7, au9, au10, au12, au15 ,au20, au25,au26 ,au27,au43, au50))