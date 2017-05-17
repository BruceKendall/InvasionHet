### Creates the data objects popLer and popLer_cm, representing the Ler populations
###     experiments

# Raw data consists of one file per generation
# Final column is named inconsistently, so needs to be corrected before merge

## To use Jenn's code I need to change the working directory...
data_dir <- "~/Dropbox/Arabidopsis/analysis"
savewd <- getwd() # Where to get back to when we're done
setwd(data_dir)



####### BEGIN CODE FROM JENN ###################
###ALL SEEDLING DATA

seedlings_gen1<-read.csv("2014_02_18_Exp1_Gen1_seedposition.csv", header=T)
names(seedlings_gen1)<-c("ecot","ID","trt","Rep","gap_size","gen","pot","d","sdlgs")

seedlings_gen2<-read.csv("2014_06_12_Exp1_Gen2_seedposition.csv", header=T)
names(seedlings_gen2)<-c("ecot","ID","trt","Rep","gap_size","gen","pot","d","sdlgs")

seedlings_gen3<-read.csv("2014_11_11_Exp1_Gen3_seedposition.csv", header=T) #Gen3
names(seedlings_gen3)<-c("ecot","ID","trt","Rep","gap_size","gen","pot","d","sdlgs")

seedlings_gen4<-read.csv("2015_03_11_Exp1_Gen4_seedposition.csv", header=T) #Gen4
names(seedlings_gen4)<-c("ecot","ID","trt","Rep","gap_size","gen","pot","d","sdlgs")

seedlings_gen5<-read.csv("2015_08_11_Exp1_Gen5_seedposition.csv", header=T) #Gen5
names(seedlings_gen5)<-c("ecot","ID","trt","Rep","gap_size","gen","pot","d","sdlgs")

seedlings_gen6<-read.csv("2015_11_10_Exp1_Gen6_seedposition.csv", header=T) #Gen6
names(seedlings_gen6)<-c("ecot","ID","trt","Rep","gap_size","gen","pot","d","sdlgs")

seedlings_gen6<- seedlings_gen6[-140,] #drop ID 27 record of jumping 8 pots
seedlings_gen6<- seedlings_gen6[-c(163:169),] #drop ID 32 record of jumping 6 pots and being super dense

seedlingsALL<-rbind(seedlings_gen1,seedlings_gen2, seedlings_gen3, seedlings_gen4, seedlings_gen5, seedlings_gen6)

#drop runways with problems
# 35 & 60: a pot went missing (so it looks like the runway shrank)
# 45: different data on spreadsheet as on data sheets and can't figure it out. also, HUGE leap
seedlingsALL<-subset(seedlingsALL,!(ID %in% c(35, 60)))

####### END CODE FROM JENN ###################

# Get back home
setwd(savewd)

# Change to my name and clean up
popLer_cm <- seedlingsALL
rm(list = ls(pattern = "seedlings"))

# Clean up column names and get a useful order
popLer_cm <- popLer_cm[-1]
names(popLer_cm) <- c("ID", "Treatment", "Replicate", "Gap", "Generation", "Pot", "Distance",
                      "Seedlings")
popLer_cm <- popLer_cm[!is.na(popLer_cm$ID),]

ord <- with(popLer_cm, order(Treatment, Gap, Replicate, Generation, Pot, Distance))
popLer_cm <- popLer_cm[ord,]

# Make some factor variables
popLer_cm$Rep <- as.factor(popLer_cm$Replicate)
popLer_cm$Gen <- as.factor(popLer_cm$Generation)

# Make a version that just has pot totals
popLer <- plyr::ddply(popLer_cm, plyr::.(ID, Gap, Replicate, Rep, Treatment, Generation, Gen, Pot), summarize,
                Seedlings = sum(Seedlings))
ord <- with(popLer, order(Treatment, Gap, Rep, Generation, Pot))
popLer <- popLer[ord,]

# Clean up the workspace
rm(data_dir, ord)

# Auto-cache the data
ProjectTemplate::cache("popLer")
ProjectTemplate::cache("popLer_cm")
