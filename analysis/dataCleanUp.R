#Analysis of Second Sruvey Data
library(dplyr)
library(stringr)

#read in the data
#dat<-read.csv(file="tbsurvey2_questOnly_rev_noLocation.csv",header=T,quote="")
dat<-read.csv(file="tbsurvey2_responses_noLocation_UPDATED.csv",header=T,quote="",sep=",")
##########################################################################
# DATA WRANGLING
#########################################################################
#find columns that start with X

questOnly<-dat %>%
  select(starts_with("ID"),starts_with("Location"),starts_with("X"))

#parse to extract the questions

questStore<-c()

for(x in colnames(questOnly)){
  
  if(x %in% c("ID","Location")){
    questStore<-rbind(questStore,c(x,NA,NA))
    next
  }
    
  
  questNum<-str_extract(x,"[0-9]+[a-zA-Z]+")
  if(is.na(questNum))
    questNum<-str_extract(x,"^X.[0-9]+") %>% gsub("X","",.) %>% gsub("\\.","",.)
  
  if(is.na(questNum)) 
    questNum<-str_extract(x,"^X[0-9]+") %>% gsub("X","",.)
  
  questText<-gsub("\\."," ", str_extract(x,"\\.\\.[a-zA-Z]+.*")) %>% trimws()
  questOption<-str_extract(x,"OPTSTART.*OPTEND") 
  
  if(!is.na(questOption)){
  questOption <- questOption %>%
    gsub("\\."," ",.) %>%
    gsub("OPTSTART","",.) %>%
    gsub("OPTEND","",.) %>%
    trimws()
  }
  questStore<-rbind(questStore,c(questNum,questText,questOption))
}

#format questStore into a data frame
questStore<-data.frame(questStore)
colnames(questStore)<-c("questNumPart","questText","questOpt")

#Create a question ID, to rename the columns and to link with respondent's other data

questID<-c("ID","Location")
for(i in setdiff(unique(questStore$questNumPart),c("ID","Location"))){
  
  idx<-which(questStore$questNumPart == i)
  
  #taking advantage that questions are ordered consecutively
  questID<-c(questID,paste(paste0("Q",i,collapose=""),1:length(idx),sep="opt"))
}

questStore$questID<-questID


#--- CLASSIFY QUESTION TYPES (FOR FIGURE LATER ON)
#done by manual inspection
onePtIdx<-c("4","5A","7A","9A","10A","11A",,"15A","18","19")
multiOptIdx<-c("1B","2B","2C","3","11B","20A","21A","22A","23A")
rankingIdx<-c("6A","8A","12A","13A","14A","16A","17A","24A")
textIdx<-c("5B","6B","7B","8B","9B","10B","12B","13B","14B","15B","16B","17B","20B","21B","22B","23B","24B")
binIdx<-c("1","2")



#NOW, rename the questOnlyData
dim(questOnly)
colnames(questOnly)<-questID


#The final dataset
#gatherSet<-setdiff(questID,paste("Q1Bopt",1:7,sep=""))

questOnly_rev<-questOnly %>%
  select(-contains("Q1B"),-contains("Q2B"),-contains("Q3"))

questOnly_rev<-tidyr::gather(questOnly_rev,question,response,Q2Copt1:Q24Bopt1)

colnames(questOnly_rev)<-c("ID","Location","workWithTB","workInPH","Question","Response")


#QUESTION1: PROFESSION, COLLAPOSE INTO ONLE COLUMN
temp<-questOnly %>% select(ID,contains("Q1B"))
questAnswer<-filter(questStore, questID %in% setdiff(colnames(temp),"ID")) %>% select(questOpt)

profession<-c()

for(i in 1:nrow(temp)){
  x<-temp[i,2:ncol(temp)]
  
  if(sum(x==1 & !is.na(x))==0){
    profession<-c(profession,"Non-TB")
  }else{
    y<-questAnswer$questOpt[x==1 & !is.na(x)]
    profession<-c(profession,strsplit(as.character(y)," ")[[1]][1])
  }
}

temp<-cbind(as.character(temp$ID),profession)
colnames(temp)<-c("ID","profession") 
temp<-data.frame(temp)

#add new professionv variable
questOnly_rev<-merge(questOnly_rev,temp,by="ID")

#QUESTION2: PROFESSION, COLLAPOSE INTO ONLE COLUMN

temp<-questOnly %>% select(ID,contains("Q2B"))
questAnswer<-filter(questStore, questID %in% setdiff(colnames(temp),"ID")) %>% select(questOpt)

phRole<-c()

for(i in 1:nrow(temp)){
  x<-temp[i,2:ncol(temp)]
  
  if(sum(x==1 & !is.na(x))==0){
    print(i)
    phRole<-c(phRole,"Non-Micro")
  }else{
    y<-questAnswer$questOpt[x==1 & !is.na(x)]
    phRole<-c(phRole,strsplit(as.character(y)," ")[[1]][1])
  }
}

temp<-cbind(as.character(temp$ID),phRole)
colnames(temp)<-c("ID","phRole") 
temp<-data.frame(temp)

#add new professionv variable
questOnly_rev<-merge(questOnly_rev,temp,by="ID")


#QUESTION3: Employer, COLLAPOSE INTO ONLE COLUMN
temp<-questOnly %>% select(ID,contains("Q3"))
questAnswer<-filter(questStore, questID %in% setdiff(colnames(temp),"ID")) %>% select(questOpt)

employer<-c()

for(i in 1:nrow(temp)){
  x<-temp[i,2:ncol(temp)]
  
  if(sum(x==1 & !is.na(x))==0){
    print(i)
    employer<-c(employer,"NS")
  }else{
    y<-questAnswer$questOpt[x==1 & !is.na(x)]
    employer<-c(employer,trimws(strsplit(as.character(y)," e g ")[[1]][1]))
  }
}

employer<-gsub("Other  please specify","Other",employer)

temp<-cbind(as.character(temp$ID),employer)
colnames(temp)<-c("ID","employer") 
temp<-data.frame(temp)

#add new professionv variable
questOnly_rev<-merge(questOnly_rev,temp,by="ID")

#reorder the columns for human reading
questOnly_rev<-questOnly_rev[,c(1,2,3,4,7,8,9,5,6)]

questOnly_rev$phRole<-factor(questOnly_rev$phRole, levels=c(setdiff(levels(questOnly_rev$phRole),"Non-Micro"),"Non-Micro"))

questOnly_rev$profession<-factor(questOnly_rev$profession, levels=c(setdiff(levels(questOnly_rev$phRole),"Non-TB"),"Non-TB"))

questOnly_rev$employer<-gsub("Other  please specify","Other",questOnly_rev$employer)
questOnly_rev$employer<-factor(questOnly_rev$employer, levels=c(setdiff(sort(unique(questOnly_rev$employer)),"Other"),"Other"))


#SAVE DATASETS FOR OTHER ANALYSIS
saveRDS(file="respondentDataSet_Updated.RDS",questOnly_rev)
saveRDS(file="questionDataSet_Updated.RDS",questStore)
