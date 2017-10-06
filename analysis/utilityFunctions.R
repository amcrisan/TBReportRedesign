wrapper <- function(x, ...) 
{
  paste(strwrap(x, ...), collapse = "\n")
}



# CALC RANK

normRank<-function(qData=NULL,temp=NULL,nOpt=NULL,maxScore=NULL,minScore=NULL,type=NULL){
  
  if(type=="RANK"){
    print("RANK")
    # add a normalized score
    qData<-qData %>%
      mutate(rebase = totalScore - minScore) %>%
      mutate(normalized = rebase/(maxScore-minScore)) #baseline based upon the lowest rank
    
    #last thing, what would happen if people just randomly chose options
    #temp2<-responses %>%
    #  filter(Question %in% temp$questID)
    
    temp2<-c()
    for(i in 1:nrow(qData)){
      temp2<-rbind(temp2,cbind(rep(qData$Question[i]),rep(0,minScore)))  
    }
    
    temp2<-data.frame(temp2)
    colnames(temp2)<-c("Question","Filler")
    
    nResponses<-maxScore
    
    #randomized 1000 random scores
    storeScore<-c()
    for(i in 1:1000){
      temp3<-temp2 %>%
        mutate(randomResponse=sample(1:nOpt,nResponses,replace = T)) %>%
        group_by(Question) %>% 
        summarise(randomScore = sum(randomResponse)) %>%
        mutate(randomRebase = randomScore-minScore) %>%
        mutate(randomNormalized = randomRebase/(maxScore-minScore))
      
      storeScore<-rbind(storeScore,temp3)
    }
  }else{
    #set it up slightly differently, so that it's more like the rank questions, expect that
    #the rank is either 0 or 1
    
    countTab<-qData %>%
      group_by(Responses) %>%
      summarise(totalScore = n()) %>%
      complete(Responses, fill = list(totalScore= 0))
    
    maxScore=sum(countTab$totalScore)
    
    # add a normalized score
    countTab<-countTab %>%
      mutate(normalized = totalScore/maxScore) #baseline based upon the lowest rank
    
    nResponses<-nrow(qData)
    responseTypes<-levels(countTab$Responses)
    
    #randomized 1000 random scores
    storeScore<-c()
    for(i in 1:1000){
      temp3<-qData %>%
        mutate(randomResponse=sample(responseTypes,nResponses,replace = T)) %>%
        group_by(randomResponse) %>% 
        summarise(randomScore = n()) %>%
        complete(randomResponse, fill = list(randomScore= 0))%>%
        mutate(randomNormalized = randomScore/maxScore)
      
      storeScore<-rbind(storeScore,temp3)
    }
  }
  
  #basically, if it's random, it get's a normalized rank score around 0.5, we can get a p-value from this distribution too..
  M<-mean(storeScore$randomNormalized)
  S<-sd(storeScore$randomNormalized)
  
  #so because it's normally distributed...
  boxBounds<-rbind(c(0,M-S*3),
                   c(M-S*3,M-S*2),
                   c(M-S*2,M-S*1),
                   c(M-S*1,M),
                   c(M,M+S*1),
                   c(M+S*1,M+S*2),
                   c(M+S*2,M+S*3),
                   c(M+S*3,1))
  
  rndDistQ<-data.frame(xmin=boxBounds[,1],
                       xmax=boxBounds[,2],
                       ymin=rep(0.85,8),
                       ymax=rep(1.15,8),
                       sector=factor(c("3Dev","2Dev","1Dev","M","M","1Dev","2Dev","3Dev"),levels=c("3Dev","2Dev","1Dev","M")))
  
  if(type=="RANK"){
    return(list(qData=qData,rndDistQ=rndDistQ))
  }else{
    return(list(qData=countTab,rndDistQ=rndDistQ))
  }
  
}


# FUNCTION: Get percentage with reference distribution for onechoice and multichoice questions
normGenericVis_mod<-function(cntrl=NULL,responses = NULL,clin=TRUE,sep=TRUE){
  #qData<-responses %>%
  #  filter(Question %in% temp$questID)
  
  temp2<-normRank(qData=responses,type="ONECHOICE")
  qData<-temp2$qData
  rndDistQ<-temp2$rndDistQ
  
  #modify qData to extract the questions of the form: A (Item)
  #There is too little consistency, regExp will fail
  #qData$questOptVal<-sapply(qData$Responses,function(x){
  #  sub("\\(","-",as.character(x)) %>%sub("\\)","",.) %>% strsplit(.,"-")}[[1]][1] %>% trimws())
  
  qData$questOptVal<-letters[1:nrow(qData)] %>% toupper()
  qData$isControl<-factor(ifelse(qData$questOptVal == cntrl, "Yes","No"),levels=c("Yes","No"))
  
  print(qData)
  
  if(clin){
    p<-ggplot(data=qData,aes(x=normalized,y=1))+
      geom_rect(data=rndDistQ,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=sector),alpha=0.2,inherit.aes=F,fill=c(brewer.pal(n=4,name="Oranges"),rev(brewer.pal(n=4,name="Greens")[1:4])))+
      geom_point(colour="black",pch=22,alpha=0.75,aes(fill=isControl),size=6)
  }else{
    p<-ggplot(data=qData,aes(x=normalized,y=1))+
      geom_rect(data=rndDistQ,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=sector),alpha=0.2,inherit.aes=F,fill=c(brewer.pal(n=4,name="Oranges"),rev(brewer.pal(n=4,name="Greens")[1:4])))+
      geom_point(colour="black",pch=21,alpha=0.75,aes(fill=isControl),size=6)
  }
    p<-p+xlab("Percentage Score")+
    geom_text(aes(x=normalized,y=1,label=questOptVal,colour=isControl))+
    ylab("")+
    theme_bw()+
    scale_colour_manual(values=c("black","lightgrey"),limits=c("Yes","No"))+
    scale_fill_manual(values=c("white","black"),limits=c("Yes","No"))+
    #geom_vline(xintercept = rndDistQ[4,]$xmax,colour="darkgrey")+
    xlim(c(0,1))+
    ylim(c(0.85,1.2))+
    theme(legend.position="none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text.y=element_blank(),axis.ticks.y = element_blank())
    
    if(!sep){
      p<- p + geom_text(x=0.75,y=1+0.1,label="Preferred",hjust=0,vjust=0,colour="black")+
        geom_text(x=0.15,y=1+0.1,label="Not Preferred",hjust=0.5,vjust=0,colour="black")
    }
    return(p)
}

# FUNCTION: Get normalized rank score for rank questions

normRankVis_mod<-function(temp=NULL,responses=NULL,nOpt=NULL,maxScore=NULL,minScore=NULL,sep=FALSE,clin=FALSE){

  maxScore<-minScore*nOpt
  
  qData<-responses %>%
    #filter(Question %in% temp$questID) %>%
    mutate(Response = as.numeric(Response)) %>%
    group_by(Question) %>% 
    summarise(totalScore = sum(Response)) %>%
    arrange(totalScore)%>%
    mutate(questionName = plyr::mapvalues(Question,temp$questID, as.character(temp$questOpt))) %>%
    mutate(questOptVal = plyr::mapvalues(Question,temp$questID, as.character(temp$questOptVal))) %>%
    mutate(questOptText = plyr::mapvalues(Question,temp$questID, as.character(temp$questOptText)))
  
  temp2<-normRank(qData=qData,nOpt=nOpt,maxScore=maxScore,minScore=minScore,type="RANK")
  qData<-temp2$qData
  rndDistQ<-temp2$rndDistQ
  
  cntrl<-as.character(unique(temp$controlOpt))
  
  qData$isControl<-factor(ifelse(qData$questOptVal == cntrl, "Yes","No"),levels=c("Yes","No"))
  
  if(clin){
    p<-ggplot(data=qData,aes(x=abs(1-normalized),y=1))+
      geom_rect(data=rndDistQ,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=sector),alpha=0.2,inherit.aes=F,fill=c(brewer.pal(n=4,name="Oranges"),rev(brewer.pal(n=4,name="Greens")[1:4])))+
      geom_point(colour="black",pch=22,alpha=0.75,aes(fill=isControl),size=6)
  }else{
    p<-ggplot(data=qData,aes(x=abs(1-normalized),y=1))+
      geom_rect(data=rndDistQ,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=sector),alpha=0.2,inherit.aes=F,fill=c(brewer.pal(n=4,name="Oranges"),rev(brewer.pal(n=4,name="Greens")[1:4])))+
      geom_point(colour="black",pch=21,alpha=0.75,aes(fill=isControl),size=6)
  }
  p<- p +
    geom_text(aes(x=abs(1-normalized),y=1,label=questOptVal,colour=isControl))+
    scale_fill_manual(values=c("white","black"),limits=c("Yes","No"))+
    scale_colour_manual(values=c("black","lightgrey"),limits=c("Yes","No"))+
    xlab("Normalized Rank Score")+
    ylab("")+
    theme_bw()+
    xlim(c(0,1))+
    ylim(c(0.85,1.2))+
    theme(legend.position="none",panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text.y=element_blank(),axis.ticks.y = element_blank())
  
  if(!sep){
    p<- p + geom_text(x=0.75,y=1+0.1,label="Preferred",hjust=0,vjust=0,colour="black")+
      geom_text(x=0.15,y=1+0.1,label="Not Preferred",hjust=0.5,vjust=0,colour="black")
  }
  return(p)
}


# FUNCTION: Plot Clinicans and Non-clinicans seperately
normRankVisSplit_mod<-function(temp=NULL,responses=NULL,nOpt=NULL,title=NULL,manyStacked=TRUE,qType=NULL){
  
  #---- Derive Data for clinicians ---
  qData<-responses %>%
    filter(Question %in% temp$questID) %>%
    mutate(Responses = factor(Response)) %>% #this is mainly for the non-rank stuff
    filter(clinician=="Physician")
  
  
  if(qType=="RANK"){
    clin<-normRankVis_mod(temp = temp,responses=qData,nOpt=nOpt,minScore=nrow(qData)/nOpt,clin=T,sep=manyStacked)
  }else{
    cntrl<-as.character(unique(temp$controlOpt))
    clin<-normGenericVis_mod(cntrl=cntrl,responses = qData,clin=T,sep=manyStacked)
  }
  
  #---- Derive Data for non-clinicians ---
  qData<-responses %>%
    filter(Question %in% temp$questID) %>%
    mutate(Responses = factor(Response)) %>% #this is mainly for the non-rank stuff
    filter(clinician=="Non-Physician")
  
  if(qType=="RANK"){
    nonClin<-normRankVis_mod(temp = temp,responses=qData,nOpt=nOpt,minScore=nrow(qData)/nOpt,clin=F,sep=manyStacked)
  }else{
    cntrl<-as.character(unique(temp$controlOpt))
    nonClin<-normGenericVis_mod(cntrl=cntrl,responses = qData,clin=F,sep=manyStacked)
  }
  
  #---- Stack these plots on top of each other ---
 
  # Get rid of labels for clinician plot (will go on top)
   clin <- clin + 
    ylab("")+
    xlab("")
  
  # Modify the edge boundries, so plots are closer together
  clin <- clin + theme(plot.margin=unit(c(-0.95,0,0,0),"cm"))
    
  nonClin <- nonClin + 
    ylab("")+
    theme(plot.margin=unit(c(-0.95,0,0,0),"cm"))

  # If there's a bunch of graph, don't print out the x-axis
  if(manyStacked){
    nonClin <- nonClin + xlab("") + 
      theme(axis.ticks.x = element_blank())
  }

  if(!is.null(title)){
    sep<-plot_grid(clin,nonClin,ncol=1, align="v")
    title<-ggdraw() + draw_label(title)
    sep<-plot_grid(title,sep,ncol=2,rel_widths=c(0.1,1))
  }else{
  #return the stacked plots
    sep<-plot_grid(clin,nonClin,ncol=1, align="v")
  }

  return(sep)
}




#--------------------- vv OLDER STUFF vv ---------------------#

rankVis<-function(temp=NULL,responses=NULL){
  title<-sapply(temp$questText,function(x){
    strsplit(as.character(x),"OPTSTART")[[1]][1] %>%
      trimws()
  }) %>% unique()
  
  qData<-responses %>%
    filter(Question %in% temp$questID)
  
  
  all<-ggplot(qData,aes(x=Response,group=Question)) +
    geom_bar(aes(fill=Question),position="dodge")+
    geom_text(aes(label = ..count..,y = ..count.. + 0.5), stat= "count",position = position_dodge(0.9))+
    facet_grid(.~Response,scale="free_x")+
    ggtitle(wrapper(title, width = 75))+
    scale_fill_brewer(palette = "Set2",labels=unique(as.character(temp$questOpt)),name="Choice")+
    theme_bw()+
    theme(plot.title=element_text(size=12,face="bold",hjust=0))
  
  
  sep<-ggplot(qData,aes(x=Response,group=Question)) +
    geom_bar(aes(fill=Question),position="dodge")+  
    geom_text(aes(label = ..count..,y = ..count.. + 0.75), stat= "count",position = position_dodge(0.9))+
    facet_grid(clinician~Response,scale="free_x")+
    ggtitle(wrapper(title, width = 75))+
    scale_fill_brewer(palette = "Set2",labels=unique(as.character(temp$questOpt)),name="Choice")+
    theme_bw()+
    theme(plot.title=element_text(size=12,face="bold",hjust=0))
  
  return(list(all=all,sep=sep))

}

normRankVis<-function(temp=NULL,responses=NULL,nOpt=NULL,minScore=NULL,sep=FALSE,clin=FALSE){
  
  #------- FOR ALL DATA TOGETHER ------
  maxScore<-minScore*nOpt
  
  qData<-responses %>%
    filter(Question %in% temp$questID) %>%
    mutate(Response = as.numeric(Response)) %>%
    group_by(Question) %>% 
    summarise(totalScore = sum(Response)) %>%
    arrange(totalScore)%>%
    mutate(questionName = plyr::mapvalues(Question,temp$questID, as.character(temp$questOpt))) %>%
    mutate(questOptVal = plyr::mapvalues(Question,temp$questID, as.character(temp$questOptVal))) %>%
    mutate(questOptText = plyr::mapvalues(Question,temp$questID, as.character(temp$questOptText)))
  
  # add a normalized score
  qData<-qData %>%
    mutate(rebase = totalScore - minScore) %>%
    mutate(normalized = rebase/(maxScore-minScore)) #baseline based upon the lowest rank
  
  #last thing, what would happen if people just randomly chose options
  temp2<-responses %>%
    filter(Question %in% temp$questID)
  
  nResponses<-nrow(temp2) 
  
  #randomized 1000 random scores
  storeScore<-c()
  for(i in 1:1000){
    temp3<-temp2 %>%
      mutate(randomResponse=sample(1:nOpt,nResponses,replace = T)) %>%
      group_by(Question) %>% 
      summarise(randomScore = sum(randomResponse)) %>%
      mutate(randomRebase = randomScore-minScore) %>%
      mutate(randomNormalized = randomRebase/(maxScore-minScore))
    
    storeScore<-rbind(storeScore,temp3)
  }
  
  #basically, if it's random, it get's a normalized rank score around 0.5, we can get a p-value from this distribution too..
  M<-mean(storeScore$randomNormalized)
  S<-sd(storeScore$randomNormalized)
  
  #so because it's normally distributed...
  boxBounds<-rbind(c(0,M-S*3),
                   c(M-S*3,M-S*2),
                   c(M-S*2,M-S*1),
                   c(M-S*1,M),
                   c(M,M+S*1),
                   c(M+S*1,M+S*2),
                   c(M+S*2,M+S*3),
                   c(M+S*3,1))

  rndDistQ<-data.frame(xmin=boxBounds[,1],
                       xmax=boxBounds[,2],
                       ymin=rep(0.85,8),
                       ymax=rep(1.15,8),
                       sector=factor(c("3Dev","2Dev","1Dev","M","M","1Dev","2Dev","3Dev"),levels=c("3Dev","2Dev","1Dev","M")))
  
  # SPREAD OUT VERSION  
  #
  # p<-ggplot(data=qData,aes(x=normalized,y=questionName))+
  #   #geom_point(aes(fill=questionName),pch=21,colour="black",size=5)+
  #   geom_point(aes(fill=questionName),pch=21,colour="black",size=5)+
  #   #geom_rect(data=rndDistQ,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=sector),alpha=0.2,inherit.aes=F,fill=c(brewer.pal(n=4,name="Greens"),rev(brewer.pal(n=4,name="Oranges")[1:4])))+
  #   geom_rect(data=rndDistQ,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=sector),alpha=0.2,inherit.aes=F,fill=c(brewer.pal(n=4,name="Greys"),rev(brewer.pal(n=4,name="Greys")[1:4])))+
  #   xlab("Normalized Rank Score")+
  #   ylab("Design Choice Option")+
  #   geom_text(x=0.01,y=nOpt+0.35,label="Most Favoured",hjust=0,vjust=0,colour="grey")+
  #   geom_text(x=0.87,y=nOpt+0.35,label="Least Favoured",hjust=0.5,vjust=0,colour="grey")+
  #   theme_bw()+
  #   xlim(c(0,1))+
  #   theme(legend.position="None")  
  
  #DENSE VERION

    if(clin){
      p<-ggplot(data=qData,aes(x=abs(1-normalized),y=1))+
        geom_rect(data=rndDistQ,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=sector),alpha=0.2,inherit.aes=F,fill=c(brewer.pal(n=4,name="Oranges"),rev(brewer.pal(n=4,name="Greens")[1:4])))+
        #geom_rect(data=rndDistQ,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=sector),alpha=0.2,inherit.aes=F,fill=c(brewer.pal(n=4,name="Greys"),rev(brewer.pal(n=4,name="Greys")[1:4])))+
        geom_point(fill="black",pch=22,alpha=0.75,colour="black",size=6)
    }else{
      p<-ggplot(data=qData,aes(x=abs(1-normalized),y=1))+
        geom_rect(data=rndDistQ,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=sector),alpha=0.2,inherit.aes=F,fill=c(brewer.pal(n=4,name="Oranges"),rev(brewer.pal(n=4,name="Greens")[1:4])))+
        #geom_rect(data=rndDistQ,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=sector),alpha=0.2,inherit.aes=F,fill=c(brewer.pal(n=4,name="Greys"),rev(brewer.pal(n=4,name="Greys")[1:4])))+
        geom_point(fill="black",pch=21,alpha=0.75,colour="black",size=6)
    }
    p<- p +
    geom_text(aes(x=abs(1-normalized),y=1,label=questOptVal),colour="white")+
    xlab("Normalized Rank Score")+
    ylab("")+
    theme_bw()+
    xlim(c(0,1))+
    ylim(c(0.85,1.2))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.text.y=element_blank(),axis.ticks.y = element_blank())
    
    if(!sep){
      p<- p + geom_text(x=0.75,y=1+0.1,label="Preferred",hjust=0,vjust=0,colour="black")+
         geom_text(x=0.15,y=1+0.1,label="Not Preferred",hjust=0.5,vjust=0,colour="black")
    }
  
  
  return(list(all=p))
  
}

normRankVisSplit<-function(temp=NULL,responses=NULL,nOpt=NULL,title=NULL,manyStacked=TRUE,first=FALSE){
 
  #----- Clinicians
  
  qData<-responses %>%
    filter(Question %in% temp$questID) %>%
    filter(clinician=="Physician")
  
  if(manyStacked){
    clin<-normRankVis(temp=temp,responses = qData,nOpt=nOpt,minScore=nrow(qData)/nOpt,clin=T,sep=T)$all
  }else{
    clin<-normRankVis(temp=temp,responses = qData,nOpt=nOpt,minScore=nrow(qData)/nOpt,clin=T)$all
  }
  #---- Non-clincians
  
  qData<-responses %>%
    filter(Question %in% temp$questID) %>%
    filter(clinician=="Non-Physician")
  
  nonClin<-normRankVis(temp=temp,responses = qData,nOpt=nOpt,minScore=nrow(qData)/nOpt,sep=T)$all
  
  #---- Putting it all together
  
  clin <- clin + 
    ylab("")+
    xlab("")
  
  if(first){
    clin <- clin + theme(plot.margin=unit(c(1,0,0,0),"cm"))
    
    nonClin <- nonClin + 
      ylab("")+
      theme(plot.margin=unit(c(-0.95,0,0,0),"cm"))
  }else{
    clin <- clin + theme(plot.margin=unit(c(-0.95,0,0,0),"cm"))
    
    nonClin <- nonClin + 
      ylab("")+
      theme(plot.margin=unit(c(-0.95,0,0,0),"cm"))
  }
  
  if(!manyStacked){
      clin<-clin+theme(axis.line.x = element_line(size=1))
  }
  
  
  
  
  if(manyStacked){
    nonClin <- nonClin + xlab("")
  } else{
    nonClin <- nonClin + xlab("Normalized Rank Score")
  }
  
  if(is.null(title)){
    sep<-plot_grid(clin,nonClin,ncol=1, align="v")
    
    #sep<-grid.arrange(clin, nonClin, ncol = 1,top = "Preferences by Physician (square)/Non-Physician (circle)")
    #grid.rect(width = .98, height = .98, gp = gpar(lwd = 2, col = "blue"))
  }
  else{
    sep<-plot_grid(clin,nonClin,ncol=1, align="v")
    #sep<-grid.arrange(clin, nonClin, nrow=2,ncol = 1,heights=unit(c(0.25,0.25),"in"),left = title)
    #grid.rect(width = .98, height = .98, gp = gpar(lwd = 2, col = "blue"))
  }
  return(list(sep=sep))
}



#Figures that are just a basic bar plot
genericPlot<-function(temp=NULL, responses=NULL){
    title<-sapply(temp$questText,function(x){
      strsplit(as.character(x),"OPTSTART")[[1]][1] %>%
        trimws()
    }) %>% unique()
    
    qData<-responses %>%
      filter(Question %in% temp$questID)
    
    qData$Response <- factor(qData$Response)
    
    all<-ggplot(qData,aes(x=Response,group=Question)) +
      geom_bar(position="dodge")+
      geom_text(aes(label = ..count..,y = ..count.. + 0.5), stat= "count",position = position_dodge(0.9))+
      ggtitle(wrapper(title, width = 75))+
      theme_bw()+
      theme(plot.title=element_text(size=12,face="bold",hjust=0))
    
    
    sep<-ggplot(qData,aes(x=Response,group=Question)) +
      geom_bar(position="dodge")+  
      geom_text(aes(label = ..count..,y = ..count.. + 0.75), stat= "count",position = position_dodge(0.9))+
      ggtitle(wrapper(title, width = 75))+
      facet_grid(clinician~.,scale="free_x")+
      theme_bw()+
      theme(plot.title=element_text(size=12,face="bold",hjust=0))
    
    #---- CALCULATE THE PERCENTAGE SCORE ----#
    
     countTab<-qData %>%
        group_by(Response) %>%
        summarise(totalScore = n())

    
    # add a normalized score
   countTab<-countTab %>%
      mutate(normalized = totalScore/maxScore) #baseline based upon the lowest rank
    
   nResponses<-nrow(qData)
   responseTypes<-countTab$Response
  
    #randomized 1000 random scores
    storeScore<-c()
    for(i in 1:1000){
      temp3<-qData %>%
        mutate(randomResponse=sample(responseTypes,nResponses,replace = T)) %>%
        group_by(randomResponse) %>% 
        summarise(randomScore = n()) %>%
        mutate(randomPer = randomScore/maxScore)
      
      storeScore<-rbind(storeScore,temp3)
    }
    
    #basically, if it's random, it get's a normalized rank score around 0.5, we can get a p-value from this distribution too..
    M<-mean(storeScore$randomPer)
    S<-sd(storeScore$randomPer)
    
    #so because it's normally distributed...
    boxBounds<-rbind(c(0,M-S*3),
                     c(M-S*3,M-S*2),
                     c(M-S*2,M-S*1),
                     c(M-S*1,M),
                     c(M,M+S*1),
                     c(M+S*1,M+S*2),
                     c(M+S*2,M+S*3),
                     c(M+S*3,1))
    
    rndDistQ<-data.frame(xmin=boxBounds[,1],
                         xmax=boxBounds[,2],
                         ymin=rep(0.85,8),
                         ymax=rep(1.15,8),
                         sector=factor(c("3Dev","2Dev","1Dev","M","M","1Dev","2Dev","3Dev"),levels=c("3Dev","2Dev","1Dev","M")))
    
    p<-ggplot(data=countTab,aes(x=normalized,y=1))+
      geom_rect(data=rndDistQ,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,fill=sector),alpha=0.2,inherit.aes=F,fill=c(brewer.pal(n=4,name="Oranges"),rev(brewer.pal(n=4,name="Greens")[1:4])))+
      geom_point(fill="black",pch=22,alpha=0.75,colour="black",size=6)+
      #geom_text(aes(x=abs(1-normalized),y=1,label=questOptVal),colour="white")+
      xlab("Normalized Rank Score")+
      ylab("")+
      theme_bw()+
      xlim(c(0,1))+
      ylim(c(0.85,1.2))+
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.border = element_blank(),
            axis.text.y=element_blank(),axis.ticks.y = element_blank())
    
    
    
    return(list(all=all,sep=sep))
}

#Figures for mutlti option plots
multiPlot<-function(temp=NULL,responses=NULL,nested=FALSE){
  title<-sapply(temp$questText,function(x){
    strsplit(as.character(x),"OPTSTART")[[1]][1] %>%
      trimws()
  }) %>% unique()
  
  qData<-responses %>%
    filter(Question %in% temp$questID) 

  if(!nested){
    qData[!is.na(qData$Response),]$Response<-qData[!is.na(qData$Response),]$Question
    
    qData$Response <-plyr::mapvalues(as.character(qData$Response),
                            from=as.character(temp$questID),
                            to = as.character(temp$questOpt))
    
    #temp2<-with(qData,table(QuestionOpt,Response))
    #qData$QuestionOpt<-factor(qData$QuestionOpt,levels=rownames(temp2)[order(temp2,decreasing = TRUE)])
    
  }else{
    #for the nested questions
    qData$Response<-factor(qData$Response,levels=c("Strongly Disagree",
                                                   "Disagree","Neutral","Agree",
                                                   "Strongly Agree"))
  }
    qData$Question<-plyr::mapvalues(as.character(qData$Question),
                                    from=as.character(temp$questID),
                                    to = as.character(temp$questOpt))
  
  
  all<-ggplot(qData,aes(x=Response,group=Question))+
    geom_bar(aes(fill=Response),position="dodge")+
    geom_text(aes(label = ..count..,y = ..count.. + 0.5), stat= "count",position = position_dodge(0.9))+
    ggtitle(wrapper(title, width = 75))+
    scale_fill_brewer(palette = "Set2",name="Choice")+
    theme_bw()+
    theme(plot.title=element_text(size=12,face="bold",hjust=0))
  
  
  sep<-ggplot(qData,aes(x=Response,group=Question))+
    scale_fill_brewer(palette = "Set2",name="Choice")+
    theme_bw()+
    theme(plot.title=element_text(size=12,face="bold",hjust=0))
  
  numOptChoose<-with(qData,table(Response,ID)) %>% table()
  
  if(nested){
    all<-all + facet_wrap(~Question,ncol=1)+scale_x_discrete(drop=FALSE)
    sep<-sep + geom_bar(aes(fill=clinician))+
      #geom_text(aes(label = ..count..,y = ..count.. + 0.5), stat= "count",position = position_dodge(0.9))+
      ggtitle(wrapper(title, width = 75))+
      facet_wrap(~Question,ncol=1)+
      scale_x_discrete(drop=FALSE)
    numOptChoose<-NULL
  }else{
    sep<-sep + geom_bar(aes(fill=Response),position="dodge")+
      geom_text(aes(label = ..count..,y = ..count.. + 0.5), stat= "count",position = position_dodge(0.9))+
      ggtitle(wrapper(title, width = 75))+
      facet_grid(clinician~.,scale="free_x")+
      theme(axis.text.x=element_blank())
    
    all<-all+theme(axis.text.x=element_blank())
  }

  
  return(list(all=all,sep=sep,numOptChoose=numOptChoose))
}