indEBE <- function(data=NA,idCol="subject",RefCol="reference",deviceCol="device",doPlot=TRUE,
                   stage=0,stageLabel="wake",digits=2){
  
  # renaming variables
  colnames(data) <- gsub(idCol,"ID",colnames(data))
  colnames(data) <- gsub(RefCol,"ref",colnames(data))
  colnames(data) <- gsub(deviceCol,"device",colnames(data))
  
  # empty dataframe to be filled
  results <- as.data.frame(matrix(nrow=0,ncol=4))
  
  for(ID in levels(as.factor(data$ID))){
    
    d <- data[data$ID==ID,]
    
    # .....................
    # DYCHOTOMIC ENCODING
    # .....................
    
    # reference dychotomous recoding
    d$reference_EBE <- NA
    d[d$ref!=stage,"reference_EBE"] = 0 # other stages = 0
    d[d$ref==stage,"reference_EBE"] = 1 # target stage = 1
    
    # device dychotomous recoding
    d$device_EBE = NA
    d[d$device==stage,"device_EBE"] = 1 # target stage = 1
    d[d$device!=stage,"device_EBE"] = 0 # other stages = 0
    
    # ...................
    # CONFUSION MATRIX
    # ...................
    
    errMatrix <- data.frame(device=c("positive","negative"),
                            ref_positive=c(nrow(d[d$device_EBE==1 & d$reference_EBE==1,]), # true wake
                                           nrow(d[d$device_EBE==0 & d$reference_EBE==1,])), # false sleep
                            ref_negative=c(nrow(d[d$device_EBE==1 & d$reference_EBE==0,]), # false wake
                                           nrow(d[d$device_EBE==0 & d$reference_EBE==0,]))) # true sleep
    
    # ..............
    # EBE MEASURES
    # ..............
    
    results <- rbind(results,
                     data.frame(subject=ID,
                                
                                # accuracy
                                acc=round(100*(errMatrix[1,2]+errMatrix[2,3])/
                                            (errMatrix[1,2]+errMatrix[2,2]+errMatrix[1,3]+errMatrix[2,3]),digits),
                                
                                # sensitivity
                                sens=round(100*errMatrix[1,2]/(errMatrix[1,2]+errMatrix[2,2]),digits),
                                
                                # specificity
                                spec=round(100*errMatrix[2,3]/(errMatrix[2,3]+errMatrix[1,3]),digits))) }
  
  # ..............
  # PLOTTING RESULTS
  # ..............
  
  if(doPlot==TRUE){
    
    require(ggplot2)
    require(reshape2)
    
    melted.results <- melt(results)
    p <- ggplot(data=melted.results,aes(x=variable,y=subject,fill=value)) +
      geom_tile() +
      scale_fill_gradient2(low="#f03b20",high="lightgreen",mid="yellow",
                           limit = c(0,100),midpoint = 50,
                           space = "Lab",
                           name="% discrepancy",
                           guide="legend",
                           breaks=round(seq(100,0,length.out = 11),2),
                           minor_breaks=round(seq(100,0,length.out = 11),2)) +
      geom_text(data=melted.results,aes(x=variable,y=subject,label=value),color="black",size=3) +
      ggtitle(paste("Epoch-by-epoch statistics by subject for",stageLabel)) +
      theme(panel.background = element_blank())
    
    colnames(results)[2:4] <- paste(stageLabel,c("acc","sens","spec")) 
    
    return(list(results,p))
    
  } else { 
    
    colnames(results)[2:4] <- paste(stageLabel,c("acc","sens","spec")) 
    return(results) 
    
  }
  
}