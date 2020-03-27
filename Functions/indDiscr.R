indDiscr <- function(data=NA,staging=TRUE,digits=2,doPlot=TRUE){
  
  indivDiscr <- data.frame(subject=data$subject,
                           TST_diff = data$TST_device - data$TST_ref, # TST
                           SE_diff = data$SE_device - data$SE_ref, # SE
                           SOL_diff = data$SOL_device - data$SOL_ref, # SOL
                           WASO_diff = data$WASO_device - data$WASO_ref) # WASO
  
  if(staging==TRUE){
    
    indivDiscr <- cbind(indivDiscr,
                        data.frame(
                          
                          # adding sleep stages duration
                          Light_diff = data$Light_device - data$Light_ref,
                          Deep_diff = data$Deep_device - data$Deep_ref,
                          REM_diff = data$REM_device - data$REM_ref,
                          
                          # adding sleep stages percentages
                          LightPerc_diff = data$LightPerc_device - data$LightPerc_ref,
                          DeepPerc_diff = data$DeepPerc_device - data$DeepPerc_ref,
                          REMPerc_diff = data$REMPerc_device - data$REMPerc_ref
                          
                        ))
    
  }
  
  # rounding values
  nums <- vapply(indivDiscr, is.numeric, FUN.VALUE = logical(1))
  indivDiscr[,nums] <- round(indivDiscr[,nums], digits = digits)
  
  if(doPlot==TRUE){
    
    require(ggplot2) 
    require(reshape2)
    
    # normalizing data by column to have color code
    melted.perc <- absDiscr <- indivDiscr
    absDiscr[,2:ncol(absDiscr)] <- abs(absDiscr[,2:ncol(absDiscr)])
    for(i in 1:nrow(melted.perc)){ for(j in 2:ncol(melted.perc)){ 
      melted.perc[i,j] <- 100*(absDiscr[i,j] - min(absDiscr[,j]))/(max(absDiscr[,j])-min(absDiscr[,j]))
    } }
    melted.perc <- melt(melted.perc[,!grepl("diffPerc",colnames(melted.perc))])
    melted.perc$variable <- gsub("Perc","%",gsub("_diff","",melted.perc$variable))
    
    melted.labs <- melt(indivDiscr[,!grepl("diffPerc",colnames(indivDiscr))])
    melted.labs$variable <- gsub("Perc","%",gsub("_diff","",melted.labs$variable))
    
    # sorting axis labels
    if(staging == TRUE){ sleep.levels <- c("TST","SE","WASO","SOL","Light","Deep","REM","Light%","Deep%","REM%")
    } else { sleep.levels <- c("TST","SE","WASO","SOL") }
    
    # generating plot
    p <- ggplot(data=melted.perc,aes(x=ordered(variable,levels=sleep.levels),
                                     y=ordered(subject,levels=rev(levels(subject))),fill=value)) +
      geom_tile() + xlab("variable") + ylab("subject") +
      scale_fill_gradient2(low="white",high="#f03b20",
                           limit = c(0,100),
                           space = "Lab",
                           name="Min-max normalized \ndiscrepancy (%)",
                           guide="legend",
                           breaks=round(seq(100,0,length.out = 11),2),
                           minor_breaks=round(seq(100,0,length.out = 11),2)) +
      geom_text(data=melted.labs,aes(x=ordered(variable,levels=sleep.levels),
                                     y=ordered(subject,levels=rev(levels(subject))),label=value),color="black",size=3) +
      ggtitle("Discrepancies between device and ref by subject") + theme(panel.background = element_blank())
    
    return(list(indivDiscr,p))
    
  } else { return(indivDiscr) }
  
}