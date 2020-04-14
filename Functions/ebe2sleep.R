ebe2sleep <- function(data=NA,idCol="subject",RefCol="reference",deviceCol="device",epochLenght=30,
                      staging=TRUE,stages=c(wake=0,light=1,deep=2,REM=3),digits=2){
  
  # renaming variables
  colnames(data) <- gsub(idCol,"ID",colnames(data))
  data$ID <- as.factor(data$ID)
  colnames(data) <- gsub(RefCol,"ref",colnames(data))
  colnames(data) <- gsub(deviceCol,"device",colnames(data))
  
  # setting stages as 0 = wake, 1 = light, 2 = deep, 3 = REM
  if(staging==TRUE){ 
    data$ref <- as.integer(as.character(factor(data$ref,levels=as.numeric(stages),labels=c(0,1,2,3))))
    data$device <- as.integer(as.character(factor(data$device,levels=as.numeric(stages),labels=c(0,1,2,3))))
    
  } else { # setting stages as 0 = wake, 1 = sleep when staging = FALSE
    if(length(stages)>2){ stop("only two elements should be used in the stages argument when staging = FALSE,
                               e.g., stages = c(wake = 0, sleep = 1)") }
    data$ref <- as.integer(as.character(factor(data$ref,levels=as.numeric(stages),labels=c(0,1))))
    data$device <- as.integer(as.character(factor(data$device,levels=as.numeric(stages),labels=c(0,1)))) }
  
  # empty dataframe of sleep measures (stages length is not provided when staging = FALSE)
  sleep.metrics <- as.data.frame(matrix(nrow=0,ncol=ifelse(staging==TRUE,22,10)))
  
  for(ID in levels(data$ID)){
    
    sleepID <- data[data$ID==ID,]
    
    # TIB = number of minutes between lights on and lights off
    TIB <- nrow(sleepID)*epochLenght/60
    
    # TST = number of minutes scored as sleep
    TST_ref <- nrow(sleepID[sleepID$ref!=0,])*epochLenght/60
    TST_device <- nrow(sleepID[sleepID$device!=0,])*epochLenght/60
    
    # SE = percentage of sleep time over TIB
    SE_ref <- 100*TST_ref/TIB
    SE_device <- 100*TST_device/TIB
    
    # SOL = number of minutes scored as wake before the first epoch scored as sleep
    SOL_ref = SOL_device = 0
    for(i in 1:nrow(sleepID)){ if(sleepID[i,"ref"]==0){ SOL_ref = SOL_ref + 1 } else { break } }
    for(i in 1:nrow(sleepID)){ if(sleepID[i,"device"]==0){ SOL_device = SOL_device + 1 } else { break } }
    SOL_ref <- SOL_ref*epochLenght/60
    SOL_device <- SOL_device*epochLenght/60
    
    # WASO = number of minutes scored as wake after the first epoch scored as sleep
    WASO_ref <- sleepID[(SOL_ref*60/epochLenght+1):nrow(sleepID),]
    WASO_ref <- nrow(WASO_ref[WASO_ref$ref==0,])*epochLenght/60
    WASO_device <- sleepID[(SOL_device*60/epochLenght+1):nrow(sleepID),]
    WASO_device <- nrow(WASO_device[WASO_device$device==0,])*epochLenght/60
    
    if(staging==TRUE){
      
      # Light = number of minutes scored as Light sleep (N1 + N2)
      Light_ref <- nrow(sleepID[sleepID$ref==1,])*epochLenght/60
      Light_device <- nrow(sleepID[sleepID$device==1,])*epochLenght/60
      
      # Deep = number of minutes scored as Light sleep (N3)
      Deep_ref <- nrow(sleepID[sleepID$ref==2,])*epochLenght/60
      Deep_device <- nrow(sleepID[sleepID$device==2,])*epochLenght/60
      
      # REM = number of minutes scored as REM sleep
      REM_ref <- nrow(sleepID[sleepID$ref==3,])*epochLenght/60
      REM_device <- nrow(sleepID[sleepID$device==3,])*epochLenght/60
      
      # LightPerc = percentage of Light sleep over TST
      LightPerc_ref <- 100*Light_ref/TST_ref
      LightPerc_device <- 100*Light_device/TST_device
      
      # DeepPerc  = percentage of Deep sleep over TST
      DeepPerc_ref <- 100*Deep_ref/TST_ref
      DeepPerc_device <- 100*Deep_device/TST_device
      
      # REMPerc = percentage of REM sleep over TST
      REMPerc_ref <- 100*REM_ref/TST_ref
      REMPerc_device <- 100*REM_device/TST_device
      
      # filling dataframe of sleep metrics
      sleep.metrics <- rbind(sleep.metrics,
                             data.frame(subject=ID,TIB=TIB,
                                        # sleep/wake metrics
                                        TST_ref,TST_device,SE_ref,SE_device,SOL_ref,SOL_device,WASO_ref,WASO_device,
                                        # sleep stages duration metrics
                                        Light_ref,Light_device,Deep_ref,Deep_device,REM_ref,REM_device,
                                        # sleep stages percentages metrics
                                        LightPerc_ref,LightPerc_device,DeepPerc_ref,DeepPerc_device,REMPerc_ref,REMPerc_device))
      
    } else {
      
      sleep.metrics <- rbind(sleep.metrics,
                             data.frame(subject=ID,TIB=TIB,
                                        # sleep/wake metrics
                                        TST_ref,TST_device,SE_ref,SE_device,SOL_ref,SOL_device,WASO_ref,WASO_device))
      
    }
    
  }
  
  # rounding values and returning dataset
  nums <- vapply(sleep.metrics, is.numeric, FUN.VALUE = logical(1))
  sleep.metrics[,nums] <- round(sleep.metrics[,nums], digits = digits)
  return(sleep.metrics) 
  }