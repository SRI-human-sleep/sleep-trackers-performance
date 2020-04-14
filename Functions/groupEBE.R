groupEBE <- function(data=NA,idCol="subject",RefCol="reference",deviceCol="device",
                     stage=0,stageLabel="wake",metricsType="avg",CI.type="classic",CI.level=.95,boot.type="basic",boot.R=10000,
                     advancedMetrics=FALSE,doROC=FALSE,digits=2){
  
  # renaming variables
  colnames(data) <- gsub(idCol,"ID",colnames(data))
  colnames(data) <- gsub(RefCol,"ref",colnames(data))
  colnames(data) <- gsub(deviceCol,"device",colnames(data))
  
  # ............................
  # DYCHOTOMIC RECODING
  # ............................
  
  # reference dychotomous recoding
  data$ref_EBE <- NA
  data[data$ref!=stage,"ref_EBE"] = 0 # other stages = 0
  data[data$ref==stage,"ref_EBE"] = 1 # target stage = 1
  
  # device dychotomous recoding
  data$device_EBE = NA
  data[data$device==stage,"device_EBE"] = 1 # target stage = 1
  data[data$device!=stage,"device_EBE"] = 0 # other stages = 0
  
  generateAccMetrics <- function(data,advancedMetrics){
    
    # ............................
    # CONFUSION MATRIX
    # ............................
    
    errMatrix <- data.frame(device=c("positive","negative"),
                            ref_positive=c(nrow(data[data$device_EBE==1 & data$ref_EBE==1,]), # true wake
                                           nrow(data[data$device_EBE==0 & data$ref_EBE==1,])), # false sleep
                            ref_negative=c(nrow(data[data$device_EBE==1 & data$ref_EBE==0,]), # false wake
                                           nrow(data[data$device_EBE==0 & data$ref_EBE==0,]))) # true sleep
    
    # ............................
    # BASIC EBE METRICS
    # ............................
    
    # accuracy
    accuracy <- round(100*(errMatrix[1,2]+errMatrix[2,3])/
                        (errMatrix[1,2]+errMatrix[2,2]+errMatrix[1,3]+errMatrix[2,3]),digits)
    # sensitivity
    sensitivity <- round(100*errMatrix[1,2]/(errMatrix[1,2]+errMatrix[2,2]),digits)
    # specificity
    specificity <- round(100*errMatrix[2,3]/(errMatrix[2,3]+errMatrix[1,3]),digits)
    
    # ............................
    # ADDITIONAL EBE METRICS
    # ............................
    
    if(advancedMetrics==TRUE){
      
      require(epiR)
      
      # prevalence
      prevalence <- round(100*(errMatrix[1,2]+errMatrix[2,2])/
                            (errMatrix[1,2]+errMatrix[2,2]+errMatrix[1,3]+errMatrix[2,3]),2)
      # positive predictive value
      ppv <- round(100*(sensitivity*prevalence)/
                     (sensitivity*prevalence + (1-specificity)*(1-prevalence)),digits)
      # negative predictive value
      npv <- round(100*(specificity*(1-prevalence))/
                     ((1-specificity)*prevalence + specificity*(1-prevalence)),digits)
      # Cohen's kappa
      kappa <- round(epi.kappa(as.table(as.matrix(errMatrix[1:2,2:3])))$kappa$est,digits)
      # PABAK
      pabak <- round(epi.kappa(as.table(as.matrix(errMatrix[1:2,2:3])))$pabak$est,digits)
      # bias index
      bindex <- round(epi.kappa(as.table(as.matrix(errMatrix[1:2,2:3])))$bindex$est,digits)
      # prevalence index
      pindex <- round(epi.kappa(as.table(as.matrix(errMatrix[1:2,2:3])))$pindex$est,digits)
      
      # creating dataframe with basic and advanced metrics
      results <- data.frame(stage=stageLabel,
                            accuracy=accuracy,sensitivity=sensitivity,specificity=specificity,
                            ppv=ppv,npv=npv,kappa=kappa,pabak=pabak,pindex=pindex)
      
    } else { # only basic metrics
      results <- data.frame(stage=stageLabel,
                            accuracy=accuracy,sensitivity=sensitivity,specificity=specificity) } 
    
    return(results)
  }
  
  # generating metrics from absolute error matrix
  if(metricsType=="sum"){ 
    results <- generateAccMetrics(data,advancedMetrics)
    
    # generating metrics from averaged error matrix    
  } else if(metricsType=="avg"){
    # empty dataframe to be filled with individual EBE metrics
    results <- as.data.frame(matrix(nrow=0,ncol=ncol(generateAccMetrics(data,advancedMetrics))))
    for(ID in levels(as.factor(data$ID))){
      results <- rbind(results,
                       data.frame(ID,generateAccMetrics(data[data$ID==ID,],advancedMetrics))) 
    }
    
    # function to print EBE metrics info as "mean (sd) [CI]"
    printInfo <- function(results,metric){
      require(DescTools)
      m <- round(mean(results[,metric],na.rm=TRUE),digits)
      s <- round(sd(results[,metric],na.rm=TRUE),digits)
      if(CI.type=="classic"){ require(DescTools)
        CI.lo <- round(MeanCI(results[,metric],method=CI.type,btype=boot.type,R=boot.R)[2],digits)
        CI.up <- round(MeanCI(results[,metric],method=CI.type,btype=boot.type,R=boot.R)[3],digits)
      } else if (CI.type=="boot"){ library(boot)
        CI.lo <- round(boot.ci(boot(results[,metric],
                                    function(dat,idx)mean(dat[idx],na.rm=TRUE),R=boot.R),
                               type=boot.type,conf=CI.level)[[4]][4],digits)
        CI.up <- round(boot.ci(boot(results[,metric],
                                    function(dat,idx)mean(dat[idx],na.rm=TRUE),R=boot.R),
                               type=boot.type,conf=CI.level)[[4]][5],digits)
      } else { stop("Error: CI.type can be either 'classic' or 'boot'") }
      paste(m," (",s,") [",CI.lo,", ",CI.up,"]",sep="") }
    
    # creating and filling data.frame of EBE metrics information
    out <- as.data.frame(matrix(nrow=0,ncol=ncol(results)-2))
    for(i in 1:ncol(out)){ out[1,i] <- printInfo(results,colnames(results)[i+2]) } 
    colnames(out) <- colnames(results)[3:ncol(results)]
    results <- cbind(stage=stageLabel,out)
    
  } else { stop("Error: metricsType argument can be either 'avg' or 'sum'") }
  
  # ROC curves
  if(doROC==TRUE){
    
    require(ROCR)
    p <- plot(performance(prediction(data$device_EBE,data$ref_EBE),"tpr","fpr"),lwd=2,
              main=paste(stageLabel,"- ROC curves"),xlab="1-specificity",ylab="sensitivity")
    abline(0,1,col="darkgray")
    res <- list(results,p)
  }
  
  return(results)
  
}