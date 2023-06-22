errorMatrix <- function(data=NA,idCol="subject",RefCol="reference",deviceCol="device",
                        staging=TRUE,stages=c(wake=0,light=1,deep=2,REM=3),matrixType="prop",
                        CI.type="classic",CI.level=.95,boot.type="basic",boot.R=10000,digits=2){
  
  # renaming variables
  colnames(data) <- gsub(idCol,"ID",colnames(data))
  colnames(data) <- gsub(RefCol,"ref",colnames(data))
  colnames(data) <- gsub(deviceCol,"device",colnames(data))
  
  # function to print error info as "mean (sd) [CI]"
  printInfo <- function(condition){
    m <- round(mean(errMatrix[,condition],na.rm=TRUE),digits)
    s <- round(sd(errMatrix[,condition],na.rm=TRUE),digits)
    if(CI.type=="classic"){ require(DescTools)
      CI.up <- round(MeanCI(errMatrix[,condition],method="classic")[3],digits)
      CI.lo <- round(MeanCI(errMatrix[,condition],method="classic")[2],digits)
    } else if(CI.type=="boot"){ require(boot)
      CI.lo <- round(boot.ci(boot(errMatrix[,condition],
                                  function(dat,idx)mean(dat[idx],na.rm=TRUE),R=boot.R),
                             type=boot.type,conf=CI.level)[[4]][4],digits)
      CI.up <- round(boot.ci(boot(errMatrix[,condition],
                                  function(dat,idx)mean(dat[idx],na.rm=TRUE),R=boot.R),
                             type=boot.type,conf=CI.level)[[4]][5],digits)
    } else { stop("Error: CI.type can be either 'classic' or 'boot'") }
    if(CI.lo < 0) CI.lo = 0 # bounding at zero
    paste(m," (",s,") [",CI.lo,", ",CI.up,"]",sep="") }
  
  # .................................
  # sleep staging is available
  # .................................
  
  if(staging==TRUE){
    
    # setting stages as 0 = wake, 1 = light, 2 = deep, 3 = REM
    data$ref <- as.integer(as.character(factor(data$ref,levels=as.numeric(stages),labels=c(0,1,2,3))))
    data$device <- as.integer(as.character(factor(data$device,levels=as.numeric(stages),labels=c(0,1,2,3))))
    
    # function to generate error matrix
    generateMatrix <- function(data,matrixType){
      # empty dataframe for error matrix
      errMatrix <- as.data.frame(matrix(nrow=0,ncol=5))
      for(i in 0:3){
        # dividend based on matrix type
        h <- ifelse(matrixType=="prop",nrow(data[data$ref==i,]),1)
        # filling matrix with number (sum) or proportion (prop) of epochs in each condition
        errMatrix <- rbind(errMatrix,
                           data.frame(reference=names(stages)[i+1],
                                      device_wake=nrow(data[data$device==0 & data$ref==i,])/h,
                                      device_light=nrow(data[data$device==1 & data$ref==i,])/h,
                                      device_deep=nrow(data[data$device==2 & data$ref==i,])/h,
                                      device_REM=nrow(data[data$device==3 & data$ref==i,])/h,
                                      reference_tot=nrow(data[data$ref==i,]))) }
      return(rbind(errMatrix,
                   # adding marginal sums
                   data.frame(reference="device_tot",device_wake=nrow(data[data$device==0,]),
                              device_light=nrow(data[data$device==1,]),device_deep=nrow(data[data$device==2,]),
                              device_REM=nrow(data[data$device==3,]),reference_tot=nrow(data))))
    }
    
    # generating absolute error matrix
    if(matrixType=="sum"){ 
      errMatrix <- generateMatrix(data,matrixType)
      
      # generating averaged proportional error matrix 
    } else if(matrixType=="prop"){
      # empty dataframe for error matrix
      errMatrix <- as.data.frame(matrix(nrow=0,ncol=17))
      # on matrix per subject (in wide form)
      for(ID in levels(as.factor(data$ID))){
        errMatrix <- rbind(errMatrix,
                           reshape(data=cbind(ID=rep(ID,length(stages)),
                                              generateMatrix(data[data$ID==ID,],matrixType)[1:4,1:5]),
                                   idvar="ID",v.names=paste("device",c("wake","light","deep","REM"),sep="_"),
                                   timevar="reference",direction="wide")) }
      
      # using printInfo for reporting mean, sd, and CI
      errMatrix <- data.frame(reference=c("wake","light","deep","REM"),
                              device_wake=unlist(lapply(colnames(errMatrix)[substr(colnames(errMatrix),
                                                                                   8,10)=="wak"],printInfo)),
                              device_light=unlist(lapply(colnames(errMatrix)[substr(colnames(errMatrix),
                                                                                    8,10)=="lig"],printInfo)),
                              device_deep=unlist(lapply(colnames(errMatrix)[substr(colnames(errMatrix),
                                                                                   8,10)=="dee"],printInfo)),
                              device_REM=unlist(lapply(colnames(errMatrix)[substr(colnames(errMatrix),
                                                                                  8,10)=="REM"],printInfo)))
      
    } else { stop("Error: matrixType argument can be either 'prop' or 'sum'") }
    
    # .................................
    # sleep staging is *not* available
    # .................................
    
  } else {
    
    # setting stages
    data$ref <- as.integer(as.character(factor(data$ref,levels=as.numeric(stages),labels=c(0,1))))
    data$device <- as.integer(as.character(factor(data$device,levels=as.numeric(stages),labels=c(0,1))))
    
    # function to generate error matrix
    generateMatrix <- function(data,matrixType){
      # empty dataframe for error matrix
      errMatrix <- as.data.frame(matrix(nrow=0,ncol=3))
      for(i in 0:1){
        # dividend based on matrix type
        h <- ifelse(matrixType=="prop",nrow(data[data$ref==i,]),1)
        # filling matrix with number (sum) or proportions (prop) of epochs in each condition
        errMatrix <- rbind(errMatrix,
                           data.frame(reference=names(stages)[i+1],
                                      device_wake=nrow(data[data$device==0 & data$ref==i,])/h,
                                      device_sleep=nrow(data[data$device==1 & data$ref==i,])/h,
                                      reference_tot=nrow(data[data$ref==i,]))) }
      return(rbind(errMatrix,
                   # adding marginal sums
                   data.frame(reference="device_tot",device_wake=nrow(data[data$device==0,]),
                              device_sleep=nrow(data[data$device==1,]),reference_tot=nrow(data))))
    }
    
    # generating absolute error matrix
    if(matrixType=="sum"){
      errMatrix <- generateMatrix(data,matrixType)
      
      # generating averaged error matrix 
    } else if(matrixType=="prop"){
      errMatrix <- as.data.frame(matrix(nrow=0,ncol=5))
      for(ID in levels(as.factor(data$ID))){
        errMatrix <- rbind(errMatrix,
                           reshape(data=cbind(ID=rep(ID,length(stages)),
                                              generateMatrix(data[data$ID==ID,],matrixType)[1:2,1:3]),
                                   idvar="ID",v.names=paste("device",c("wake","sleep"),sep="_"),
                                   timevar="reference",direction="wide"))
      }
      
      errMatrix <- data.frame(reference=c("wake","sleep"),
                              device_wake=unlist(lapply(colnames(errMatrix)[substr(colnames(errMatrix),
                                                                                   8,10)=="wak"],printInfo)),
                              device_sleep=unlist(lapply(colnames(errMatrix)[substr(colnames(errMatrix),
                                                                                    8,10)=="sle"],printInfo)))
      
    } else { cat('Error: matrixType argument can be either "sum" or "prop"')}
    
  }
  
  return(errMatrix)
  
}
