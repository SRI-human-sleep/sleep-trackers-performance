groupDiscr <- function(data=NA,measures=c("TST_device","TST_ref"),size="reference",logTransf=FALSE,
                       CI.type="classic",CI.level=.95,boot.type="basic",boot.R=10000,digits=2,warnings=TRUE){
  
  require(BlandAltmanLeh)
  
  # setting measure name and unit of measurement
  measure <- gsub("_ref","",gsub("_device","",measures[1]))
  if(any(grepl("efficiency|EFFICIENCY|se|SE|eff|EFF|perc|Perc|PERC|%",measure))){
    meas.unit <- "%" } else { meas.unit <- "min" }
  if(warnings==TRUE){cat("\n\n----------------\n Measure:",measure,"\n----------------")}
  
  # packages and functions to be used with boostrap CI
  if(CI.type=="boot"){ 
    # functions to generate bootstrap CI for model parameters
    boot.reg <- function(data,formula,indices){ return(coef(lm(formula,data=data[indices,]))[2]) } # slope
    boot.int <- function(data,formula,indices){ return(coef(lm(formula,data=data[indices,]))[1]) } # intercept
    boot.res <- function(data,formula,indices){return(1.96*sd(resid(lm(formula,data=data[indices,]))))} # 1.96 SD of residuals
    if(warnings==TRUE){cat("\n\nComputing boostrap CI with method '",boot.type,"' ...",sep="")}
  } else if(CI.type!="classic") { stop("Error: CI.type can be either 'classic' or 'boot'") }
  
  # data to be used
  ba.stat <- bland.altman.stats(data[,measures[1]],data[,measures[2]],conf.int=CI.level)
  if(size=="reference"){
    ba <- data.frame(size=ba.stat$groups$group2,diffs=ba.stat$diffs)
    xlab <- paste("Reference-derived",measure)
  } else if(size=="mean"){
    ba <- data.frame(size=ba.stat$means,diffs=ba.stat$diffs)
    xlab <- paste("Mean",measure,"by device and reference")
  } else { stop("Error: size argument can be either 'reference' or 'mean'") }
  
  # basic output table
  out <- data.frame(measure=paste(measure," (",meas.unit,")",sep=""),
                    
                    # mean and standard deviation for ref and device
                    device = paste(round(mean(ba.stat$groups$group1,na.rm=TRUE),digits)," (",
                                   round(sd(ba.stat$groups$group1,na.rm=TRUE),digits),")",sep=""),
                    reference = paste(round(mean(ba.stat$groups$group2,na.rm=TRUE),digits)," (",
                                      round(sd(ba.stat$groups$group2,na.rm=TRUE),digits),")",sep=""),
                    
                    # CI type
                    CI.type=CI.type,
                    CI.level=CI.level)
  
  # ..........................................
  # 1. TESTING PROPORTIONAL BIAS
  # ..........................................
  m <- lm(diffs~size,ba)
  if(CI.type=="classic"){ CI <- confint(m,level=CI.level)[2,] 
  } else { CI <- boot.ci(boot(data=ba,statistic=boot.reg,formula=diffs~size,R=boot.R),
                         type=boot.type,conf=CI.level)[[4]][4:5] }
  prop.bias <- ifelse(CI[1] > 0 | CI[2] < 0, TRUE, FALSE)
  
  # updating output table
  out <- cbind(out,prop.bias=prop.bias)
  
  # ...........................................
  # 1.1. DIFFERENCES INDEPENDENT FROM SIZE
  # ...........................................
  if(prop.bias == FALSE){
    
    if(CI.type=="boot"){ # changing bias CI when CI.type="boot"
      ba.stat$CI.lines[3] <- boot.ci(boot(ba$diffs,function(dat,idx)mean(dat[idx],na.rm=TRUE),R=boot.R),
                                     type=boot.type,conf=CI.level)[[4]][4]
      ba.stat$CI.lines[4] <- boot.ci(boot(ba$diffs,function(dat,idx)mean(dat[idx],na.rm=TRUE),R=boot.R),
                                     type=boot.type,conf=CI.level)[[4]][5] }
    
    out <- cbind(out, # updating output table
                 # bias (SD) [CI]
                 bias=paste(round(ba.stat$mean.diffs,digits)," (",round(sd(ba.stat$diffs),digits),")",sep=""),
                 bias_CI=paste("[",round(ba.stat$CI.lines[3],digits),", ",round(ba.stat$CI.lines[4],digits),"]",sep=""))
    
    # ..........................................
    # 1.2. DIFFERENCES PROPORTIONAL TO SIZE
    # ..........................................
  } else { 
    
    b0 <- coef(m)[1]
    b1 <- coef(m)[2]
    
    # warning message
    if(warnings==TRUE){cat("\n\nWARNING: differences in ",measure," might be proportional to the size of measurement (coeff. = ",
        round(b1,2)," [",round(CI[1],2),", ",round(CI[2],2),"]",").",
        "\nBias and LOAs are represented as a function of the size of measurement.",sep="")}
    
    # intercept CI
    if(CI.type=="classic"){ CInt <- confint(m,level=CI.level)[1,]
    } else { CInt <- boot.ci(boot(data=ba,statistic=boot.int,formula=diffs~size,R=boot.R),
                             type=boot.type,conf=CI.level)[[4]][4:5] }
    
    out <- cbind(out, # updating output table
                 
                 # bias and CI following Bland & Altman (1999): D = b0 + b1 * size
                 bias=paste(round(b0,digits)," + ",round(b1,digits)," x ",ifelse(size=="mean","mean","ref"),sep=""),
                 bias_CI=paste("b0 = [",round(CInt[1],digits),", ",round(CInt[2],digits),
                               "], b1 = [",round(CI[1],digits),", ",round(CI[2],digits),"]",sep="")) }
  
  # ..............................................
  # 2. LOAs ESTIMATION FROM ORIGINAL DATA
  # ..............................................
  if(logTransf == FALSE){
    
    # testing heteroscedasticity
    mRes <- lm(abs(resid(m))~size,ba)
    if(CI.type=="classic"){ CIRes <- confint(mRes,level=CI.level)[2,]
    } else { CIRes <- boot.ci(boot(data=ba,statistic=boot.reg,formula=abs(resid(m))~size,R=boot.R),
                              type=boot.type,conf=CI.level)[[4]][4:5] }
    heterosced <- ifelse(CIRes[1] > 0 | CIRes[2] < 0,TRUE,FALSE)
    
    # testing normality of differences
    shapiro <- shapiro.test(ba.stat$diffs)
    if(shapiro$p.value <= .05){ normality = FALSE
    if(warnings==TRUE){cat("\n\nWARNING: differences in ",measure,
        " might be not normally distributed (Shapiro-Wilk W = ",round(shapiro$statistic,3),", p = ",round(shapiro$p.value,3),
        ").","\nBootstrap CI (CI.type='boot') and log transformation (logTransf=TRUE) are recommended.",sep="")} } else { normality = TRUE }
    
    # updating output table
    out <- cbind(out[,1:6],logTransf=FALSE,normality=normality,heterosced=heterosced,out[,7:ncol(out)])
    
    # ...............................................
    # 2.1. CONSTANT BIAS AND HOMOSCEDASTICITY
    # ............................................
    if(prop.bias==FALSE & heterosced==FALSE){
      
      if(CI.type=="boot"){ # changing LOAs CI when CI.type="boot"
        ba.stat$CI.lines[1] <- boot.ci(boot(ba$diffs-1.96*sd(ba.stat$diffs),
                                            function(dat,idx)mean(dat[idx],na.rm=TRUE),R=boot.R),
                                       type=boot.type,conf=CI.level)[[4]][4]
        ba.stat$CI.lines[2] <- boot.ci(boot(ba$diffs-1.96*sd(ba.stat$diffs),
                                            function(dat,idx)mean(dat[idx],na.rm=TRUE),R=boot.R),
                                       type=boot.type,conf=CI.level)[[4]][5]
        ba.stat$CI.lines[5] <- boot.ci(boot(ba$diffs+1.96*sd(ba.stat$diffs),
                                            function(dat,idx)mean(dat[idx],na.rm=TRUE),R=boot.R),
                                       type=boot.type,conf=CI.level)[[4]][4]
        ba.stat$CI.lines[6] <- boot.ci(boot(ba$diffs+1.96*sd(ba.stat$diffs),
                                            function(dat,idx)mean(dat[idx],na.rm=TRUE),R=boot.R),
                                       type=boot.type,conf=CI.level)[[4]][5] }
      
      out <- cbind(out, # updating output table
                   
                   # lower LOA and CI
                   LOA.lower = ba.stat$lower.limit,
                   LOA.lower_CI = paste("[",round(ba.stat$CI.lines[1],2),", ",round(ba.stat$CI.lines[2],digits),"]",sep=""),
                   
                   # upper LOA and CI
                   LOA.upper = ba.stat$upper.limit,
                   LOA.upper_CI = paste("[",round(ba.stat$CI.lines[5],2),", ",round(ba.stat$CI.lines[6],digits),"]",sep=""))
      
      # ...............................................
      # 2.2. PROPORTIONAL BIAS AND HOMOOSCEDASTICITY
      # ...............................................
    } else if(prop.bias==TRUE & heterosced==FALSE) { 
      
      # boostrapped CI in any case
      if(warnings==TRUE){cat("\n\nLOAs CI are computed using boostrap with method '",boot.type,"'.",sep="")}
      
      require(boot)
      
      boot.res <- function(data,formula,indices){return(1.96*sd(resid(lm(formula,data=data[indices,]))))} # 1.96 SD of residuals
      CI.sdRes <- boot.ci(boot(data=ba,statistic=boot.res,formula=diffs~size,R=boot.R),
                          type=boot.type,conf=CI.level)[[4]][4:5]
      
      out <- cbind(out, # updating output table
                   
                   # lower LOA and CI following Bland & Altman (1999): LOAs = bias - 1.96sd of the residuals
                   LOA.lower = paste("bias -",round(1.96*sd(resid(m)),digits)),
                   LOA.lower_CI = paste("bias - [",round(CI.sdRes[1],digits),", ",round(CI.sdRes[2],digits),"]",sep=""),
                   
                   # upper LOA and CI following Bland & Altman (1999): LOAs = bias + 1.96sd of the residuals
                   LOA.upper = paste("bias +",round(1.96*sd(resid(m)),digits)),
                   LOA.upper_CI =paste("bias + [",round(CI.sdRes[1],digits),", ",round(CI.sdRes[2],digits),"]",sep="")) 
      
      # ............................................
      # 2.3. HETEROSCEDASTICITY
      # ............................................
    } else if(heterosced==TRUE){
      
      c0 <- coef(mRes)[1]
      c1 <- coef(mRes)[2]
      
      # warning message
      if(warnings==TRUE){cat("\n\nWARNING: standard deviation of differences in ",measure,
          " might be proportional to the size of measurement (coeff. = ",
          round(c1,digits)," [",round(CIRes[1],digits),", ",round(CIRes[2],digits),"]",").",
          "\nLOAs range is represented as a function of the size of measurement.",sep="")}
      
      # intercept CI
      if(CI.type=="classic"){ CInt <- confint(mRes,level=CI.level)[1,]
      } else { CInt <- boot.ci(boot(data=ba,statistic=boot.int,formula=abs(resid(m))~size,R=boot.R),
                               type=boot.type,conf=CI.level)[[4]][4:5] }
      
      out <- cbind(out, # updating output table
                   
                   # lower LOA and CI following Bland & Altman (1999): LOAs = meanDiff - 2.46(c0 + c1A)
                   LOA.lower = paste("bias - 2.46(",round(c0,digits)," + ",round(c1,digits)," x ",
                                     ifelse(size=="mean","mean","ref"),")",sep=""),
                   LOA.lower_CI = paste("c0 = [",round(CInt[1],digits),", ",round(CInt[2],digits),
                                        "], c1 = [",round(CIRes[1],digits),", ",round(CIRes[2],digits),"]",sep=""),
                   
                   # upper LOA and CI following Bland & Altman (1999): LOAs = meanDiff - 2.46(c0 + c1A)
                   LOA.upper = paste("bias + 2.46(",round(c0,digits)," + ",round(c1,digits)," x ",
                                     ifelse(size=="mean","mean","ref"),")",sep=""),
                   LOA.upper_CI = paste("c0 = [",round(CInt[1],digits),", ",round(CInt[2],digits),
                                        "], c1 = [",round(CIRes[1],digits),", ",round(CIRes[2],digits),"]",sep="")) }
    
    # ..............................................
    # 3. LOAs ESTIMATION FROM LOG-TRANSFORMED DATA
    # ..............................................
  } else {
    
    # log transformation of data (add little constant to avoid Inf values)
    if(warnings==TRUE){cat("\n\nLog transforming the data before computing LOAs...")}
    ba.stat$groups$LOGgroup1 <- log(ba.stat$groups$group1 + .0001)
    ba.stat$groups$LOGgroup2 <- log(ba.stat$groups$group2 + .0001)
    ba.stat$groups$LOGdiff <- ba.stat$groups$LOGgroup1 - ba.stat$groups$LOGgroup2
    if(size=="reference"){ baLog <- data.frame(size=ba.stat$groups$LOGgroup2,diffs=ba.stat$groups$LOGdiff)
    } else { baLog <- data.frame(size=(ba.stat$groups$LOGgroup1 + ba.stat$groups$LOGgroup2)/2,diffs=ba.stat$groups$LOGdiff) }
    
    # testing heteroscedasticity
    mRes <- lm(abs(resid(m))~size,baLog)
    if(CI.type=="classic"){ CIRes <- confint(mRes,level=CI.level)[2,]
    } else { CIRes <- boot.ci(boot(data=baLog,statistic=boot.reg,formula=abs(resid(m))~size,R=boot.R),
                              type=boot.type,conf=CI.level)[[4]][4:5] }
    heterosced <- ifelse(CIRes[1] > 0 | CIRes[2] < 0,TRUE,FALSE)
    
    # testing normality of differences
    shapiro <- shapiro.test(baLog$diffs)
    if(shapiro$p.value <= .05){ normality = FALSE
    if(warnings==TRUE){cat("\n\nWARNING: differences in log transformed ",measure,
        " might be not normally distributed (Shapiro-Wilk W = ",round(shapiro$statistic,3),", p = ",round(shapiro$p.value,3),
        ").","\nBootstrap CI (CI.type='boot') are recommended.",sep="")} } else { normality = TRUE }
    
    # updating output table
    out <- cbind(out[,1:6],logTransf=FALSE,normality=normality,heterosced=heterosced,out[,7:ncol(out)])
    
    # LOAs slope following Euser et al (2008) for antilog transformation: slope = 2 * (e^(1.96 SD) - 1)/(e^(1.96 SD) + 1)
    ANTILOGslope <- function(x){ 2 * (exp(1.96 * sd(x)) - 1) / (exp(1.96*sd(x)) + 1) }
    ba.stat$LOA.slope <- ANTILOGslope(baLog$diffs)
    
    # LOAs CI slopes 
    if(CI.type=="classic"){ # classic CI
      t1 <- qt((1 - CI.level)/2, df = ba.stat$based.on - 1) # t-value right
      t2 <- qt((CI.level + 1)/2, df = ba.stat$based.on - 1) # t-value left
      ba.stat$LOA.slope.CI.upper <- 2 * (exp(1.96 * sd(baLog$diffs) + t2 * sqrt(sd(baLog$diffs)^2 * 3/ba.stat$based.on)) - 1) /
        (exp(1.96*sd(baLog$diffs) + t2 * sqrt(sd(baLog$diffs)^2 * 3/ba.stat$based.on)) + 1)
      ba.stat$LOA.slope.CI.lower <- 2 * (exp(1.96 * sd(baLog$diffs) + t1 * sqrt(sd(baLog$diffs)^2 * 3/ba.stat$based.on)) - 1) /
        (exp(1.96*sd(baLog$diffs) + t1 * sqrt(sd(baLog$diffs)^2 * 3/ba.stat$based.on)) + 1)
    } else { # boostrap CI
      ba.stat$LOA.slope.CI.lower <- boot.ci(boot(baLog$diffs,function(dat,idx) ANTILOGslope(dat[idx]),R=boot.R),
                                            type=boot.type,conf=CI.level)[[4]][4]
      ba.stat$LOA.slope.CI.upper <- boot.ci(boot(baLog$diffs,function(dat,idx) ANTILOGslope(dat[idx]),R=boot.R),
                                            type=boot.type,conf=CI.level)[[4]][5] }
    
    # LOAs depending on mean and size (regardless if bias is proportional or not)
    out <- cbind(out, # updating output table
                 
                 # lower LOA and CI
                 LOA.lower = paste("bias - ",size," x ",round(ba.stat$LOA.slope,digits)),
                 LOA.lower_CI = paste("bias - ",size," x [",round(ba.stat$LOA.slope.CI.lower,digits),
                                      ", ",round(ba.stat$LOA.slope.CI.upper,digits),"]",sep=""),
                 
                 # upper LOA and CI
                 LOA.upper = paste("bias + ",size," x ",round(ba.stat$LOA.slope,digits)),
                 LOA.upper_CI = paste("bias + ",size," x [",round(ba.stat$LOA.slope.CI.lower,digits),
                                      ", ",round(ba.stat$LOA.slope.CI.upper,digits),"]",sep="")) }
  
  
  # rounding values
  nums <- vapply(out, is.numeric, FUN.VALUE = logical(1))
  out[,nums] <- round(out[,nums], digits = digits)
  out[,nums] <- as.character(out[,nums]) # to prevent NA values when combined with other cases
  row.names(out) <- NULL
  
  return(out)
  
}