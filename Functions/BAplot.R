BAplot <- function(data=NA,measures=c("TST_device","TST_ref"),logTransf=FALSE,
                   xaxis="reference",CI.type="classic",CI.level=.95,boot.type="basic",boot.R=10000,
                   xlim=NA,ylim=NA){
  
  require(BlandAltmanLeh); require(ggplot2); require(ggExtra)
  
  # setting labels
  Measure <- gsub("_ref","",gsub("_device","",measures[1]))
  measure <- gsub("TST","total sleep time (min)",Measure)
  measure <- gsub("SE","sleep efficiency (%)",measure)
  measure <- gsub("SOL","sleep onset latency (min)",measure)
  measure <- gsub("WASO","wake after sleep onset (min)",measure)
  measure <- gsub("LightPerc","light sleep percentage (%)",measure)
  measure <- gsub("DeepPerc","deep sleep percentage (%)",measure)
  measure <- gsub("REMPerc","REM sleep percentage (%)",measure)
  measure <- gsub("Light","light sleep duration (min)",measure)
  measure <- gsub("Deep","deep sleep duration (min)",measure)
  if(grepl("REM",measure) & !grepl("%",measure)) { measure <- gsub("REM","REM sleep duration (min)",measure) }
  cat("\n\n----------------\n Measure:",Measure,"\n----------------")
  
  # packages and functions to be used with boostrap CI
  if(CI.type=="boot"){ require(boot)
    # function to generate bootstrap CI for model parameters
    boot.reg <- function(data,formula,indices){ return(coef(lm(formula,data=data[indices,]))[2]) }
    # function for sampling and predicting Y values based on model
    boot.pred <- function(data,formula,tofit) { indices <- sample(1:nrow(data),replace = TRUE)
    return(predict(lm(formula,data=data[indices,]), newdata=data.frame(tofit))) }
    cat("\n\nComputing boostrap CI with method '",boot.type,"' ...",sep="")
  } else if(CI.type!="classic") { stop("Error: CI.type can be either 'classic' or 'boot'") }
  
  # data to be used
  ba.stat <- bland.altman.stats(data[,measures[1]],data[,measures[2]],conf.int=CI.level)
  if(xaxis=="reference"){
    ba <- data.frame(size=ba.stat$groups$group2,diffs=ba.stat$diffs)
    xlab <- paste("Reference",measure)
  } else if(xaxis=="mean"){
    ba <- data.frame(size=ba.stat$means,diffs=ba.stat$diffs)
    xlab <- paste("Mean",measure,"by device and reference")
  } else { stop("Error: xaxis argument can be either 'reference' or 'mean'") }
  
  # range of values to be fitted for drawing the lines (i.e., from min to max of x-axis values, by .1)
  size <- seq(min(ba$size),max(ba$size),(max(ba$size)-min(ba$size))/((max(ba$size)-min(ba$size))*10))
  
  # basic plot
  p <- ggplot(data=ba,aes(size,diffs))
  
  # ..........................................
  # 1. TESTING PROPORTIONAL BIAS
  # ..........................................
  m <- lm(diffs~size,ba)
  if(CI.type=="classic"){ CI <- confint(m,level=CI.level)[2,] 
  } else { CI <- boot.ci(boot(data=ba,statistic=boot.reg,formula=diffs~size,R=boot.R),
                         type=boot.type,conf=CI.level)[[4]][4:5] }
  prop.bias <- ifelse(CI[1] > 0 | CI[2] < 0, TRUE, FALSE)
  
  # ...........................................
  # 1.1. DIFFERENCES INDEPENDENT FROM SIZE
  # ...........................................
  if(prop.bias == FALSE){ 
    
    if(CI.type=="boot"){ # changing bias CI when CI.type="boot"
      ba.stat$CI.lines[3] <- boot.ci(boot(ba$diffs,function(dat,idx)mean(dat[idx],na.rm=TRUE),R=boot.R),
                                     type=boot.type,conf=CI.level)[[4]][4]
      ba.stat$CI.lines[4] <- boot.ci(boot(ba$diffs,function(dat,idx)mean(dat[idx],na.rm=TRUE),R=boot.R),
                                     type=boot.type,conf=CI.level)[[4]][5] }
    
    p <- p + # adding lines to plot
      
      # bias and CI (i.e., mean diff)
      geom_line(aes(y=ba.stat$mean.diffs),colour="red",size=1.5) +
      geom_line(aes(y=ba.stat$CI.lines[3]),colour="red",linetype=2,size=1) +
      geom_line(aes(y=ba.stat$CI.lines[4]),colour="red",linetype=2,size=1)
    
    # ..........................................
    # 1.2. DIFFERENCES PROPORTIONAL TO SIZE
    # ..........................................
  } else {
    
    b0 <- coef(m)[1]
    b1 <- coef(m)[2]
    
    # warning message
    cat("\n\nWARNING: differences in ",Measure," might be proportional to the size of measurement (coeff. = ",
        round(b1,2)," [",round(CI[1],2),", ",round(CI[2],2),"]",").",
        "\nBias and LOAs are plotted as a function of the size of measurement.",sep="")
    
    # modeling bias following Bland & Altman (1999): D = b0 + b1 * size
    y.fit <- data.frame(size,y.bias=b0+b1*size) 
    
    # bias CI
    if(CI.type=="classic"){ # classic ci
      y.fit$y.biasCI.upr <- predict(m,newdata=data.frame(y.fit$size),interval="confidence",level=CI.level)[,3]
      y.fit$y.biasCI.lwr <- predict(m,newdata=data.frame(y.fit$size),interval="confidence",level=CI.level)[,2]
    } else { # boostrap CI 
      fitted <- t(replicate(boot.R,boot.pred(ba,"diffs~size",y.fit))) # sampling CIs
      y.fit$y.biasCI.upr <- apply(fitted,2,quantile,probs=c((1-CI.level)/2))
      y.fit$y.biasCI.lwr <- apply(fitted,2,quantile,probs=c(CI.level+(1-CI.level)/2)) }
    
    p <- p + # adding lines to plot
      
      # bias and CI (i.e., D = b0 + b1 * size)
      geom_line(data=y.fit,aes(y=y.bias),colour="red",size=1.5) +
      geom_line(data=y.fit,aes(y=y.biasCI.upr),colour="red",linetype=2,size=1) +
      geom_line(data=y.fit,aes(y=y.biasCI.lwr),colour="red",linetype=2,size=1) }
  
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
    shapiro <- shapiro.test(ba$diffs)
    if(shapiro$p.value <= .05){
      cat("\n\nWARNING: differences in ",Measure,
          " might be not normally distributed (Shapiro-Wilk W = ",round(shapiro$statistic,3),", p = ",round(shapiro$p.value,3),
          ").","\nBootstrap CI (CI.type='boot') and log transformation (logTransf=TRUE) are recommended.",sep="") }
    
    # ............................................
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
      
      p <- p + # adding lines to plot
        
        # Upper LOA and CI (i.e., mean diff + 1.96 SD)
        geom_line(aes(y=ba.stat$upper.limit),colour="darkgray",size=1.3) +
        geom_line(aes(y=ba.stat$CI.lines[5]),colour="darkgray",linetype=2,size=1) +
        geom_line(aes(y=ba.stat$CI.lines[6]),colour="darkgray",linetype=2,size=1) +
        
        # Lower LOA and CI (i.e., mean diff - 1.96 SD)
        geom_line(aes(y=ba.stat$lower.limit),colour="darkgray",size=1.3) +
        geom_line(aes(y=ba.stat$CI.lines[1]),colour="darkgray",linetype=2,size=1) +
        geom_line(aes(y=ba.stat$CI.lines[2]),colour="darkgray",linetype=2,size=1)
      
      # ............................................
      # 2.2. PROPORTIONAL BIAS AND HOMOSCEDASTICITY
      # ............................................
    } else if(prop.bias==TRUE & heterosced==FALSE) { 
      
      # modeling LOAs following Bland & Altman (1999): LOAs = bias +- 1.96sd of the residuals
      y.fit$y.LOAu = b0+b1*size + 1.96*sd(resid(m))
      y.fit$y.LOAl = b0+b1*size - 1.96*sd(resid(m))
      
      # LOAs CI based on bias CI +- 1.96sd of the residuals
      cat(" Note that LOAs CI are represented based on bias CI.")
      y.fit$y.LOAu.upr = y.fit$y.biasCI.upr + 1.96*sd(resid(m))
      y.fit$y.LOAu.lwr = y.fit$y.biasCI.lwr + 1.96*sd(resid(m))
      y.fit$y.LOAl.upr = y.fit$y.biasCI.upr - 1.96*sd(resid(m))
      y.fit$y.LOAl.lwr = y.fit$y.biasCI.lwr - 1.96*sd(resid(m))
      
      # ............................................
      # 2.3. CONSTANT BIAS AND HOMOSCEDASTICITY
      # ............................................
    } else if(prop.bias==FALSE & heterosced==TRUE) {
      
      c0 <- coef(mRes)[1]
      c1 <- coef(mRes)[2]
      
      # warning message
      cat("WARNING: SD of differences in ",Measure,
          " might be proportional to the size of measurement (coeff. = ",
          round(c1,2)," [",round(CIRes[1],2),", ",round(CIRes[2],2),"]",").",
          "\nLOAs range is plotted as a function of the size of measurement.",sep="")
      
      # modeling LOAs following Bland & Altman (1999): LOAs = meanDiff +- 2.46(c0 + c1A)
      y.fit <- data.frame(size=size,
                          y.LOAu = ba.stat$mean.diffs + 2.46*(c0+c1*size),
                          y.LOAl = ba.stat$mean.diffs - 2.46*(c0+c1*size))
      
      # LOAs CI
      if(CI.type=="classic"){ # classic ci
        fitted <- predict(mRes,newdata=data.frame(y.fit$size),interval="confidence",level=CI.level) # based on mRes
        y.fit$y.LOAu.upr <- ba.stat$mean.diffs + 2.46*fitted[,3]
        y.fit$y.LOAu.lwr <- ba.stat$mean.diffs + 2.46*fitted[,2]
        y.fit$y.LOAl.upr <- ba.stat$mean.diffs - 2.46*fitted[,3]
        y.fit$y.LOAl.lwr <- ba.stat$mean.diffs - 2.46*fitted[,2]
      } else { # boostrap CI
        fitted <- t(replicate(boot.R,boot.pred(ba,"abs(resid(lm(diffs ~ size))) ~ size",y.fit)))
        y.fit$y.LOAu.upr <- ba.stat$mean.diffs + 2.46*apply(fitted,2,quantile,probs=c(CI.level+(1-CI.level)/2))
        y.fit$y.LOAu.lwr <- ba.stat$mean.diffs + 2.46*apply(fitted,2,quantile,probs=c((1-CI.level)/2))
        y.fit$y.LOAl.upr <- ba.stat$mean.diffs - 2.46*apply(fitted,2,quantile,probs=c(CI.level+(1-CI.level)/2))
        y.fit$y.LOAl.lwr <- ba.stat$mean.diffs - 2.46*apply(fitted,2,quantile,probs=c((1-CI.level)/2)) }
      
      # ............................................
      # 2.4. PROPORTIONAL BIAS AND HETEROSCEDASTICITY
      # ............................................ 
    } else if(prop.bias==TRUE & heterosced==TRUE) {
      
      c0 <- coef(mRes)[1]
      c1 <- coef(mRes)[2]
      
      # warning message
      cat("\n\nWARNING: SD of differences in ",Measure,
          " might be proportional to the size of measurement (coeff. = ",
          round(c1,2)," [",round(CIRes[1],2),", ",round(CIRes[2],2),"]",").",
          "\nLOAs range is plotted as a function of the size of measurement.",sep="")
      
      # modeling LOAs following Bland & Altman (1999): LOAs = b0 + b1 * size +- 2.46(c0 + c1A) 
      y.fit$y.LOAu = b0+b1*size + 2.46*(c0+c1*size)
      y.fit$y.LOAl = b0+b1*size - 2.46*(c0+c1*size)
      
      # LOAs CI
      if(CI.type=="classic"){ # classic ci
        fitted <- predict(mRes,newdata=data.frame(y.fit$size),interval="confidence",level=CI.level) # based on mRes
        y.fit$y.LOAu.upr <- b0+b1*size + 2.46*fitted[,3]
        y.fit$y.LOAu.lwr <- b0+b1*size + 2.46*fitted[,2]
        y.fit$y.LOAl.upr <- b0+b1*size - 2.46*fitted[,3]
        y.fit$y.LOAl.lwr <- b0+b1*size - 2.46*fitted[,2] 
      } else { # boostrap CI
        fitted <- t(replicate(boot.R,boot.pred(ba,"abs(resid(lm(diffs ~ size))) ~ size",y.fit)))
        y.fit$y.LOAu.upr <- b0+b1*size + 2.46*apply(fitted,2,quantile,probs=c(CI.level+(1-CI.level)/2))
        y.fit$y.LOAu.lwr <- b0+b1*size + 2.46*apply(fitted,2,quantile,probs=c((1-CI.level)/2))
        y.fit$y.LOAl.upr <- b0+b1*size - 2.46*apply(fitted,2,quantile,probs=c(CI.level+(1-CI.level)/2))
        y.fit$y.LOAl.lwr <- b0+b1*size - 2.46*apply(fitted,2,quantile,probs=c((1-CI.level)/2)) }}
    
    if(prop.bias==TRUE | heterosced==TRUE){
      
      p <- p + # adding lines to plot
        
        # Upper LOA and CI
        geom_line(data=y.fit,aes(y=y.LOAu),colour="darkgray",size=1.3) +
        geom_line(data=y.fit,aes(y=y.LOAu.upr),colour="darkgray",linetype=2,size=1) +
        geom_line(data=y.fit,aes(y=y.LOAu.lwr),colour="darkgray",linetype=2,size=1) +
        
        # Lower LOA and CI
        geom_line(data=y.fit,aes(y=y.LOAl),colour="darkgray",size=1.3) +
        geom_line(data=y.fit,aes(y=y.LOAl.upr),colour="darkgray",linetype=2,size=1) +
        geom_line(data=y.fit,aes(y=y.LOAl.lwr),colour="darkgray",linetype=2,size=1)
      
    }
    
    # ..............................................
    # 3. LOAs ESTIMATION FROM LOG-TRANSFORMED DATA
    # ..............................................
  } else {
    
    # log transformation of data (add little constant to avoid Inf values)
    cat("\n\nLog transforming data ...")
    ba.stat$groups$LOGgroup1 <- log(ba.stat$groups$group1 + .0001)
    ba.stat$groups$LOGgroup2 <- log(ba.stat$groups$group2 + .0001)
    ba.stat$groups$LOGdiff <- ba.stat$groups$LOGgroup1 - ba.stat$groups$LOGgroup2
    if(xaxis=="reference"){ baLog <- data.frame(size=ba.stat$groups$LOGgroup2,diffs=ba.stat$groups$LOGdiff)
    } else { baLog <- data.frame(size=(ba.stat$groups$LOGgroup1 + ba.stat$groups$LOGgroup2)/2,diffs=ba.stat$groups$LOGdiff) }
    
    # testing heteroscedasticity
    mRes <- lm(abs(resid(m))~size,baLog)
    if(CI.type=="classic"){ CIRes <- confint(mRes,level=CI.level)[2,]
    } else { CIRes <- boot.ci(boot(data=baLog,statistic=boot.reg,formula=abs(resid(m))~size,R=boot.R),
                              type=boot.type,conf=CI.level)[[4]][4:5] }
    heterosced <- ifelse(CIRes[1] > 0 | CIRes[2] < 0,TRUE,FALSE)
    
    # testing normality of differences
    shapiro <- shapiro.test(baLog$diffs)
    if(shapiro$p.value <= .05){
      cat("\n\nWARNING: differences in log transformed ",Measure,
          " might be not normally distributed (Shapiro-Wilk W = ",round(shapiro$statistic,3),", p = ",round(shapiro$p.value,3),
          ").","\nBootstrap CI (CI.type='boot') are recommended.",sep="") }
    
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
      ba.stat$LOA.slope.CI.upper <- boot.ci(boot(baLog$diffs,
                                                 function(dat,idx) ANTILOGslope(dat[idx]),R=boot.R),
                                            type=boot.type,conf=CI.level)[[4]][4]
      ba.stat$LOA.slope.CI.lower <- boot.ci(boot(baLog$diffs,
                                                 function(dat,idx) ANTILOGslope(dat[idx]),R=boot.R),
                                            type=boot.type,conf=CI.level)[[4]][5] }
    
    # Recomputing LOAs and their CIs as a function of size multiplied by the computed slopes
    y.fit <- data.frame(size,
                        ANTLOGdiffs.upper = size * ba.stat$LOA.slope, # upper LOA
                        ANTLOGdiffs.upper.lower = size * ba.stat$LOA.slope.CI.lower,
                        ANTLOGdiffs.upper.upper = size * ba.stat$LOA.slope.CI.upper,
                        ANTLOGdiffs.lower = size * ((-1)*ba.stat$LOA.slope), # lower LOA
                        ANTLOGdiffs.lower.lower = size * ((-1)*ba.stat$LOA.slope.CI.lower),
                        ANTLOGdiffs.lower.upper = size * ((-1)*ba.stat$LOA.slope.CI.upper))
    
    # adding bias values based on prop.bias
    if(prop.bias==FALSE){ y.fit$y.bias <- rep(ba.stat$mean.diffs,nrow(y.fit)) } else { y.fit$y.bias <- b0+b1*y.fit$size }
    
    p <- p + # adding lines to plot
      
      # UPPER LIMIT (i.e., bias + 2 * (e^(1.96 SD) - 1)/(e^(1.96 SD) + 1))
      geom_line(data=y.fit,aes(y = y.bias + ANTLOGdiffs.upper),colour="darkgray",size=1.3) +
      geom_line(data=y.fit,aes(y = y.bias + ANTLOGdiffs.upper.upper),colour="darkgray",linetype=2,size=1) +
      geom_line(data=y.fit,aes(y = y.bias + ANTLOGdiffs.upper.lower),colour="darkgray",linetype=2,size=1) +
      
      # LOWER LIMIT (i.e., bias - 2 * (e^(1.96 SD) - 1)/(e^(1.96 SD) + 1))
      geom_line(data=y.fit,aes(y = y.bias + ANTLOGdiffs.lower),colour="darkgray",size=1.3) +
      geom_line(data=y.fit,aes(y = y.bias + ANTLOGdiffs.lower.upper),colour="darkgray",linetype=2,size=1) +
      geom_line(data=y.fit,aes(y = y.bias + ANTLOGdiffs.lower.lower),colour="darkgray",linetype=2,size=1)
    
    # ..........................................
    # 3.3. HETEROSCHEDASTICITY (only a warning)
    # ..........................................
    if(heterosced==TRUE){
      
      # warning message
      cat("\n\nWARNING: standard deviation of differences in in log transformed ",Measure,
          " might be proportional to the size of measurement (coeff. = ",
          round(coef(mRes)[2],2)," [",round(CIRes[1],2),", ",round(CIRes[2],2),"]",").",sep="") }}
  
  
  p <- p + # adding last graphical elements and plotting with marginal density distribution
    
    geom_point(size=4.5,shape=20) +
    xlab(xlab) + ylab(paste("Device - reference differences in\n",measure,sep="")) +
    ggtitle(paste("Bland-Altman plot of",measure))+
    theme(axis.text = element_text(size=12,face="bold"),
          axis.title = element_text(size=15,face="bold",colour="black"),
          plot.title = element_text(hjust = 0.5,size=12,face="bold"))
  if(!is.na(ylim[1])){ p <- p + ylim(ylim) }
  if(!is.na(xlim[1])){ p <- p + xlim(xlim) }
  return(ggMarginal(p,fill="lightgray",colour="lightgray",size=4,margins="y"))
  
}