library(tidyverse)
library(ggformula)
options(scipen = 999)


SimulateProportion <- function(n, x, p, alternative, reps){
  if (length(n)==1){
    phat <- x/n
    Heads <- c(rep(NA), reps)
    for( i in 1:reps){
      Flips <- sample(1:2, prob= c(p, 1-p), size=n, replace=TRUE)
      Heads[i] <- sum(Flips == 1)
    }
    PropSuccess <- Heads/n     
    Results <- data.frame(PropSuccess)
    hist <- gf_histogram(~PropSuccess, data=Results,fill="blue", color="black") %>%
      gf_labs(title="Null Distribution for Sample Proportion", x="Simulated Proportion", y="Frequency") +
      geom_vline(xintercept=phat, colour="red")
    
    if(alternative=="less"){
      pval <- sum(PropSuccess <= phat) /reps} else if (alternative=="greater"){
        pval <- sum(PropSuccess >= phat) /reps} else{
          pval <- sum(abs(PropSuccess - p) >= abs(phat-p)) /reps
          hist <- gf_histogram(~PropSuccess, data=Results,fill="blue", color="black") %>%
            gf_labs(title="Null Distribution for Sample Proportion", x="Simulated Proportion", y="Frequency") +
            geom_vline(xintercept=c(p-abs(p-phat),p+abs(p-phat) ), colour="red")}
  }
  
  if (length(n)==2){
    nsuccess1 <- x[1]
    nfail1 <- n[1]-x[1]
    nsuccess2 <- x[2]
    nfail2 <- n[2]-x[2]
    
    Group1 <- c(rep(0, nfail1), rep(1, nsuccess1))
    Group2 <- c(rep(0, nfail2), rep(1, nsuccess2))
    MeanDiff <- mean(Group1)-mean(Group2)
    Group <- c(rep("Group1", length(Group1)), rep("Group2", length(Group2)))
    Response <- c( Group1, Group2) #observations
    Data <- data.frame(Group, Response) #Put data into a dataframe structure
    reps <- 1000 #number of repetitions
    Difference <- c(rep("NA", reps))
    
    #simulate reps repetitions
    for (i in 1:reps){
      #randomly shuffle responses
      Data$Response <- Data$Response[sample(1:nrow(Data))]
      #calculate difference in Group Means
      Difference[i] <- Data %>% filter(Group == "Group1") %>% summarize(mean(Response))-
        Data %>% filter(Group == "Group2") %>% summarize(mean(Response))
    }
    Difference <- as.numeric(as.character((Difference)))  #convert to numeric
    Results <- data.frame(Difference)  #create dataframce with results
    
    hist <- gf_histogram(~Difference, data=Results, color = "black", fill="blue") %>%
      gf_labs(title="Null Distribution for Differences in Sample Proportion", x="Simulated Difference in Proportions", y="Frequency")
    if(alternative=="less"){
      pval <- sum(Results <= MeanDiff)/reps
      hist <- hist + geom_vline(xintercept=MeanDiff, colour="red")
    } else if (alternative=="greater"){
      pval <- sum(Results >= MeanDiff)/reps
      hist <- hist + geom_vline(xintercept=MeanDiff, colour="red") } else{
        pval <- sum(abs(Results) >= abs(MeanDiff))/reps
        hist <- gf_histogram(~Difference, data=Results, color = "black", fill="blue") +
          geom_vline(xintercept=c(MeanDiff, -MeanDiff), colour="red")   #for 1-sided test, get rid of 2nd or 3rd line
      }
  }
  if(length(n)==2){
    list <- list(hist, "Observed Difference in Proportions"=MeanDiff, "Simulation-based p-value"=pval)}else{
      list <- list(hist, "Observed Proportion"=phat, "Simulation-based p-value"=pval)}
  return(list)
}

###########################################################################################################

SimulateMean <- function(x, y=NULL, mu=0, alternative, reps, paired=FALSE){
  if (is.null(y)){
    Values <- x
    xbar <- mean(Values)
    Values <- Values - (mean(Values)-mu)
    
    #Take samples of size equal to your sample and calculate sample means
    sampsize <- length(Values)
    xbarsim <- c(rep(NA), reps)
    for( i in 1:reps){
      bt <- Values[sample(1:sampsize, sampsize , replace=TRUE)]
      xbarsim[i] <- mean(bt)
    }
    Results <- data.frame(xbarsim)
    
    hist <- gf_dhistogram(~xbarsim, data=Results, border=0, fill="blue", color="black") %>%
      gf_labs(title="Null Distribution for Sample Mean", x="Simulated Sample Mean", y="Frequency") +
      geom_vline(xintercept=xbar, colour="red")
    
    if(alternative=="less"){
      pval <- sum(Results <= xbar)/reps
    } else if (alternative=="greater"){
      pval <- sum(Results >= xbar)/reps} else{
        pval <- sum(abs(Results) >= abs(xbar))/reps
        hist <- hist <- gf_dhistogram(~xbarsim, data=Results, border=0, fill="blue", color="black") %>%
          gf_labs(title="Null Distribution for Sample Mean", x="Simulated Sample Mean", y="Frequency") +
          geom_vline(xintercept=c(mu-abs(xbar-mu), mu+abs(xbar-mu)), colour="red")   #for 1-sided test, get rid of 2nd or 3rd line
      }
  } else if(paired==FALSE){
    Group1 <- x
    Group2 <- y
    MeanDiff <- mean(Group1)-mean(Group2)
    Group <- c(rep("Group1", length(Group1)), rep("Group2", length(Group2)))
    Response <- c( Group1, Group2) #observations
    Data <- data.frame(Group, Response) #Put data into a dataframe structure
    Difference <- c(rep("NA", reps))
    #simulate repetitions
    for (i in 1:reps){
      #randomly shuffle responses
      Data$Response <- Data$Response[sample(1:nrow(Data))]
      #calculate difference in Group Means
      #  Difference[i] <- Data %>% filter(Group == "Group1") %>% summarize(mean(Response))-
      #    Data %>% filter(Group == "Group2") %>% summarize(mean(Response))
      Difference[i] <- mean(Data[Data$Group=="Group1", ]$Response)-mean(Data[Data$Group=="Group2", ]$Response)
    }
    Difference <- as.numeric(as.character((Difference)))  #convert to numeric
    Results <- data.frame(Difference)  #create dataframce with results
    
    hist <-  gf_histogram(~Difference, data=Results, color = "black", fill="blue") %>%
      gf_labs(title="Null Distribution for Differences in Sample Mean", x="Simulated Difference in Sample Means", y="Frequency")+
      geom_vline(xintercept=MeanDiff, colour="red")
    if(alternative=="less"){
      pval <- sum(Results <= MeanDiff)/reps
      hist <- hist + geom_vline(xintercept=MeanDiff, colour="red")
    } else if (alternative=="greater"){
      pval <- sum(Results >= MeanDiff)/reps
      hist <- hist + geom_vline(xintercept=MeanDiff, colour="red") } else{
        pval <- sum(abs(Results) > abs(MeanDiff))/reps
        hist <- gf_histogram(~Difference, data=Results, color = "black", fill="blue") %>%
          gf_labs(title="Null Distribution for Differences in Sample Mean", x="Simulated Difference in Sample Means", y="Frequency")+
          geom_vline(xintercept=c(MeanDiff, -MeanDiff), colour="red")   #for 1-sided test, get rid of 2nd or 3rd line
      }
  } else{#for paired data
    Group1 <- x
    Group2 <- y
    MeanDiff <- mean(Group1)-mean(Group2)
    Data <- data.frame(Group1, Group2) #Put data into a dataframe structure
    Data$Diff <- Data$Group1 - Data$Group2 #compute pairwise differences
    Difference <- c(rep("NA", reps))
    
    #simulate reps repetitions
    for (i in 1:reps){ #paired data
      #randomly shuffle within each pair (i.e. change sign for randomly selected pairs)
      samp <- sample(0:1, replace=TRUE, nrow(Data))
      Data$Diff[samp==1] <- -1*Data$Diff[samp==1]
      #calculate difference in Group Means
      Difference[i] <- mean(Data$Diff)
    }
    Difference <- as.numeric(as.character(Difference))
    Results <- data.frame(Difference)  #create dataframce with results
    hist <-  gf_histogram(~Difference, data=Results, color = "black", fill="blue") %>%
      gf_labs(title="Null Distribution for Differences in Sample Mean", x="Simulated Difference in Sample Means", y="Frequency")+
      geom_vline(xintercept=MeanDiff, colour="red")
    
    if(alternative=="less"){
      pval <- sum(Results <= MeanDiff)/reps
      hist <- hist + geom_vline(xintercept=MeanDiff, colour="red")
    } else if (alternative=="greater"){
      pval <- sum(Results >= MeanDiff)/reps
      hist <- hist + geom_vline(xintercept=MeanDiff, colour="red") } else{
        pval <- sum(abs(Results) > abs(MeanDiff))/reps
        hist <- gf_histogram(~Difference, data=Results, color = "black", fill="blue") %>%
          gf_labs(title="Null Distribution for Differences in Sample Mean", x="Simulated Difference in Sample Means", y="Frequency")+
          geom_vline(xintercept=c(MeanDiff, -MeanDiff), colour="red")   #for 1-sided test, get rid of 2nd or 3rd line
      }
  }
  if(is.null(y)==FALSE){
    list <- list(hist, "Observed Difference in Sample Means"=MeanDiff, "Simulation-based p-value"=pval)}else{
      list <- list(hist, "Observed Sample Mean"=xbar, "Simulation-based p-value"=pval)}
  return(list)
}
###########################################################################################################

SimulateRegression <- function(data, x, y, reps){
  Slopes <- rep(NA, reps)
  names(data)[which(names(data)==x)] <- "x"
  names(data)[which(names(data)==y)] <- "y"
  ObsSlope <- summary(lm(data=data, y ~ x))$coefficients[2]
  for (i in 1:reps){
    data$Shuffled <- data$x[sample(1:nrow(data))]
    Slopes[i] <- summary(lm(data=data, y ~ Shuffled))$coefficients[2]
  }
  Slopesdf <- data.frame(Slopes)
  hist <- gf_histogram(~Slopes, data=Slopesdf, fill="blue", color="black") %>%
    gf_labs(title="Null Distribution for Slope", x="Simulated Slope", y="Frequency")+
    geom_vline(xintercept=-ObsSlope, colour="red") +
    geom_vline(xintercept=ObsSlope, colour="red")
  pval <- mean(abs(Slopesdf$Slopes)>=abs(ObsSlope))
  list <- list(hist, "Observed Slope"=ObsSlope,  "Simulation-based p-value"=pval)
  return(list)
}

##########################################################################3

SimulateChiSq <- function(data, x, y, reps){
  ChiSq <- rep(NA, reps)
  names(data)[which(names(data)==x)] <- "x"
  names(data)[which(names(data)==y)] <- "y"
  T <- table(data$y, data$x)
  ObsChiSq <- chisq.test(T)$statistic
  for (i in 1:reps){
    data$Shuffled <- data$x[sample(1:nrow(data))]
    Ts <- table(data$y, data$Shuffled)
    ChiSq[i] <- chisq.test(Ts)$statistic
  }
  ChiSqdf <- data.frame(ChiSq)
  hist <- gf_histogram(~ChiSq, data=ChiSqdf, fill="blue", color="black") %>%
    gf_labs(title="Null Distribution for Chi-Square Statistic", x="Simulated Chi-Squared", y="Frequency")+
    geom_vline(xintercept=ObsChiSq, colour="red")
  pval <- mean(abs(ChiSqdf$ChiSq)>=ObsChiSq)
  list <- list(hist, "Observed Chi-Square Statistic"=ObsChiSq, "Simulation-based p-value"=pval)
  return(list)
}

SimulateF <- function(data, x, y, reps){
  Fsim <- rep(NA, reps)
  names(data)[which(names(data)==x)] <- "x"
  names(data)[which(names(data)==y)] <- "y"
  ObsF <- summary(lm(data=data, y~x))$fstatistic[1]
  for (i in 1:reps){
    data$Shuffled <- data$x[sample(1:nrow(data))]
    Fsim[i] <- summary(lm(data=data, y~Shuffled))$fstatistic[1]
  }
  Fsimdf <- data.frame(Fsim)
  hist <- gf_histogram(~Fsim, data=Fsimdf, fill="blue", color="black") %>%
    gf_labs(title="Null Distribution for F-Square Statistic", x="Simulated F", y="Frequency")+
    geom_vline(xintercept=ObsF, colour="red")
  pval <- mean(abs(Fsimdf$Fsim)>=ObsF)
  list <- list(hist, "Observed F Statistic"=ObsF, "Simulation-based p-value"=pval)
  return(list)
}

######################################################
#Modify prop.test in R to return z-stat, rather than Chi-sq

zprop.test <- function (x, n, p = NULL, alternative = c("two.sided", "less", 
                                          "greater"), conf.level = 0.95, correct = TRUE) 
{
  DNAME <- deparse1(substitute(x))
  if (is.table(x) && length(dim(x)) == 1L) {
    if (dim(x) != 2L) 
      stop("table 'x' should have 2 entries")
    l <- 1
    n <- sum(x)
    x <- x[1L]
  }
  else if (is.matrix(x)) {
    if (ncol(x) != 2L) 
      stop("'x' must have 2 columns")
    l <- nrow(x)
    n <- rowSums(x)
    x <- x[, 1L]
  }
  else {
    DNAME <- paste(DNAME, "out of", deparse1(substitute(n)))
    if ((l <- length(x)) != length(n)) 
      stop("'x' and 'n' must have the same length")
  }
  OK <- complete.cases(x, n)
  x <- x[OK]
  n <- n[OK]
  if ((k <- length(x)) < 1L) 
    stop("not enough data")
  if (any(n <= 0)) 
    stop("elements of 'n' must be positive")
  if (any(x < 0)) 
    stop("elements of 'x' must be nonnegative")
  if (any(x > n)) 
    stop("elements of 'x' must not be greater than those of 'n'")
  if (is.null(p) && (k == 1)) 
    p <- 0.5
  if (!is.null(p)) {
    DNAME <- paste0(DNAME, ", null ", if (k == 1) 
      "probability "
      else "probabilities ", deparse1(substitute(p)))
    if (length(p) != l) 
      stop("'p' must have the same length as 'x' and 'n'")
    p <- p[OK]
    if (any((p <= 0) | (p >= 1))) 
      stop("elements of 'p' must be in (0,1)")
  }
  alternative <- match.arg(alternative)
  if (k > 2 || (k == 2) && !is.null(p)) 
    alternative <- "two.sided"
  if ((length(conf.level) != 1L) || is.na(conf.level) || (conf.level <= 
                                                          0) || (conf.level >= 1)) 
    stop("'conf.level' must be a single number between 0 and 1")
  correct <- as.logical(correct)
  ESTIMATE <- setNames(x/n, if (k == 1) 
    "p"
    else paste("prop", 1L:l)[OK])
  NVAL <- p
  CINT <- NULL
  YATES <- if (correct && (k <= 2)) 
    0.5
  else 0
  if (k == 1) {
    z <- qnorm(if (alternative == "two.sided") 
      (1 + conf.level)/2
      else conf.level)
    YATES <- min(YATES, abs(x - n * p))
    z22n <- z^2/(2 * n)
    p.c <- ESTIMATE + YATES/n
    p.u <- if (p.c >= 1) 
      1
    else (p.c + z22n + z * sqrt(p.c * (1 - p.c)/n + z22n/(2 * 
                                                            n)))/(1 + 2 * z22n)
    p.c <- ESTIMATE - YATES/n
    p.l <- if (p.c <= 0) 
      0
    else (p.c + z22n - z * sqrt(p.c * (1 - p.c)/n + z22n/(2 * 
                                                            n)))/(1 + 2 * z22n)
    CINT <- switch(alternative, two.sided = c(max(p.l, 0), 
                                              min(p.u, 1)), greater = c(max(p.l, 0), 1), less = c(0, 
                                                                                                  min(p.u, 1)))
  }
  else if ((k == 2) && is.null(p)) {
    DELTA <- ESTIMATE[1L] - ESTIMATE[2L]
    YATES <- min(YATES, abs(DELTA)/sum(1/n))
    WIDTH <- (switch(alternative, two.sided = qnorm((1 + 
                                                       conf.level)/2), qnorm(conf.level)) * sqrt(sum(ESTIMATE * 
                                                                                                       (1 - ESTIMATE)/n)) + YATES * sum(1/n))
    CINT <- switch(alternative, two.sided = c(max(DELTA - 
                                                    WIDTH, -1), min(DELTA + WIDTH, 1)), greater = c(max(DELTA - 
                                                                                                          WIDTH, -1), 1), less = c(-1, min(DELTA + WIDTH, 1)))
  }
  if (!is.null(CINT)) 
    attr(CINT, "conf.level") <- conf.level
  METHOD <- paste(if (k == 1) 
    "1-sample proportions test"
    else paste0(k, "-sample test for ", if (is.null(p)) 
      "equality of"
      else "given", " proportions"), if (YATES) 
        "with"
    else "without", "continuity correction")
  if (is.null(p)) {
    p <- sum(x)/sum(n)
    PARAMETER <- k - 1
  }
  else {
    PARAMETER <- k
    names(NVAL) <- names(ESTIMATE)
  }
  names(PARAMETER) <- "df"
  x <- cbind(x, n - x)
  E <- cbind(n * p, n * (1 - p))
  if (any(E < 5)) 
    warning("Chi-squared approximation may be incorrect")
  STATISTIC <- sum((abs(x - E) - YATES)^2/E)
  if (alternative == "two.sided") 
    PVAL <- pchisq(STATISTIC, PARAMETER, lower.tail = FALSE)
  else {
    if (k == 1) 
      z <- sign(ESTIMATE - p) * sqrt(STATISTIC)
    else z <- sign(DELTA) * sqrt(STATISTIC)
    PVAL <- pnorm(z, lower.tail = (alternative == "less"))
  }
  if (k == 1) 
    SIGN <- sign(ESTIMATE - p) 
  else SIGN <- sign(DELTA) 
  STATISTIC <- sqrt(sum((abs(x - E) - YATES)^2/E))*SIGN
  names(STATISTIC) <- "z-statistic"
  RVAL <- list(statistic = STATISTIC, parameter = PARAMETER, 
               p.value = as.numeric(PVAL), estimate = ESTIMATE, null.value = NVAL, 
               conf.int = CINT, alternative = alternative, method = METHOD, 
               data.name = DNAME)
  class(RVAL) <- "htest"
  return(RVAL)
}
