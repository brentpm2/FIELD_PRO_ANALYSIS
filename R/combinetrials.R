
#average all numeric values
combine<-function(datafile, trt, p_crit = 0.05) {
  #does the datafile have the necessary components?
  if(is.atomic(datafile)) {
    stop("Improper datafile, expected a list")
  }
  if(is.null(datafile$'EVALUATION MEAN')) {
    stop("Improper datafile, requires EVALUATION MEAN entry in provided list")
  }
  if(is.null(datafile$`EVALUATION MEAN`$`SUMMARY TRT#`)) {
    stop("Improper datafile, requires 'SUMMARY TRT# within EVALUATION MEAN in provided list")
  }
  if(is.null(datafile$`EVALUATION MEAN`$`TRIAL #`)) {
    stop("Improper datafile, requires 'TRIAL #' within EVaLUATION MEAN in provided list")
  }
  if(is.null(datafile$'EVALUATION MEAN'$DATA)) {
    stop("Improper datafile, requires 'DATA' within EVALUATION MEAN in the provided list")
  }
  #is trt present within datafile
  if(length(datafile$`EVALUATION MEAN`$`SUMMARY TRT#` == trt) <= 1) {
    stop("Improper common treatment, present only in one or fewer entries in datafile")
  }
  #is the supplied p-value valid? 1>p_crit>0
  if(p_crit > 1 || p_crit < 0) {
    stop("Improper p_critical value. P value must be between 0 and 1.")
  }
  
  #create a subset of inputted datafile delimited by inputted treatment number 
  datafilesubset <- datafile$`EVALUATION MEAN`[datafile$`EVALUATION MEAN`$`SUMMARY TRT#` ==trt ,]
  #create a list of all trials observed in datafilesublist
  index <- datafilesubset$`TRIAL #`
  #use list of trials to obtain data values
  fulltrials <- datafile$`EVALUATION MEAN`[datafile$`EVALUATION MEAN`$`TRIAL #` %in% index,]
  #name columns to improve readability
  colnames(fulltrials)[3] <- "summarytrt"
  colnames(fulltrials)[1] <- "trialnum"
  #conduct anova across the subsetted datafile
  my_anova <- aov(DATA ~ summarytrt + trialnum, data = fulltrials)
  #extract p-values from the conducted anova
  p_values <- summary(my_anova)[[1]][["Pr(>F)"]]
  #did the anova pass? did the pvalue for trialnum beat the inputted p_crit?
  can_combine <- p_values[2] > p_crit
  #if data can be combined, output the combined values under a common trial name
  
  if(can_combine) {
    #make a unique name to prevent overlap with existing trials
    
    #add new name to list of trials
    index[[length(index)+1]] <- as.numeric(Sys.time())
    is_duplicated <- duplicated(index)
    #is the newly added trial name unique?
    if(is_duplicated[length(index)]) {
      #when passed a list of two entries which are the same, make.unique will modify the second one.
      #this code will execute if the new name is a duplicate, and will be replaced with a unique name.
      index[[length(index)]] <- make.unique(rep(index[[length(index) + 1]],2))[2]
    }
    fulltrials$trialnum <- rep(index[[length(index)]], nrow(fulltrials))
    return(fulltrials)
  } else {
    return(can_combine)
  }
}
