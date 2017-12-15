install.packages("stringr")
library(stringr)
#constructs a bar chart of yield output all treatments containing a specific component
#works within a specified trial, by default the first trial listed in the dataset will be used
plot_by_component <- function(datafile,treatment_list,component,trial = "",resp_var = "DATA") {
  #check to see if inputs are valid
  if(!is.data.frame(datafile)) {
    stop("Improper datafile input: expected a dataframe")
  }
  if(!is.data.frame(treatment_list)) {
    stop("Improper treatment_list input: expected dataframe")
  }
  if(!is.character(component)) {
    stop("Improper component input: expected string/character")
  }
  if(is.null(datafile[resp_var,])) {
    stop("Improper datafile and resp_var combination: specified variable is not present within datafile")
  }
  if(is.null(treatment_list$"TREATMENT COMPONENT")) {
    stop("Improper treatment_list: expected column labeled 'TREATMENT COMPONENT'")
  }
  if(is.null(treatment_list$"SUMMARY TRT #")) {
    stop("Improper treatmetn_list: expected column labeled 'SUMMARY TRT#'")
  }
  if(is.null(datafile$"SUMMARY TRT#")) {
    stop("improper datafile: expected column labeled 'SUMMARY TRT#")
  }
  #get a list of treatments containing a component
  valid_treatments <- treatment_list[treatment_list$"TREATMENT COMPONENT"==component,"SUMMARY TRT #"]
  if(length(valid_treatments)<1) {
    stop("Improper component: component not present within treatment_list")
  }
  #ensure only one trial is present within datafile
  if(trial=="") {
    datafile <- datafile[datafile$`TRIAL #`==datafile$`TRIAL #`[1],]
  } else {
    datafile <- datafile[datafile$`TRIAL #`==trial,]
  }
  #extract digits from non-treatments, allowing comparison to datafile
  valid_treatments <- str_extract(valid_treatments,"^[[:digit:]]+")
  #subset datafile by the list of treatments
  datafile_subset <- datafile[datafile$`SUMMARY TRT#` %in% valid_treatments,]
  #aggregate all instances of the same treatment
  datafile_subset <- aggregate(x = datafile_subset,by = list(datafile_subset$"SUMMARY TRT#"),mean)
  #graph the subset by the specified variable
  output_heights <-datafile_subset[,resp_var]
  pdf_title <-paste("Effect of Treatment containing '",component,"' on Yield", sep = "")
  pdf(paste(pdf_title,".pdf",sep = ""))
  barplot(height = output_heights,names.arg = datafile_subset$'SUMMARY TRT#',
          xlab = "SUMMARY TRT#", ylab = resp_var,
          main = pdf_title)
  dev.off()
  write.table(datafile_subset,file = paste(pdf_title,".txt",sep = ""))
  #export graph as pdf
  return(datafile_subset)
  #export component list of all treatments used in graph to same pdf
}

trtlist <- data.frame("SUMMARY TRT #" = c("1-A","1-B","2-A","2-B","3-A"),
                      "TREATMENT COMPONENT" = c("a","b","a","c","b"))
names(trtlist)<-c("SUMMARY TRT #","TREATMENT COMPONENT")
datafile <- data.frame("DATA" = c(10,9,8,7,6,5,4),
                       "SUMMMARY TRT#" = c(1,2,3,4,5,1,1),
                       "TRIAL #" = c(1,1,1,1,1,1,2))
names(datafile)<-c("DATA","SUMMARY TRT#","TRIAL #")
plot_by_component(datafile = datafile,treatment_list = trtlist,
                  component = "a",resp_var = "DATA")
