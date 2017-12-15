library(readxl)   
readallsheets <- function(filename,new_csv_name= "Evaluationmean.csv") {
  #this function will read in the designated xls file and will convert three sheets into a list of dataframes
  #The first and third data frame will have only a subset of the columns
  #The third sheet (EVALUATION MEAN)will be outputted as a user generated named file.
  #If no name is selected the file will be called Evaluationmean.csv
  
  #Will give an error if the file is not located in the local directory
  if(!file.exists(filename)){
    stop("File does not exist in the local directory")
  }
  #Get the names of the sheets
  sheets <- excel_sheets(filename)
  #Puts the data from the sheets into the list as individual data frames
  data_in_list <-    sapply(sheets, function(X) as.data.frame(read_excel(filename, sheet = X)))
  #Will give an error message if the data is too large or negative
  if(max(data_in_list$"EVALUATION MEAN"$DATA, na.rm = TRUE) > 1000 || min(data_in_list$"EVALUATION MEAN"$DATA, na.rm = TRUE) < 0){
    stop("Unreasonable response variable data found.")
  }
  #Will give an error message if any of the rainfall data is less than zero
  if(min(data_in_list$"EVALUATION MEAN"[,c("TOTAL MOISTURE - 1 DAY
[DALA]","TOTAL MOISTURE - 3 DAY
[DALA]","TOTAL MOISTURE - 7 DAY
[DALA]","TOTAL MOISTURE - 2 WEEK
[DALA]","TOTAL MOISTURE - 3 WEEK
[DALA]","TOTAL MOISTURE - 4 WEEK
[DALA]")], na.rm = TRUE) < 0){
    stop("unvreasonable rainfall data")
  }
    #Gives an error message if the treatment number is negative
  if(min(data_in_list$"EVALUATION MEAN"$"SUMMARY TRT#", na.rm=TRUE) < 0){
    stop("Cannot have a negative treatment number")
  }
  #Will give a warning message if the rate values are too high, but will still finish the rest of the function
  if(max(data_in_list$"TREATMENT"$RATE, na.rm = TRUE) > 100 || min(data_in_list$"TREATMENT"$RATE, na.rm = TRUE) < 0){
    warning("Abnormal rate values detected.")
  }

  #Removes 'optional trt info' from the treatment dataframe
  data_in_list$TREATMENT <- data_in_list$TREATMENT[, c(1:9)]
  #Removes all columns that are not useful for analysis
  data_in_list$"EVALUATION MEAN"<-data_in_list$"EVALUATION MEAN"[,c(1,2,3,5,7,8,15,17,19,21,25,26,27
                                                                    ,32,33,34,35,36,37,54,56,57,58,61,62,63)]

  #assigns the names from the excel file to the data
  names(data_in_list) <- sheets
  #Outputs the evaluation mean dataframe to a csv file of inputted name
  write.csv(data_in_list$"EVALUATION MEAN",  new_csv_name)
 

  #Output the list of all three sheets
  data_in_list
}


