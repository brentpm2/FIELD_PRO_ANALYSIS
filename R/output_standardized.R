#for the specified columns within the inputted dataframe:
#make the mean 0 and deviation 1
#each column must only contain numeric data
output_standardized <- function(datafile,col_names,filename, should_append = FALSE) {
  #check if datafile is a dataframe or list
  if(is.atomic(datafile)) {
    stop("Improper datafile input: expected vector")
  }
  #check if columns in col_names are all present within datafile
  index <-colnames(datafile) %in% col_names
  num_index <- sum(index, na.rm = TRUE)
  if(num_index!=length(col_names)) {
    stop("Improper col_names input: at least one column name is absent from the datafile")
  }
  #check if the columns in col_name are numeric
  #subset dataframe to necessary columns
  datafile_subset <- datafile[,col_names]
  #check which columns are numeric
  numeric_col <- sapply(datafile_subset,is.numeric)
  #count numeric columns
  numeric_col <- sum(numeric_col,na.rm = TRUE)
  #check if the number of numeric columns is the same as the number of columns
  if(numeric_col!= length(col_names)) {
    stop("Improper col_names input: at least one column name corresponds to a non-numeric column")
  }
  #check if filename is valid (character) 
  if(!is.character(filename)) {
    stop("Improper filename: expected character")
  }
  #get mean of each column, put into list
  means <- colMeans(datafile_subset)
  #get st.dev of each column, put into list
  variance <- apply(datafile_subset,2,sd)
  #subtract mean from each entry of the corresponding column, divide by st.dev
  datafile_subset <- apply(datafile_subset,1,function(x) (x-means)/variance)
  #export as tab separated table
  write.table(datafile_subset,file = filename, append = should_append,sep = "\t", row.names = FALSE, col.names = FALSE)
  #export mean and st.dev lists into tab separated file.
  coeffic <- data.frame(means, variance)
  coeffic_file <- paste(filename, "coefficients")
  write.table(coeffic, file = coeffic_file, append = should_append, sep = "\t",row.names = FALSE, col.names = FALSE)
  #return standardized datafile
  return(datafile_subset)
}


