\name{output_standardized}
\alias{output_standardized}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Standardize select variables within dataframe
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function standardized specified columns within a dataframe to have a mean of 0 and a standard deviation of 1. The standardized dataframe is exported to a tab separated file of a provided name. The mean and variance, used to standardize the dataframe, are outputted to a separate file.
}
\usage{
output_standardized(datafile, col_names, filename, should_append = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datafile}{
%%     ~~Describe \code{datafile} here~~
A dataframe input containing data for standardization.
}
  \item{col_names}{
%%     ~~Describe \code{col_names} here~~
A list of column names corresponding to the inputted dataset, specifies which columns to standardize.
}
  \item{filename}{
%%     ~~Describe \code{filename} here~~
A character string denoting the name of the outputted files.
}
  \item{should_append}{
%%     ~~Describe \code{should_append} here~~
A boolean variable denoting if the outputted files should overwrite existing files or be appended to existing files.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a transposed matrix of standardized data.
Exports a the standardized dataframe to a tab separated file.
Exports coefficients used to standardize data to a separate tab separated file.
}
\author{
%%  ~~who you are~~
Brent Murphy
}
\examples{
test1 <-data.frame(x = c(1,2,3,4,5),
                   y = c(2,4,6,8,10),
                   z = c(15,20,30,40,50))
colnames(test1)<-c("ONE","TWO","THREE")
output_standardized(datafile = test1, 
                    col_names = c("ONE","TWO","THREE"), 
                    filename = "testfiles")
}
