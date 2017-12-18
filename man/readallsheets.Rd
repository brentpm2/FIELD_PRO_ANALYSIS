\name{readallsheets}

\alias{readallsheets}

\title{
Readallsheets-xls file
}
\description{

This function takes in a user given xls file that was outputted by MTA for FieldPro and reads in all three sheets.  Several columns not used for data analysis are removed.  The function will also give errors if there are negative values for weather data and response variable data.  A CSV file containing a the saved columns from the EVALUATION MEANS sheet of the original document will be saved to a user defined file name.  If no file name is given, the saved file will be named "Evaluationmean.csv".
}

\usage{
readallsheets(filename, new_csv_name = "Evaluationmean.csv")
}

\arguments{
  \item{filename}{
An XLS file ,outputted from MTA for FieldPro, that is inputted by the user.
}
  \item{new_csv_name}{
The name of the new file that will be created.
}
}

\value{
A CSV file containing data from the EVALUATION MEAN sheet of the inputted MTA xls file.

A list of three dataframes built from the original inputted MTA file:

  \item{TREATMENT }{
  Dataframe containing all columns from the TREATMENT sheet of the original MTA file except for the "OPTIONAL TRT INFO" column.
  }
  \item{SETTINGS}{
  Dataframe containing all columns from the SETTINGS sheet of the original MTA file.
  }
  \item{EVALUATION MEANS }{
  Dataframe containining columns useful for data analysis and identification from the EVALUATION MEAN sheet of the original MTA file. }
}

\author{
Christopher Landau
}

\examples{
path1<-system.file("extdata", "YIELDALL_MTA_FLATFILE.xls", package = "fieldproanalysis")
mysheets<-readallsheets(path1,"example.csv")
## Will give a warning that the rate values may be abnormal.  
## This is due to some of the example rates in YIELDALL_MTA_FLATFILE.xls being large.
}
