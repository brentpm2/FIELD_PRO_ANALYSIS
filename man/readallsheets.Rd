\name{readallsheets}
\alias{readallsheets}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Readallsheets-xls file
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
This function takes in a user given xls file that was outputted by MTA for FieldPro and reads in all three sheets.  Several columns not used for data analysis are removed.  The function will also give errors if there are negative values for weather data and response variable data.  A CSV file containing a the saved columns from the EVALUATION MEANS sheet of the original document will be saved to a user defined file name.  If no file name is given, the saved file will be named "Evaluationmean.csv".
}
\usage{
readallsheets(filename, new_csv_name = "Evaluationmean.csv")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
An XLS file ,outputted from MTA for FieldPro, that is inputted by the user.
}
  \item{new_csv_name}{
%%     ~~Describe \code{new_csv_name} here~~
The name of the new file that will be created.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
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

%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Christopher Landau
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
require(readxl) ##for read_excel, excel_sheets
mysheets<-readallsheets("YIELDALL_MTA_FLATFILE.xls","example.csv")
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.

