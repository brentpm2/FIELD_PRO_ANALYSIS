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
Brent Murphy and Christopher Landau
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (filename, new_csv_name = "Evaluationmean.csv") 
{
    if (!file.exists(filename)) {
        stop("File does not exist in the local directory")
    }
    sheets <- excel_sheets(filename)
    data_in_list <- sapply(sheets, function(X) as.data.frame(read_excel(filename, 
        sheet = X)))
    if (max(data_in_list$"EVALUATION MEAN"$DATA, na.rm = TRUE) > 
        1000 || min(data_in_list$"EVALUATION MEAN"$DATA, na.rm = TRUE) < 
        0) {
        stop("Unreasonable response variable data found.")
    }
    if (min(data_in_list$"EVALUATION MEAN"[, c("TOTAL MOISTURE - 1 DAY\n[DALA]", 
        "TOTAL MOISTURE - 3 DAY\n[DALA]", "TOTAL MOISTURE - 7 DAY\n[DALA]", 
        "TOTAL MOISTURE - 2 WEEK\n[DALA]", "TOTAL MOISTURE - 3 WEEK\n[DALA]", 
        "TOTAL MOISTURE - 4 WEEK\n[DALA]")], na.rm = TRUE) < 
        0) {
        stop("unvreasonable rainfall data")
    }
    if (min(data_in_list$"EVALUATION MEAN"$"SUMMARY TRT#", na.rm = TRUE) < 
        0) {
        stop("Cannot have a negative treatment number")
    }
    if (max(data_in_list$TREATMENT$RATE, na.rm = TRUE) > 100 || 
        min(data_in_list$TREATMENT$RATE, na.rm = TRUE) < 0) {
        warning("Abnormal rate values detected.")
    }
    data_in_list$TREATMENT <- data_in_list$TREATMENT[, c(1:9)]
    data_in_list$"EVALUATION MEAN" <- data_in_list$"EVALUATION MEAN"[, 
        c(1, 2, 3, 5, 7, 8, 15, 17, 19, 21, 25, 26, 27, 32, 33, 
            34, 35, 36, 37, 54, 56, 57, 58, 61, 62, 63)]
    names(data_in_list) <- sheets
    write.csv(data_in_list$"EVALUATION MEAN", new_csv_name)
    data_in_list
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
