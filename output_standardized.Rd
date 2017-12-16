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
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Returns a dataframe of standardized data.
Exports a the standardized dataframe to a tab separated file.
Exports coefficients used to standardize data to a separate tab separated file.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
Brent Murphy
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
test1 <-data.frame(x = c(1,2,3,4,5),y = c(2,4,6,8,10),z = c(15,20,30,40,50))
colnames(test1)<-c("ONE","TWO","THREE")
output_standardized(datafile = test1, col_names = c("ONE","TWO","THREE"), filename = "testfiles")
## The function is currently defined as
function (datafile, col_names, filename, should_append = FALSE) 
{
    if (is.atomic(datafile)) {
        stop("Improper datafile input: expected vector")
    }
    index <- colnames(datafile) \%in\% col_names
    num_index <- sum(index, na.rm = TRUE)
    if (num_index != length(col_names)) {
        stop("Improper col_names input: at least one column name is absent from the datafile")
    }
    datafile_subset <- datafile[, col_names]
    numeric_col <- sapply(datafile_subset, is.numeric)
    numeric_col <- sum(numeric_col, na.rm = TRUE)
    if (numeric_col != length(col_names)) {
        stop("Improper col_names input: at least one column name corresponds to a non-numeric column")
    }
    if (!is.character(filename)) {
        stop("Improper filename: expected character")
    }
    means <- colMeans(datafile_subset)
    variance <- apply(datafile_subset, 2, sd)
    datafile_subset <- apply(datafile_subset, 1, function(x) (x - 
        means)/variance)
    write.table(datafile_subset, file = filename, append = should_append, 
        sep = "\t", row.names = FALSE, col.names = FALSE)
    coeffic <- data.frame(means, variance)
    coeffic_file <- paste(filename, "coefficients")
    write.table(coeffic, file = coeffic_file, append = should_append, 
        sep = "\t", row.names = FALSE, col.names = FALSE)
    return(datafile_subset)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
