\name{combine}
\alias{combine}
\title{
%%  ~~function to do ... ~~
Combine trials by treatment
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
determine if a list of trials can be combined through ANOVA based on a common treatment. A critical p-value can be specified to determine if trials can be combined. If the ANOVA is insignificant, retunrs a list of all combined treatments under a unique name under column 'TRIAL #'. Otherwise, returns false, indicating the trials cannot be combined.
}
\usage{
combine(datafile, trt, p_crit = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datafile}{
%%     ~~Describe \code{datafile} here~~
A list containing 'EVALUATION MEAN' dataframe, which contains 'SUMMARY TRT#', 'TRIAL #', and 'DATA' columns. The list and containing dataframes are required to formatted as the output for the function 'readallsheets'.
}
  \item{trt}{
%%     ~~Describe \code{trt} here~~
A numeric value corresponding to at least one entry within the 'SUMMARY TRT#' within 'EVALUATION MEAN' dataframe provided in datafile list. Respresents the common treatment combining all trials containing this treatment.
}
  \item{p_crit}{
%%     ~~Describe \code{p_crit} here~~
A numeric value 1>p_crit>0, denoting the p-value which delimits a significant and non-significant ANOVA.
}
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
Should the conducted ANOVA be significant, then a boolean 'FALSE' is returned.
Should the conducted ANOVA be insignificant, then a dataframe containing the combined trials under a common, but unique trial name.
}
\author{
%%  ~~who you are~~
Brent Murphy
}

\examples{
path1<-system.file("extdata", 
                   "YIELDALL_MTA_FLATFILE.xls", 
                    package = "fieldproanalysis")
mysheets<-readallsheets(path1,"example.csv")
combine(mysheets,515)
}
