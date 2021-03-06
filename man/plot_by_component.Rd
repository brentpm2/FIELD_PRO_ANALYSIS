\name{plot_by_component}
\alias{plot_by_component}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Plot by Treatment component
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Constructs a bar chart of a provided response variable for all treatments containing a specified component within a trial. Exports graph to pdf only. Exports data used to generate graph to tab separated file.
}
\usage{
plot_by_component(datafile, 
                  treatment_list, 
                  component, 
                  trial = "", 
                  resp_var = "DATA")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datafile}{
%%     ~~Describe \code{datafile} here~~
A dataframe containing the column specified as resp_var and 'SUMMARY TRT#', and 'TRIAL #'. This dataframe is contained within the output of the 'readallsheets' function.
}
  \item{treatment_list}{
%%     ~~Describe \code{treatment_list} here~~
A dataframe containing the columns 'TREATMENT COMPONENT', one of which corresponding to component, and 'SUMMARY TRT #'. This dataframe is contained within the output of 'readallsheets' function.
}
  \item{component}{
%%     ~~Describe \code{component} here~~
A character string corresponding to at least one entry within the treatment_list column 'TREATMENT COMPONENT'.
}
  \item{trial}{
%%     ~~Describe \code{trial} here~~
A character string corresponding to an entry within the 'TRIAL #' column of dataframe. Optional, as not specifying a value will result in the first trial being used.
}
  \item{resp_var}{
%%     ~~Describe \code{resp_var} here~~
A character string corresponding to the column header present within dataframe, which contains numeric data.
}
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
returns an aggregated datafile subsetted to treatments which contain the specified component, aggregating multiple instances of a given treatment.
}
\author{
%%  ~~who you are~~
Brent Murphy
}
\examples{
trtlist <- data.frame("SUMMARY TRT #"=c("1-A","1-B",
                                        "2-A","2-B","3-A"),
                      "TREATMENT COMPONENT" = c("a","b","a",
                                              "c","b"))
names(trtlist) <- c("SUMMARY TRT #","TREATMENT COMPONENT")
datafile <- data.frame("DATA" = c(10,9,8,7,6,5,4),
                       "SUMMMARY TRT#" = c(1,2,3,4,5,1,1),
                       "TRIAL #" = c(1,1,1,1,1,1,2))
names(datafile) <- c("DATA","SUMMARY TRT#","TRIAL #")
plot_by_component(datafile = datafile,
                  treatment_list = trtlist,
                  component = "a",
                  resp_var = "DATA")
}
