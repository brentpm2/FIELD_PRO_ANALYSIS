\name{plot_by_component}
\alias{plot_by_component}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plot_by_component(datafile, treatment_list, component, trial = "", resp_var = "DATA")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datafile}{
%%     ~~Describe \code{datafile} here~~
}
  \item{treatment_list}{
%%     ~~Describe \code{treatment_list} here~~
}
  \item{component}{
%%     ~~Describe \code{component} here~~
}
  \item{trial}{
%%     ~~Describe \code{trial} here~~
}
  \item{resp_var}{
%%     ~~Describe \code{resp_var} here~~
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
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
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
function (datafile, treatment_list, component, trial = "", resp_var = "DATA") 
{
    if (!is.data.frame(datafile)) {
        stop("Improper datafile input: expected a dataframe")
    }
    if (!is.data.frame(treatment_list)) {
        stop("Improper treatment_list input: expected dataframe")
    }
    if (!is.character(component)) {
        stop("Improper component input: expected string/character")
    }
    if (is.null(datafile[resp_var, ])) {
        stop("Improper datafile and resp_var combination: specified variable is not present within datafile")
    }
    if (is.null(treatment_list$"TREATMENT COMPONENT")) {
        stop("Improper treatment_list: expected column labeled 'TREATMENT COMPONENT'")
    }
    if (is.null(treatment_list$"SUMMARY TRT #")) {
        stop("Improper treatmetn_list: expected column labeled 'SUMMARY TRT#'")
    }
    if (is.null(datafile$"SUMMARY TRT#")) {
        stop("improper datafile: expected column labeled 'SUMMARY TRT#")
    }
    valid_treatments <- treatment_list[treatment_list$"TREATMENT COMPONENT" == 
        component, "SUMMARY TRT #"]
    if (length(valid_treatments) < 1) {
        stop("Improper component: component not present within treatment_list")
    }
    if (trial == "") {
        datafile <- datafile[datafile$`TRIAL #` == datafile$`TRIAL #`[1], 
            ]
    }
    else {
        datafile <- datafile[datafile$`TRIAL #` == trial, ]
    }
    valid_treatments <- str_extract(valid_treatments, "^[[:digit:]]+")
    datafile_subset <- datafile[datafile$`SUMMARY TRT#` \%in\% 
        valid_treatments, ]
    datafile_subset <- aggregate(x = datafile_subset, by = list(datafile_subset$"SUMMARY TRT#"), 
        mean)
    output_heights <- datafile_subset[, resp_var]
    pdf_title <- paste("Effect of Treatment containing '", component, 
        "' on Yield", sep = "")
    pdf(paste(pdf_title, ".pdf", sep = ""))
    barplot(height = output_heights, names.arg = datafile_subset$"SUMMARY TRT#", 
        xlab = "SUMMARY TRT#", ylab = resp_var, main = pdf_title)
    dev.off()
    write.table(datafile_subset, file = paste(pdf_title, ".txt", 
        sep = ""))
    return(datafile_subset)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
