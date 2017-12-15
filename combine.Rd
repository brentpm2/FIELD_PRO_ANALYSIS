\name{combine}
\alias{combine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
combine(datafile, trt, p_crit = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datafile}{
%%     ~~Describe \code{datafile} here~~
}
  \item{trt}{
%%     ~~Describe \code{trt} here~~
}
  \item{p_crit}{
%%     ~~Describe \code{p_crit} here~~
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
function (datafile, trt, p_crit = 0.05) 
{
    if (is.atomic(datafile)) {
        stop("Improper datafile, expected a list")
    }
    if (is.null(datafile$"EVALUATION MEAN")) {
        stop("Improper datafile, requires EVALUATION MEAN entry in provided list")
    }
    if (is.null(datafile$`EVALUATION MEAN`$`SUMMARY TRT#`)) {
        stop("Improper datafile, requires 'SUMMARY TRT# within EVALUATION MEAN in provided list")
    }
    if (is.null(datafile$`EVALUATION MEAN`$`TRIAL #`)) {
        stop("Improper datafile, requires 'TRIAL #' within EVaLUATION MEAN in provided list")
    }
    if (length(datafile$`EVALUATION MEAN`$`SUMMARY TRT#` == trt) <= 
        1) {
        stop("Improper common treatment, present only in one or fewer entries in datafile")
    }
    if (p_crit > 1 || p_crit < 0) {
        stop("Improper p_critical value. P value must be between 0 and 1.")
    }
    datafilesubset <- datafile$`EVALUATION MEAN`[datafile$`EVALUATION MEAN`$`SUMMARY TRT#` == 
        trt, ]
    index <- datafilesubset$`TRIAL #`
    fulltrials <- datafile$`EVALUATION MEAN`[datafile$`EVALUATION MEAN`$`TRIAL #` \%in\% 
        index, ]
    colnames(fulltrials)[3] <- "summarytrt"
    colnames(fulltrials)[1] <- "trialnum"
    my_anova <- aov(DATA ~ summarytrt + trialnum, data = fulltrials)
    p_values <- summary(my_anova)[[1]][["Pr(>F)"]]
    can_combine <- p_values[2] > p_crit
    if (can_combine) {
        index[[length(index) + 1]] <- as.numeric(Sys.time())
        is_duplicated <- duplicated(index)
        if (is_duplicated[length(index)]) {
            index[[length(index)]] <- make.unique(rep(index[[length(index) + 
                1]], 2))[2]
        }
        fulltrials$trialnum <- rep(index[[length(index)]], nrow(fulltrials))
        return(fulltrials)
    }
    else {
        return(can_combine)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
