\name{combine}
\alias{combine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Combine trials by treatment
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
determine if a list of trials can be combined through ANOVA based on a common treatment. A critical p-value can be specified to determine if trials can be combined. If the ANOVA is insignificant, retunrs a list of the combined trials under a common, but unique trial name. Otherwise, returns false, indicating the trials cannot be combined.
}
\usage{
combine(datafile, trt, p_crit = 0.05)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{datafile}{
%%     ~~Describe \code{datafile} here~~
A list containing 'EVALUATION MEAN' dataframe, which contains 'SUMMARY TRT#', 'TRIAL #', and 'DATA' columns. 
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
\details{
%%  ~~ If necessary, more details than the description above ~~
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
