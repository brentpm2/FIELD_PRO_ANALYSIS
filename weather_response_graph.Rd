\name{weather_response_graph}
\alias{weather_response_graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
weather_response_graph(filename, trialname, x, y, graphtype = "p", sheetnumber = 3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
%%     ~~Describe \code{filename} here~~
}
  \item{trialname}{
%%     ~~Describe \code{trialname} here~~
}
  \item{x}{
%%     ~~Describe \code{x} here~~
}
  \item{y}{
%%     ~~Describe \code{y} here~~
}
  \item{graphtype}{
%%     ~~Describe \code{graphtype} here~~
}
  \item{sheetnumber}{
%%     ~~Describe \code{sheetnumber} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
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
function (filename, trialname, x, y, graphtype = "p", sheetnumber = 3, 
    ...) 
{
    graphfile <- read_xls(filename, sheet = sheetnumber)
    if (!"TRIAL TRT#" \%in\% names(graphfile)) {
        stop("Excel File is missing TRIAL TRT# column")
    }
    if (!"DATA" \%in\% names(graphfile)) {
        stop("Excel File is missing DATA column")
    }
    if (!is.numeric(graphfile$"TRIAL TRT#")) {
        stop("The trial treatment number column needs to be numeric")
    }
    if (!is.numeric(graphfile$DATA)) {
        stop("The Data column needs to be numeric")
    }
    xvar <- graphfile$"TRIAL TRT#"[graphfile$"TRIAL #" == trialname]
    yvar <- graphfile$DATA[graphfile$"TRIAL #" == trialname]
    if (graphtype == "p") {
        plot(xvar, yvar, xlab = "Trial treatment number", ylab = "Yield kg/hectare", 
            main = trialname, type = graphtype, ...)
        abline(fit <- lm(yvar ~ xvar))
        legend("topright", bty = "n", legend = paste("Adjusted R^2", 
            format(summary(fit)$adj.r.squared, digits = 4)))
    }
    else {
        if (graphtype == "h") {
            hist(yvar, xlab = "Yield kg/hectare", ...)
        }
        else {
            plot(xvar, yvar, xlab = "Trial treatment number", 
                ylab = "Yield kg/hectare", main = trialname, 
                type = graphtype, ...)
        }
    }
    graphfile
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
