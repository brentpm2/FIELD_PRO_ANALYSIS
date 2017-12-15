\name{weather_response_graph}
\alias{weather_response_graph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Weather_response_graph
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Read in an xls file outputted by MTA for FieldPro and give a graph of a chosen type based off of a selected treatment name.
}
\usage{
weather_response_graph(filename, trialname, graphtype = "p", sheetnumber = 3, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{filename}{
The XLS file from MTA that will be read in.
}
  \item{trialname}{
The name of the trial that the graph will be made from
}
  \item{graphtype}{
The type of plot that will be drawn:
    "p" for a scatterplot with an R squared value,
    "l" for a plot with just lines,
    "b" for both lines and points,
    "c" for lines with blank spaces where the points would be,
    "o" for lines going through the points,
    "h" for a histogram of the response variable,
    "s" for a step plot,
    "n" for no plot.
}
  \item{sheetnumber}{
The sheet number for the EVALUATION MEANS data.  The standard output from MTA is the third sheet.
}
  \item{\dots}{
Arguments to be passed to methods, such as colors or size.  
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
A graph of the selected type given by the user.  If no plot type is selected, a scatterplot will be returned.
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
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
