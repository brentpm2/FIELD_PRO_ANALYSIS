\name{response_trt_graph}
\alias{response_trt_graph}

\title{
Response by Treatment Graphs

}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Read in an xls file outputted by MTA for FieldPro and give a graph of response variable by treatment number.  The type of graph returned can be selected by the user.
}
\usage{
response_trt_graph(filename, trialname, graphtype = "p", sheetnumber = 3, ...)
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

require(readxl)  ## for read_excel
## Scatterplot of response variable by treatments
response_trt_graph("YIELDALL_MTA_FLATFILE.xls","US 101/10/01 002 01")

## Histogram of the response variables
response_trt_graph("YIELDALL_MTA_FLATFILE.xls","US 101/10/01 002 01", graphtype = h)
}
