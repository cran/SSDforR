\name{SD1legend}
\alias{SD1legend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SD1 legend
%%  ~~function to do ... ~~
}
\description{Adds legend to SD1 band graph.  The graph can not be modified in any way after the legend is added.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SD1legend()
}
%- maybe also 'usage' for other objects documented here.

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p132

Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin, PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 
2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", 
NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
SD1(cry,pcry,"A","week","amount","Crying")
SD1legend()
}
