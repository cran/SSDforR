\name{PEMlegend}
\alias{PEMlegend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PEM legend
%%  ~~function to do ... ~~
}
\description{Adds a legend to a PEM graph.  The graph can not be modified in any way after the legend is added.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PEMlegend()
}
%- maybe also 'usage' for other objects documented here.

\references{Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 
%%  ~~who you are~~
}

\examples{

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
#run first
PEMbelow(cry,pcry,"A","B1") 
#run after complete steps above
PEMlegend()

}
