\name{PEMlegend}
\alias{PEMlegend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PEM legend
%%  ~~function to do ... ~~
}
\description{Adds legend to PEM graph
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PEMlegend()
}
%- maybe also 'usage' for other objects documented here.

\references{go to www.ssdanalysis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin Schudrich,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{

cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
#run first
PEMbelow(cry,pcry,"A","B1") 
#run after complete steps above
PEMlegend()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
