\name{plotnum}
\alias{plotnum}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Set graphic environment
%%  ~~function to do ... ~~
}
\description{Used prior to ABplotm to set up graphic environment
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
plotnum(nr, nc)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{nr}{number of rows i.e 2
%%     ~~Describe \code{nr} here~~
}
  \item{nc}{number of columns i.e 1
%%     ~~Describe \code{nc} here~~
}
}

\references{go to www.ssdanalysis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin Schudrich,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
yell<-c(3, 4, 2, 5, 5, 4, NA, 1, 2, 2, 2, 0, 0)
pyell<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B")
plotnum(2, 1)
ABplotm(cry,pcry,"week","amount","Crying")
ABplotm(yell,pyell,"week","amount","Yelling")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
