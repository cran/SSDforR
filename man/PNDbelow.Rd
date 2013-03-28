\name{PNDbelow}
\alias{PNDbelow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PND below 
%%  ~~function to do ... ~~
}
\description{The PND (Non-overlap Methods) is based upon the percent of data points either above the lowest or highest datum point in the baseline. The data point used is based upon the direction of desired change. If the desired change is to increase the behavior, then the highest baseline value is used. On the other hand, if the desired change is decrease behavior then the lowest baseline value is used. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PNDbelow(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{
%%     ~~Describe \code{v2} here~~
}
}

\references{go to www.ssdanalysis for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin Schudrich,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
PNDbelow(cry,pcry,"A","B1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
