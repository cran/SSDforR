\name{ABautopacf}
\alias{ABautopacf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Partial autocorrelation
%%  ~~function to do ... ~~
}
\description{This function tests for partial autocorrelation for any lag. Should be used with samples greater than or equal to six. Also produces significance graph for lags. The Box-Ljung test of significance is performed for all lags up to and including the specified one. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABautopacf(behavior, phaseX, v, lags)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v}{letter for phase being tested (e.g.,"A")
%%     ~~Describe \code{v} here~~
}
  \item{lags}{number of lags (e.g., 1, 2, 3)
%%     ~~Describe \code{lags} here~~
}
}

\references{
Go to www.ssdanalysis.com for more information.
}
\author{Charles Auerbach, PhD & Wendy Zeitlin Schudrich, PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABautopacf (cry, pcry,"A", 3)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
