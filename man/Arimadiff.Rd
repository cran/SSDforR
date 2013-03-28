\name{Arimadiff}
\alias{Arimadiff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Difference for ARIMA
%%  ~~function to do ... ~~
}
\description{Difference any phase uses diff from {base}
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Arimadiff(behavior, phaseX, v, d)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v}{phase letter i.e. "A"
%%     ~~Describe \code{v} here~~
}
  \item{d}{integer for order of difference
%%     ~~Describe \code{d} here~~
}
}

\references{got to ssdanalysis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin Schudrich,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Arimadiff(cry,pcry,"B1",2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
