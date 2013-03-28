\name{robregabove}
\alias{robregabove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Chi-square above Robust regression line
%%  ~~function to do ... ~~
}
\description{Chi-square test comparing the frequency of observations above the robust regression line in any two phases.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
robregabove(behavior, phaseX, v1, v2)
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

\references{go to www.ssdanalysis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin Schudrich,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
robregabove(cry,pcry,"A","B1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
