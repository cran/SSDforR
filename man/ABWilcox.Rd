\name{ABWilcox}
\alias{ABWilcox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Wilcoxin Rank Sum between 2 phases
%%  ~~function to do ... ~~
}
\description{performs a two-sample Wilcoxon test between any two phases.
Nonparametric test to compare means.  This test should only be used if there is not a trend in either phase AND there is no problem with autocorrelation in either phase.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABWilcox(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{first phase letter i.e. "A"
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{second phase letter i.e. "B"
%%     ~~Describe \code{v2} here~~
}
}

\references{Go to www.ssdanalysis for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin Schudrich,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABWilcox(cry,pcry,"A","B")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
