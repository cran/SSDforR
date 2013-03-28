\name{medbelow}
\alias{medbelow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Chi-square below median
%%  ~~function to do ... ~~
}
\description{Chi-square test comparing the frequency of observations below the median in any two phases.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
medbelow(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter for first phase i.e "A"
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{letter for second phase i.e "B"
%%     ~~Describe \code{v2} here~~
}
}

\references{go to www.ssdanalyis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin Schudrich Wurzeiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
medbelow(cry,pcry,"A","B1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
