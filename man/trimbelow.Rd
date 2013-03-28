\name{trimbelow}
\alias{trimbelow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Chi-square below mean trim line
%%  ~~function to do ... ~~
}
\description{Chi-square test comparing the frequency of observations below the 10 percent trim mean in any two phases

}
\usage{
trimbelow(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter of first phase i.e. "A"
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{letter of second phase i.e "B"
%%     ~~Describe \code{v2} here~~
}
}

\references{go to www.ssdanalysis.com
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeiltlin Schudrich,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
trimbelow(cry,pcry,"A","B")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
