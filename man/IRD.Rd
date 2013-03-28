\name{IRD}
\alias{IRD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{IRD line
%%  ~~function to do ... ~~
}
\description{when using the IRD (Parker & Hagan-Burke, 2007) you will need to visually identify the smallest number of data points that would be needed to be removed to eliminate all overlap or ties between phases.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IRD(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{phase letter for first phase i.e "A"
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{phase letter for second phase i.e "B"
%%     ~~Describe \code{v2} here~~
}
}

\references{got www.ssdanalysis.com
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin Schudrich,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
IQRbandgraph(cry,pcry,"A","week","amount","Crying")
IRD(cry,pcry,"A","B")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
