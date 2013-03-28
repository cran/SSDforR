\name{Trimline}
\alias{Trimline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Trim mean line added to ABplot
%%  ~~function to do ... ~~
}
\description{Adds Trim mean line to ABplot. Click on phase to add line
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Trimline(behavior, phaseX, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v}{letter of phase trim mean is base upon
%%     ~~Describe \code{v} here~~
}
}

\references{go to www.ssdanalysis for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wndy Zeitlin,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABplot(cry,pcry,"week","amount","Crying")
# now run Trimline(cry,pcry,"A")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
