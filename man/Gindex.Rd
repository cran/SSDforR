\name{Gindex}
\alias{Gindex}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{g-index 
%%  ~~function to do ... ~~
}
\description{The index is calculated using the proportion of scores in the desired zone  (in this example below the regression line) for the baseline and intervention. Then the baseline average is subtracted from the intervention average.   
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Gindex(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{Phase letter for baseline i.e. "A"
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{Phase letter for second phase i.e. "B"
%%     ~~Describe \code{v2} here~~
}
}


\references{Go to www.ssdanalysis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin Schudrich,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Gindex(cry,pcry,"A","B")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
