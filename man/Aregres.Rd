\name{Aregres}
\alias{Aregres}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Regression for  single phase 
%%  ~~function to do ... ~~
}
\description{Conducts OLS regression for any phase. Coefficients and residuals are produced. Also a simple line graph for the specified phase with a regression line is displayed in the graph window.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Aregres(behavior, phaseX, v1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{phase letter i.e. "A"
%%     ~~Describe \code{v1} here~~
}
}


\references{Got to www.ssdanalysis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin Schudrich, PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Aregres(cry,pcry,"A")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
