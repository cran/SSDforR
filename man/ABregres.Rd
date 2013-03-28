\name{ABregres}
\alias{ABregres}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{OLS regression to compare trends between phases
%%  ~~function to do ... ~~
}
\description{Conducts OLS regression comparing any two phases. Coefficients and residuals are produced for each phase. Also a graph with a regression line is displayed for each phase in the graph window.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABregres(behavior, phaseX, v1, v2)
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
  \item{v2}{second phase letter i.e "B"
%%     ~~Describe \code{v2} here~~
}
}


\references{Go to www.ssdanalysis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeiltlin Schudrich Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABregres(cry,pcry,"A","B")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
