\name{ABrf2}
\alias{ABrf2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Lag-1 autocorrelation (rf2 for small sample size)
%%  ~~function to do ... ~~
}
\description{This function tests for lag-1 autocorrelation. This should be used any time the sample size is less than six. Any phase can be tested. Also produces regression line graph.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABrf2(behavior, phaseX, v1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter of phase being tested (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
}

\references{
{Huitema, B.F. & McKean, J.W. (1994). Two reduced-biased autocorrelation estimators:  rF1 and rF2.  Perceptual and Motor Skills, 78(1), 323-330.

Auerbach, C. & Schudrich, W. Z. (2013). SSD for R A Comprehensive Statistical Package to Analyze Single-System Data. Research on Social Work Practice, 23(3), 346-353. doi:10.1177/104973153477213

}
{Go to www.ssdanalysis.com for more information
}
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin Schudrich,PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABrf2(cry, pcry, "B1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
