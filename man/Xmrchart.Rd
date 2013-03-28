\name{Xmrchart}
\alias{Xmrchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SPC XMR-chart
%%  ~~function to do ... ~~
}
\description{the X-mR-chart can be use to detect changes within and between phases.  This chart should not, however, be used when there is a trend in the data.  It can be used, though, when you are not using samples of data, but individual data points. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Xmrchart(behavior, phaseX, v1, bandX, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{phase bands are based upon i.e. "A"
%%     ~~Describe \code{v1} here~~
}
  \item{bandX}{number of SDs i.e 1,2,or 3
%%     ~~Describe \code{bandX} here~~
}
  \item{ABxlab}{x labels
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{y labels
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title
%%     ~~Describe \code{ABmain} here~~
}
}

\references{go to www.ssdanalysis.com
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wndy Zeitlin,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Xmrchart(cry, pcry, "A", 2, "week", "amoun", "X-MR-chart")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
