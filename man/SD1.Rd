\name{SD1}
\alias{SD1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{1-SD band 
%%  ~~function to do ... ~~
}
\description{produces  graph, and output in the Console will provide values for 1 standard deviation bands and the mean through all phases.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SD1(behavior, phaseX, v1, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter for phase variable stats are base upon i.e. "A"
%%     ~~Describe \code{v1} here~~
}
  \item{ABxlab}{x label
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{y labels
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title
%%     ~~Describe \code{ABmain} here~~
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
SD1(cry,pcry,"A","week","amount","Crying")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
