\name{SD2}
\alias{SD2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{2-SD band graph
%%  ~~function to do ... ~~
}
\description{produces  graph, and output in the Console will provide values for 2 standard deviation bands and the mean through all phases.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SD2(behavior, phaseX, v1, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter of phase stats are based on
%%     ~~Describe \code{v1} here~~
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

\references{got to www.ssdanalysis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wndy Zeitlin,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
SD2(cry,pcry,"A","week","amount","Crying")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
