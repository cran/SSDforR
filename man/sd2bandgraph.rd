\name{sd2bandgraph}
\alias{sd2bandgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{2-SD band graph base line only
%%  ~~function to do ... ~~
}
\description{2-SD band graph for the baseline only
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sd2bandgraph(behavior, phaseX, v1, ABxlab, ABylab, ABmain)
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

\references{got www.ssdanakysis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wndy Zeitlin,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
sd2bandgraph(cry,pcry,"A","week","amount","Crying")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
