\name{ABplotm}
\alias{ABplotm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Mulch line plot
%%  ~~function to do ... ~~
}
\description{This function builds a multiple  line chart for a given behavior across all phases.  A space separates each phase.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABplotm(behavior, phaseX, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{ABxlab}{x label between quotes
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{y label between quotes
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title between quotes
%%     ~~Describe \code{ABmain} here~~
}
}

\references{Go to WWW.analysis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}



\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
yell<-c(3, 4, 2, 5, 5, 4, NA, 1, 2, 2, 2, 0, 0)
pyell<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B")
plotnum(2, 1)
ABplotm(cry,pcry,"week","amount","Crying")
ABplotm(yell,pyell,"week","amount","Yelling")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
