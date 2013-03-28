\name{Cchart}
\alias{Cchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SPC C-Chart
%%  ~~function to do ... ~~
}
\description{This function builds a c-chart and is used with individual (i.e., ungrouped) data.  A space separates each phase.  For use when the outcome variable is a count (i.e., ratio level) variable.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Cchart(behavior, phaseX, v1, bandX, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{phase letter mean SD are base upon
%%     ~~Describe \code{v1} here~~
}
  \item{bandX}{number of standard deviation i.e. 2 
%%     ~~Describe \code{bandX} here~~
}
  \item{ABxlab}{x label between quotes "weeks"
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{y label between quotes "amount"
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title
%%     ~~Describe \code{ABmain} here~~
}
}


\references{Go to ssdanalysis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD and Wendy Zeitlin Schudrich,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Cchart(cry,pcry,"A",2,"week","amount","Crying")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
