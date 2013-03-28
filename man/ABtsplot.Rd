\name{ABtsplot}
\alias{ABtsplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Time series plot for SSD Data
%%  ~~function to do ... ~~
}
\description{This function builds time series chart for a given behavior across all phases.  A space separates each phase. There are no connecting dots.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABtsplot(behavior, phaseX, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{ABxlab}{x label text should be between quotes i.e. "week"
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{y label text should be between quotes i.e. "amount"
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title should be between quotes i.e. "Crying"
%%     ~~Describe \code{ABmain} here~~
}
}

\references{Go to www.ssdanalysis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zietlin Schudrich, PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABtsplot(cry,pcry,"week","amount","Crying")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
