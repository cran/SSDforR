\name{ABtsplot}
\alias{ABtsplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Time series plot for SSD Data
%%  ~~function to do ... ~~
}
\description{This function builds a time series chart for a given behavior across all phases.  A space separates each phase. There are no connecting dots.
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
  \item{ABxlab}{label for x-axis between quotation marks (e.g., "week") 
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{label for y-axis between quotation marks (e.g., "amount")
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title graph between quotation mark (e.g., "Crying")
%%     ~~Describe \code{ABmain} here~~
}
}

\references{Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABtsplot(cry,pcry,"week","amount","Crying")
}
