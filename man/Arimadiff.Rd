\name{Arimadiff}
\alias{Arimadiff}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Difference for ARIMA
%%  ~~function to do ... ~~
}
\description{Differencing in any phase.  Graphs display both original data and differenced data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Arimadiff(behavior, phaseX, v, d)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v}{phase letter in quotation marks (e.g., "A")
%%     ~~Describe \code{v} here~~
}
  \item{d}{integer for order of difference
%%     ~~Describe \code{d} here~~
}
}

\references{Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p142

Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin, PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Arimadiff(cry,pcry,"B1",2)
}
