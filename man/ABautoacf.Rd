\name{ABautoacf}
\alias{ABautoacf}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Autocorrelation at any lag for a phase
%%  ~~function to do ... ~~
}
\description{This function tests for autocorrelation for any lag. Should be used with samples greater than or equal to six. Also produces significance graph for lags. The Box-Ljung test of significance is performed for all lags up to and including the specified one. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABautoacf(behavior, phaseX, v, l)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v}{letter for phase tested (e.g., "A")
%%     ~~Describe \code{v} here~~
}
  \item{l}{number of lags (e.g. 1, 2, 3) 
%%     ~~Describe \code{lags} here~~
}
}
\references{Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~

Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p42 ,p141 }
\author{Charles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 
}
%%  ~~who you are~~


\examples{cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABautoacf(cry, pcry, "B", 2)
}
