\name{ABma}
\alias{ABma}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Moving average 
%%  ~~function to do ... ~~
}
\description{Creates moving average transformation using every two observations.  A graph is produced and the user is given the option to save the transformed data.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABma(behavior, phaseX, v1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter of phase to be transformed (e.g.,"A")
%%     ~~Describe \code{v1} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p38-39, p67, p142

Go to www.ssdanalysis.com for more information.
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
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABma(cry, pcry, "A")
}
