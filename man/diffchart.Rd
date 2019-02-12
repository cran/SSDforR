\name{diffchart}
\alias{diffchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Difference transformation
%%  ~~function to do ... ~~
}
\description{Produces first difference transformation on any phase.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
diffchart(behavior, phaseX, v1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter for phase to be transformed between quotation marks (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p39, p40, p65-66, p90, p142

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
diffchart(cry,pcry,"A")
}

