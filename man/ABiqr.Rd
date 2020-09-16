\name{ABiqr}
\alias{ABiqr}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Interquartile band graph through all phases
%%  ~~function to do ... ~~
}
\description{Builds an iqr band graph through all phases based upon the user's selection of a phase
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABiqr(behavior, phaseX, v1, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter of phase statistics are based upon in quotation marks
%%     ~~Describe \code{v1} here~~
}
  \item{ABxlab}{label for x-axis in quotation markes
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{label for y-axis in quotation marks
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title in quotation marks
%%     ~~Describe \code{ABmain} here~~
}
}
\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p133

Go to www.ssdanalysis.com for more information.
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
# ABiqr(cry,pcry,"week","amount","Crying")

}
