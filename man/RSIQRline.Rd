\name{RSIQRline}
\alias{RSIQRline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{IQR line for ABplot for window or Posit cloud
%%  ~~function to do ... ~~
}
\description{This function enables a user to add lines representing the interquartile range to any phase of an ABplot in window and posit. 
 

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RSIQRline(behavior, phaseX, v, l)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v}{phase letter upon which IQR stats are based - in quotation marks (e.g., "A") 
 }
 \item{l}{starting point on x axis  
%%     ~~Describe \code{v} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p50, p131


Go www.ssdanalysis.com for more information.
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
ABplot(cry,pcry,"week","amount","Crying")
# type IQRline(cry, pcry, "A")
}
