\name{RSTrimline}
\alias{RSTrimline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Trimmed mean line added to ABplot
%%  ~~function to do ... ~~
}
\description{Adds trimmed mean line to ABplot. Click in the phase of the ABplot to add line.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RSTrimline(behavior, phaseX, v,l)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v}{letter of phase for which trimmed mean is desired in quotation marks (e.g., "A")
%%     ~~Describe \code{v} here~~
}
\item{l}{start point on x axis
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p48, p130-131

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
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA,
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABplot(cry,pcry,"week","amount","Crying")
# now run RSTrimline(cry,pcry,"A",1)
}

