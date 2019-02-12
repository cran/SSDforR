\name{SDAband}
\alias{SDAband}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Adds standard deviation bands to an ABplot
%%  ~~function to do ... ~~
}
\description{Adds standard deviation bands to an ABplot. Click in the phase twice to add upper and lower bands.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SDAband(behavior, phaseX, v, bandX)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v}{phase band is based upon (e.g., "A")
%%     ~~Describe \code{v} here~~
}
  \item{bandX}{number of standard deviations desired (e.g., 2) 
%%     ~~Describe \code{bandX} here~~
}
}


\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p131

Go to www.ssdnalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin, PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABplot(cry,pcry,"week","amount","Crying")
# now run SDAband(cry,pcry,"A",2)
}
