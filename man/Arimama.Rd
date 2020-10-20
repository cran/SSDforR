\name{Arimama}
\alias{Arimama}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Moving average for ARIMA 
%%  ~~function to do ... ~~
}
\description{Moving average over any period for ARIMA.  Uses TTR Package.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Arimama(behavior, phaseX, v, m)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v}{phase letter in quotation markse (e.g., "A")
%%     ~~Describe \code{v} here~~
}
  \item{m}{number of periods to average over (e.g., 2)
%%     ~~Describe \code{m} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p142-143 

Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{CCharles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Arimama(cry,pcry,"B1",2)
}
