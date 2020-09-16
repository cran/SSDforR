\name{ABrobust}
\alias{ABrobust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Robust regression 
%%  ~~function to do ... ~~
}
\description{Uses MASS package developed by Brian Ripley <ripley@stats.ox.ac.uk> to
conduct robust regression comparing any two phases. Coefficients and residuals are produced for each phase. Also a graph with a regression line is displayed for each phase in the graph window.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABrobust(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter of first phase in quotation marks (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{letter of second phase in quotation marks (e.g., "B")
%%     ~~Describe \code{v2} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p135 

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
ABrobust(cry,pcry,"A","B")
}
