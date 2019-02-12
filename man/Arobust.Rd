\name{Arobust}
\alias{Arobust}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Robust regression for a single phase
%%  ~~function to do ... ~~
}
\description{Uses MASS package developed by Brian Ripley <ripley@stats.ox.ac.uk> to
conduct robust regression for a single phase. Coefficients and residuals are produced. Also a graph with a regression line is displayed for the phase in the graph window.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Arobust(behavior, phaseX, v1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{phase letter (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
}

\references{Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p143

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
Arobust(cry,pcry,"A")
}
