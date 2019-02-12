\name{robregabove}
\alias{robregabove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Chi-square - desired values above robust regression line
%%  ~~function to do ... ~~
}
\description{Chi-square test comparing the frequency of observations above the robust regression line in a comparison phase to another phase.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
robregabove(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter of comparsion phase in quotation marks (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{letter of second phase in quotation marks (e.g., "B")
%%     ~~Describe \code{v2} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p85, p146 

Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin, PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
robregabove(cry,pcry,"A","B1")
}

