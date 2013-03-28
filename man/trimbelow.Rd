\name{trimbelow}
\alias{trimbelow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Chi-square - desired values below the trimmed mean
%%  ~~function to do ... ~~
}
\description{Chi-square test comparing the frequency of observations below the reference phase trimmed mean in any two phases.

}
\usage{
trimbelow(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{first phase letter in quotation marks (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{second phase letter in quotation marks (e.g., "B")
%%     ~~Describe \code{v2} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p84, p145

Go to www.ssdanalysis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin Schudrich, PhD; Yeshvia University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
trimbelow(cry,pcry,"A","B")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
