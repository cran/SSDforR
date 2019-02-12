\name{ABbinomial}
\alias{ABbinomial}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Binomial test
%%  ~~function to do ... ~~
}
\description{
Binomial test comparing the number of observations of a phase in a desired zone to another phase. User needs to select method for defining a desired zone (e.g., below one SD).
}
\usage{
ABbinomial(phaseX, v1, v2, successA, successB)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter of first phase (e.g.,"A")
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{letter of second phase (e.g., "B")
%%     ~~Describe \code{v2} here~~
}
  \item{successA}{occurrences in desired zone for first phase
%%     ~~Describe \code{successA} here~~
}
  \item{successB}{occurrences in desired zone for second phase
%%     ~~Describe \code{successB} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p143

Go to www.ssdanalysis.com for more information.
}
\author{Charles Auerbach, PhD & Wendy Zeitlin, PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
SD1(cry,pcry,"A","week","amount","Crying")
ABbinomial(pcry,"A","B1", 1, 8)
}
