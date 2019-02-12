\name{meanbelow}
\alias{meanbelow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Chi-square  - desired values below the mean
%%  ~~function to do ... ~~
}
\description{Chi-square test comparing the frequency of observations below the reference phase mean in any two phases.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
meanbelow(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{first phase letter (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{second phase letter (e.g., "B")
%%     ~~Describe \code{v2} here~~
}
}

\references{Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin, PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2,
NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A",
NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
meanbelow(cry,pcry,"A","B1")
}
