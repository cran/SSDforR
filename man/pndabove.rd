\name{PNDabove}
\alias{PNDabove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PND - desired values above the reference line
%%  ~~function to do ... ~~
}
\description{This effect size function evaluates the percentage of non-overlapping data (PND) above highest data point in the comparison phase. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PNDabove(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{comparison phase variable (e.g., "A") 
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{letter of second phase (e.g., "B")
%%     ~~Describe \code{v2} here~~
}
}

\references{
{Lenz, A.S. (2012). Calculating effect size in single-case research:  A comparison of nonoverlap methods.  Measurement and Evaluation in Counseling and Development, 46(1), 64-73.

Scruggs, T.E. & Mastropieri, M.A. (2012). PND at 25:  Past, present, and future trends in summarizing single-subject research.  Remedial and Special Education, 34(1), 9-19.

}
{Go to www.ssdanalysis.com for more information.}
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 
%%  ~~who you are~~
}

\examples{
esteem<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pesteem<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
PNDabove(esteem, pesteem,"A","B1")
}
