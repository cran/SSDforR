\name{PANDabove}
\alias{PANDabove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PAND - desired values above the reference line
%%  ~~function to do ... ~~
}
\description{This effect size function evaluates the percentage of all non-overlapping Data (PAND) above the reference line in the comparison phase. Users will be prompted to enter a value for the reference line.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PANDabove(behavior, phaseX, v1, v2)
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

\references{
{Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p62
p136

Lenz, A.S. (2012). Calculating effect size in single-case research:  A comparison of nonoverlap methods.  Measurement and Evaluation in Counseling and Development, 46(1), 64-73.

Parker, R.I. & Hagan-Burker, S. & Vannest, K. (2007).  Percentage of all non-overlapping data:  An alternative to PND.  The Journal of Special Education, 40(4), 194-204.

}
{Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
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
PANDabove(esteem,pesteem,"A","B1")
}
