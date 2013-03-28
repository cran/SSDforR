\name{PNDbelow}
\alias{PNDbelow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PND - desired values below the reference line 
%%  ~~function to do ... ~~
}
\description{This effect size function evaluates the percentage of non-overlapping data (PND)  below the lowest data point in the comparison phase. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PNDbelow(behavior, phaseX, v1, v2)
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
  \item{v2}{second phase variable (e.g., "B")
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
\author{Charles Auerbach, PhD & Wendy Zeitlin Schudrich, PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
PNDbelow(cry,pcry,"A","B1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
