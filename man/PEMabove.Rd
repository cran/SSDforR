\name{PEMabove}
\alias{PEMabove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PEM - desired values above the reference line
%%  ~~function to do ... ~~
}
\description{Percentage of Data Exceeding the Median (PEM). The PEM procedure offers a method to assess effect size and adjust for the influence of outliers in the baseline phase when desired values are above the reference line.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PEMabove(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{first phase variable (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{second phase variable (e.g., "B")
%%     ~~Describe \code{v2} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p60, p137

{Lenz, A.S. (2012). Calculating effect size in single-case research:  A comparison of nonoverlap methods.  Measurement and Evaluation in Counseling and Development, 46(1), 64-73.

Ma, H-H. (2009). The effectiveness of intervention on the behavior of individuals with autism:  A meta-analysis using percentage of data points exceeding the median of baseline phase.  Behavior Modification, 33(3), 339-359.

}
{
Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
}
\author{Charles Auerbach, PhD & Wendy Zeitlin,PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
esteem<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pesteem<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
PEMabove(esteem,pesteem,"A","B1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
