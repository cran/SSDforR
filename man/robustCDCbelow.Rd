\name{RobustCDCbelow}
\alias{RobustCDCbelow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Robust Conservative Dual Criteria (CDC) using robust regression desired zone below lines
%%  ~~function to do ... ~~
}
\description{The conservative dual-criteria (CDC) is a relatively new approach to comparing phases that works well when data have a moderate lag-1 autocorrelation (lower than 0.6).  This function uses two lines to define the desired zone:  the mean and the robust regression line of the comparison phase. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RobustCDCbelow(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{phase letter of first (i.e., comparison) phase between quotation marks (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{phase letter of second phase between quotation marks (e.g., "B")
%%     ~~Describe \code{v2} here~~
}
}

\references{
{Fisher, W.W., Kelley, M.E. & Lomas, J.E. (2003).  Visual aids and structured criteria for improving visual inspection and interpretation of single-case designs.  Journal of Applied Behavior Analysis, 36(3), 387-406.
  
}
{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p86, p144
Go to www.ssdanalysis.com for more information.}
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
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
RobustCDCbelow(cry,pcry,"A","B")
}
