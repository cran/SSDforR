\name{CDCabove}
\alias{CDCabove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Conservative Dual Criteria (CDC) desired zone above lines 
%%  ~~function to do ... ~~
}
\description{The conservative dual-criteria (CDC) is a relatively new approach to comparing phases that works well when data have a moderate lag-1 autocorrelation (lower than 0.6).  This function uses two lines to define the desired zone:  the mean and the regression line of the comparison phase. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
CDCabove(behavior, phaseX, v1, v2)
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

Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p85, p143

  {Fisher, W.W., Kelley, M.E. & Lomas, J.E. (2003).  Visual aids and structured criteria for improving visual inspection and interpretation of single-case designs.  Journal of Applied Behavior Analysis, 36(3), 387-406.
  
}
  {Go to www.ssdanalysis.com for more information.
}
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin, PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA,
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
CDCabove(cry,pcry,"A","B")
}

