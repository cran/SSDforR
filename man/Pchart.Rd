\name{Pchart}
\alias{Pchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SPC P-chart
%%  ~~function to do ... ~~
}
\description{This function builds a P-chart and can be used when the target behavior has a binary outcome.  This chart allows for a comparison of the proportion of tasks completed over time or between phases.  A space separates each phase.    
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Pchart(behavior, groupX, bandX, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{groupX}{grouping variable
%%     ~~Describe \code{groupX} here~~
}
  \item{bandX}{number of standard deviations desired (e.g., 2)
%%     ~~Describe \code{bandX} here~~
}
  \item{ABxlab}{label for x-axis between quotation marks (e.g., "weeks")
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{label for y-axis between quotation marks (e.g., "attendance"")
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title for chart between quotation marks (e.g., "Attendance Over Time")
%%     ~~Describe \code{ABmain} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p75, p139-140

{Orme, J. & Cox, M.E. (2001). Analyzing single-subject design data using statistical proces control charts.  Social Work Research, 25(2), 115-127.

}
{Go to www.ssdanalysis.com for more information.}
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin, PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
attend<-c(0,0,0,1,0,0,1,0,0,1,0,0,1,0,1,0,0,0,0,0,1,1,0,0,1,NA,
0,1,1,0,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

day<-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,NA,6,6,
6,6,6,7,7,7,7,7,
8,8,8,8,8,9,9,9,9,9,10,10,10,10,10,11,11,11,11,
11,12,12,12,12,12,13,
13,13,13,13,14,14,14,14,14,15,15,15,15,15)
Pchart(attend, day, 2, "week", "amount", "Group attendance")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
