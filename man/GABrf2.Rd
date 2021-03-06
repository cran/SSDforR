\name{GABrf2}
\alias{GABrf2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Autocorrelation for group data
%%  ~~function to do ... ~~
}
\description{This function tests for lag-1 autocorrelation for group data. This should be used any time the sample size is less than six. Any phase can be tested. Also produces regression line graph.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GABrf2(behavior, phaseX, timeX, v1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{timeX}{time variable (e.g., week)
%%     ~~Describe \code{timeX} here~~
}
  \item{v1}{letter of phase being tested  in quotation marks (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p99, p101, p147

{Huitema, B.F. & McKean, J.W. (1994). Two reduced-biased autocorrelation estimators:  rF1 and rF2.  Perceptual and Motor Skills, 78(1), 323-330.

}
{Go to www.ssdanalysis.com for more information.
}
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 
%%  ~~who you are~~
}

\examples{
attend<-c(0,0,0,1,0,0,1,0,0,1,0,0,1,0,1,0,0,0,0,0,1,1,0,0,1,NA,
0,1,1,0,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

week<-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,NA,6,6,6,6,6,7,7,7,7,7,
8,8,8,8,8,9,9,9,9,9,10,10,10,10,10,11,11,11,11,11,12,12,12,12,12,13,
13,13,13,13,14,14,14,14,14,15,15,15,15,15)

pattend<-c("A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A",
"A","A","A",NA,"B","B","B","B","B","B","B","B","B","B","B","B","B","B","B"
,"B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B")
# now run: GABrf2(attend,pattend,week,"A")
}
