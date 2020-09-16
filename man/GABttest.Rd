\name{GABttest}
\alias{GABttest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{T-test for group data
%%  ~~function to do ... ~~
}
\description{Computes t-test for group data.  A bar graph showing the mean for each phase is displayed.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
GABttest(behavior, phaseX, timeX, v1, v2)
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
  \item{v1}{first phase letter between quotation marks (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{second phase letter between quotation marks (e.g., "B")
%%     ~~Describe \code{v2} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p104, p108

Go to www.ssdanalysis.com for more information.
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

week<-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,NA,6,6,
6,6,6,7,7,7,7,7,
8,8,8,8,8,9,9,9,9,9,10,10,10,10,10,11,11,11,11,11,12,
12,12,12,12,13,
13,13,13,13,14,14,14,14,14,15,15,15,15,15)

pattend<-c("A","A","A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A","A","A",
"A","A","A",NA,"B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B","B","B"
,"B","B","B","B")
# now run GABttest(attend, pattend, week, "A", "B")
}
