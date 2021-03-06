\name{SPClegend}
\alias{SPClegend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SPC legend
%%  ~~function to do ... ~~
}
\description{Adds a legend to any of the SPC charts. The chart can not be modified in any way after the legend is added.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SPClegend()
}
%- maybe also 'usage' for other objects documented here.

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p71, p73, p75, p76, p77, p105, p06, p141

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
admit<-c(85,90,80,84,82,79,75,76,80,84,75,80,79,83,88,78,80,85,83,82,89,84,89,
91,87,84,77,86,80,
89,81,86,88,83,86,90,86,85,85,87,80,89,NA,
86,87,88,89,79,73,75,74,70,75,81,85,75,73,75,
79,70,72,71,69,70,64,60,59,54,53,55,50,54,51,49,
48,50,46,55,51,55,49,50,48,51,33)

day<-c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,
5,5,5,5,5,5,6,6,6,6,6,6,6,NA,7,7,7,7,7,7,7,8,8,8,8,8,8,8,9,
9,9,9,9,9,9,10,10,10,10,10,10,10,11,11,11,11,11,11,11,
12,12,12,12,12,12,12)

padmit<-c("A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A",
"A","A","A","A","A","A","A","A","A","A",NA,
"B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B")
Rchartsd(admit, day, 2, "week", "amount", "Admits to Hospital")
SPClegend()
}

