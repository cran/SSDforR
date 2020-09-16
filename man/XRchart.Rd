\name{XRchart}
\alias{XRchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SPC XR-Chart
%%  ~~function to do ... ~~
}
\description{This chart can be used when there are multiple observations per sample and uses the mean of each sample to create the chart.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
XRchart(behavior, groupX, bandX, ABxlab, ABylab, ABmain)
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
  \item{ABxlab}{x-axis label in quotation marks (e.g., "week")
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{y-axis label in quotation marks (e.g., "mean amount")
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title for chart in quotation marks (e.g., "Admits to Hospital")
%%     ~~Describe \code{ABmain} here~~
}
}

\references{

Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p71, p105

{Orme, J. & Cox, M.E. (2001). Analyzing single-subject design data using statistical proces control charts.  Social Work Research, 25(2), 115-127.

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
admit<-c(85,90,80,84,82,79,75,76,80,84,75,80,79,83,88,78,80,85,83,
82,89,84,89,91,87,84,77,86,80,
89,81,86,88,83,86,90,86,85,85,87,80,89,NA,86,87,88,89,79,73,75,
74,70,75,81,85,75,73,75,
79,70,72,71,69,70,64,60,59,54,53,55,50,54,51,49,48,50,46,55,51,
55,49,50,48,51,33)

day<-c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,
5,5,5,5,5,5,6,6,6,6,6,6,6,NA,7,7,7,7,7,7,7,8,8,8,8,8,8,8,9,
9,9,9,9,9,9,10,10,10,10,10,10,10,11,11,11,11,11,11,11,12,
12,12,12,12,12,12)

padmit<-c("A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A",NA,"B","B",
"B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B")
XRchart(admit, day, 2, "week", "amount", "Admits to Hospital")
}
