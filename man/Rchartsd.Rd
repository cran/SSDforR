\name{Rchartsd}
\alias{Rchartsd}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SPC R-chart with SD
%%  ~~function to do ... ~~
}
\description{If the sample size is larger than ten, use of the standard deviation is recommended over the range (Orme & Cox, 2001).  Although our hypothetical data have a sample size of twelve.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Rchartsd(behavior, groupX, bandX, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{groupX}{grouping variable i.e. day
%%     ~~Describe \code{groupX} here~~
}
  \item{bandX}{number of standard deviations 1,2 or 3
%%     ~~Describe \code{bandX} here~~
}
  \item{ABxlab}{x labels
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{y labels
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title
%%     ~~Describe \code{ABmain} here~~
}
}

\references{go to ssdanalysis for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin Schudrich,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
admit<-c(85,90,80,84,82,79,75,76,80,84,75,80,79,83,88,78,80,85,83,82,89,84,89,91,87,84,77,86,80,
89,81,86,88,83,86,90,86,85,85,87,80,89,NA,86,87,88,89,79,73,75,74,70,75,81,85,75,73,75,
79,70,72,71,69,70,64,60,59,54,53,55,50,54,51,49,48,50,46,55,51,55,49,50,48,51,33)

day<-c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,
5,5,5,5,5,5,6,6,6,6,6,6,6,NA,7,7,7,7,7,7,7,8,8,8,8,8,8,8,9,
9,9,9,9,9,9,10,10,10,10,10,10,10,11,11,11,11,11,11,11,12,12,12,12,12,12,12)

padmit<-c("A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A",NA,"B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B")
Rchartsd(admit, day, 2, "week", "amount", "Admits to Hospital")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
