\name{Gmedian}
\alias{Gmedian}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Median line for group data
%%  ~~function to do ... ~~
}
\description{Places median line for baseline in group boxplot.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Gmedian(behavior, phaseX, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v}{phase letter for baseline (e.g., "A")
%%     ~~Describe \code{v} here~~
}
}


\references{Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin Schudrich, PhD; Yeshiva University,  Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cohesion<-c(85,90,80,84,82,79,75,76,80,84,75,80,79,83,88,78,80,85,83,82,89,84,89,91,87,84,77,86,80,
89,81,86,88,83,86,90,86,85,85,87,80,89,NA,86,87,88,89,79,73,75,74,70,75,81,85,75,73,75,
79,70,72,71,69,70,64,60,59,54,53,55,50,54,51,49,48,50,46,55,51,55,49,50,48,51,33)

week<-c(1,1,1,1,1,1,1,2,2,2,2,2,2,2,3,3,3,3,3,3,3,4,4,4,4,4,4,4,5,
5,5,5,5,5,5,6,6,6,6,6,6,6,NA,7,7,7,7,7,7,7,8,8,8,8,8,8,8,9,
9,9,9,9,9,9,10,10,10,10,10,10,10,11,11,11,11,11,11,11,12,12,12,12,12,12,12)

pcohesion<-c("A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A","A","A","A","A","A","A",
"A","A","A","A","A","A","A","A","A","A",NA,"B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B",
"B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B","B")
ABdescrip(cohesion,week)
Gmedian(cohesion,pcohesion,"A")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
