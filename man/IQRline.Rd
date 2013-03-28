\name{IQRline}
\alias{IQRline}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{IQR line for ABplot
%%  ~~function to do ... ~~
}
\description{This function enables a user to add lines representing the interquartile range to any phase of an ABplot. 
 

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IQRline(behavior, phaseX, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v}{phase letter upon which IQR stats are based - in quotation marks (e.g., "A") 
%%     ~~Describe \code{v} here~~
}
}

\references{Go www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin Schudrich, PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABplot(cry,pcry,"week","amount","Crying")
# type IQRline(cry, pcry, "A")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
