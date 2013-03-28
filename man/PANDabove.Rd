\name{PANDabove}
\alias{PANDabove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{PAND above 
%%  ~~function to do ... ~~
}
\description{  Percentage of Non-overlapping Data (PND) above the line. This type of effect size is calculated by using the percentage of data point in the intervention / treatment phase that exceeds a notable point in the baseline phase (Lenz, 2012; Scruggs & Mastropieri, 2012;  Scruggs & Mastropieri, 1998)
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PANDabove(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter for first phase i.e "A"
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{letter for second phase i.e. "B"
%%     ~~Describe \code{v2} here~~
}
}

\references{Got www.ssdanalysis.com for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin Schudrich,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
PANDabove(cry,pcry,"A","B1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
