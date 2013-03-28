\name{meanabove}
\alias{meanabove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Chi-square - desired values above the mean
%%  ~~function to do ... ~~
}
\description{Chi-square test comparing the frequency of observations above the reference phase mean in any two phases. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
meanabove(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{behavior variable
  \item{behavior}{
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{first phase letter (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{second phase letter (e.g., "B")
%%     ~~Describe \code{v2} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p84, p144

Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeiltlin,PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
esteem<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 
2, 2, 1, 2, 1, 0, 0, 0)
pesteem<-c("A", "A", "A", "A", "A", "A", NA, 
"B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
meanabove(esteem, pesteem, "A","B1")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
