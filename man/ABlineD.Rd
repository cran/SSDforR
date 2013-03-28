\name{ABlineD}
\alias{ABlineD}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ADD Dashed Line to Graph
%%  ~~function to do ... ~~
}
\description{enables the user to draw dashed vertical lines between phases on a graph.  Once the function is invoked, the user is prompted to accept the line or not.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABlineD()
}
%- maybe also 'usage' for other objects documented here.

\references{Go to www.ssdanalysis.com for more info
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABplot(cry,pcry,"week","amount","Crying")
# now run ABlineD()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
