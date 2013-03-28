\name{ABarrow}
\alias{ABarrow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw Arrow on Graph
%%  ~~function to do ... ~~
}
\description{This function enables users to draw an arrow on a graph. For example, an arrow can be drawn from a text label of a critical event to a point on the graph. Once the function is invoked, the user is prompted to accept the arrow or not.
}
\usage{
ABarrow()
}


\references{go to www.ssdanalysis for more information
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin Schudrich,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABplot(cry,pcry,"week","amount","Crying")
# now run ABarrow()

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
