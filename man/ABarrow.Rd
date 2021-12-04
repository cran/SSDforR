\name{ABarrow}
\alias{ABarrow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw arrow on graph
%%  ~~function to do ... ~~
}
\description{This function enables users to draw an arrow on a graph. For example, an arrow can be drawn from a text label of a critical event to a point on the graph. 
}
\usage{
ABarrow()
}


\references{Go to www.ssdanalysis.com for more information
%% ~put references to the literature/web site here ~

Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data 2nd ed. 
Oxford University Press, 2022. p180, p186
}
\author{Charles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 
%%  ~~who you are~~
}

\examples{cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 
1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", 
"B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# now run ABplot(cry,pcry,"week","amount","Crying")
# now run ABarrow()

}


