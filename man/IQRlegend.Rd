\name{IQRlegend}
\alias{IQRlegend}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{IQR legend
%%  ~~function to do ... ~~
}
\description{This function creates a legend on an IQR band graph.  
NOTE:  Once this legend is in place, the graph can no longer be altered.

%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IQRlegend()
}
%- maybe also 'usage' for other objects documented here.

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p133

Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin,PhD; Yeshiva Univeresity,  Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
IQRbandgraph(cry,pcry,"A","week","amount","Crying")
IQRlegend()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
