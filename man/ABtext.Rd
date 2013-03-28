\name{ABtext}
\alias{ABtext}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add text to graph
%%  ~~function to do ... ~~
}
\description{Add text to graphs. Text must appear between quotation marks.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABtext(textx)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{textx}{text string must be entered between quotation marks (e.g., "baseline")
%%     ~~Describe \code{textx} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p30, p46, p47, p48, p50, p51, p60, p61, p71, p73, p75

Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin,PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABplot(cry,pcry,"week","amount","Crying")
# now run ABtext("A")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
