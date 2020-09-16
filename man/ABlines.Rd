\name{ABlines}
\alias{ABlines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Draw  line 
%%  ~~function to do ... ~~
}
\description{This function enables the user to draw solid vertical lines between phases on a graph.  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABlines(behavior)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
}
\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p46, p50, p51, p60, p61, p71, p75, p76, p79, p105, p108, p129

Go to www.ssdanlysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 
%%  ~~who you are~~
}

\examples{cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# now run ABplot(cry,pcry,"week","amount","Crying")
# now run ABlines(cry)
}
