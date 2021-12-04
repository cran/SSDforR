\name{RSlines}
\alias{RSlines}

\title{Draws line. For use with Rmarkdown

}
\description{This function enables the user to draw solid vertical lines between phases on a graph. For use nointeractive use.

}
\usage{
RSlines(behavior,x)
}

\arguments{
  \item{behavior}{behavior variable}
  \item{x}{X coordinate }


}
\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p46, p50, p51, p60, p61, p71, p75, p76, p79, p105, p108, p129

Go to www.ssdanlysis.com for more information.

}
\author{Charles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 

}

\examples{cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# now run ABplot(cry,pcry,"week","amount","Crying")
# now run RSlines(cry,13.5)
}
