\name{RMarrow}
\alias{RMarrow}

\title{Draw arrow on graph. For use with Rmarkdown.

}
\description{This function enables users to draw an arrow on a graph. For example, an arrow can be drawn from a text label of a critical event to a point on the graph. For Use with Rmarkdown.
}
\usage{
RMarrow(X1,Y1,X2,Y2)
}
\arguments{
 
  \item{X1}{X1 coordinate}
  \item{Y1}{Y1 coordinate}
  \item{X2}{X2 coordinate}
  \item{Y2}{Y2 coordinate}
  
  

}

\references{Go to www.ssdanalysis.com for more information

Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. 
Oxford University Press, 2014. p46-p50
}
\author{Charles Auerbach, PhD & Wendy Zeitlin, PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 
1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", 
"B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# now run ABplot(cry,pcry,"week","amount","Crying")
# now run RMarrow(9,10,11,10)

}


