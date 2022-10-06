\name{ABWilcox}
\alias{ABWilcox}

\title{Wilcoxon rank-sum test between two phases

}
\description{Performs a two-sample Wilcoxon rank-sum nonparametric test between any two phases.


}
\usage{
ABWilcox(behavior, phaseX, v1, v2)
}

\arguments{
  \item{behavior}{behavior variable

}
  \item{phaseX}{phase variable

}
  \item{v1}{first phase letter (e.g., "A")

}
  \item{v2}{second phase letter (e.g., "B")

}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p147

Go to www.ssdanalysis.com for more information.

}
\author{Charles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 

}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABWilcox(cry,pcry,"A","B")
}
