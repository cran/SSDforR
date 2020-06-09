\name{RMstat}
\alias{RMstat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Add statistic line(s) for RMarkdown
%%  ~~function to do ... ~~
}
\description{Add a mean and/or median line to an ABplot for RMarkdown.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RMstat(behavior, phaseX, v, statX,l)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v}{phase letter in quotation marks (e.g., "A")
%%     ~~Describe \code{v} here~~
}
  \item{statX}{statistic in quotation marks (i.e. "mean", "median")
%%     ~~Describe \code{statX} here~~
}
\item{l}{x ordinate to start line (e.g., 1)
}
}
\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p48, p50, p51, p138

{Auerbach, C. & Schudrich, W. Z. (2013). SSD for R A Comprehensive Statistical Package to Analyze Single-System Data. Research on Social Work Practice, 23(3), 346-353. doi:10.1177/104973153477213

}
{Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
}
\author{Charles Auerbach, PhD & Wendy Zeitlin, PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA,
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# now run this ABplot(cry,pcry,"week","amount","Crying")
# run this statement RMstat(cry, pcry, "A", "median",1)
}
