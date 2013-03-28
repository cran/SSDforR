\name{ABdescrip}
\alias{ABdescrip}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Descriptive Statistics
%%  ~~function to do ... ~~
}
\description{
This function produces descriptive statistics for all phases. Statistics produced are: mean, 10 percent trimmed mean, median, standard deviation (sd), coefficient of variation (CV), range, interquartile range, and quantiles.  Graphical output for this function is a boxplot of data in each phase.
}
\usage{
ABdescrip(behavior, PhaseX)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{PhaseX}{phase variable
%%     ~~Describe \code{PhaseX} here~~
}
}

\references{
{ Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p32, p44, p49, p98, p109, p134

Auerbach, C. & Schudrich, W. Z. (2013). SSD for R A Comprehensive Statistical Package to Analyze Single-System Data. Research on Social Work Practice, 23(3), 346-353. doi:10.1177/104973153477213

}
{Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
}
\author{Charles Auerbach,PhD & Wendy Zeitlin Schudrich,PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABdescrip(cry,pcry)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
