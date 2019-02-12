\name{ABttest}
\alias{ABttest}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{T-test comparing phases
%%  ~~function to do ... ~~
}
\description{Computes t-test comparing any twp phases selected by user.  Bar graph displaying means for each phase is displayed in the graph window.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABttest(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter of first phase (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{letter of second phase (e.g., "B")
%%     ~~Describe \code{v2} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p87, p91, p146-147

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
# now run ABttest(cry,pcry,"A","B")
}
