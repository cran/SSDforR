\name{RMEffectsize}
\alias{RMEffectsize}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Effect size
%%  ~~function to do ... ~~
}
\description{The effect size function automatically displays the percent change and calculated values for both the ES and d-index for any two phases.  Information for interpretting calculated values appears in the Console.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
RMEffectsize(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{first phase letter between quotation marks (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{second phase letter between quotation marks (e.g., "B")
%%     ~~Describe \code{v2} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p56-57, p103, p135

{Cohen, J.(1988).Statistical Power analysis for the behavioral sciences (2nd ed). Hillsdale, NJ: Lawrence Erlbaum Associates, Inc.

Glass, G. V., McGaw, B., & Smith, M. L. (1981) Meta-analysis in social research. Thousand Oaks, CA: SAGE Publications, Inc.

Bloom, M., Fischer, J. & Orme, J.G. (2009). Evaluating practice:  Guidelines for the accountable professional (6th ed.). New York:  Pearson.

}
{Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
}
\author{Charles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
RMEffectsize(cry,pcry,"A","B")
}
