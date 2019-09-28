\name{IRDabove}
\alias{IRDabove}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Improvement Rate Difference (IRD) calculation and line
%%  ~~function to do ... ~~
}
\description{This effect size function will compute the IRD and display a graph in the graph window.The desired improvemment is an increase in the baseline behavior. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IRDabove(behavior, phaseX, v1, v2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{first phase letter (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{v2}{second phase letter (e.g., "B")
%%     ~~Describe \code{v2} here~~
}
}

\references{

Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p62-65, p136

{Bloom, M., Fischer, J. & Orme, J.G. (2009). Evaluating practice:  Guidelines for the accountable professional (6th ed.). New York:  Pearson.

Parker, R.I. & Hagan-Burke, S. (2007). Median-based overlap analysis for single case data:  A second study.  Behavior Modification, 31(6), 919-936.

}
{Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
}
\author{
Charles Auerbach, PhD & Wendy Zeitlin, PhD; Yeshiva University, Wurzweiler School of Social Work
}
%%  ~~who you are~~


\examples{
esteem<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pesteem<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
IRDabove(esteem,pesteem,"A","B1")
}
