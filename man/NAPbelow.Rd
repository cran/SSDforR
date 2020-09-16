\name{NAPbelow}
\alias{NAPbelow}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Non-Overlap of All Pairs (NAP) calculation 
%%  ~~function to do ... ~~
}
\description{This effect size function will compute the NAP. The desired improvemment is an decreas in the baseline behavior. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
NAPbelow(behavior, phaseX, v1, v2)
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

Parker, R. I., & Vannest, K. (2009). An improved effect size for single-case research: Nonoverlap of all pairs. Behavior Therapy, 40(4), 357-367.

}
{Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
}
\author{
Charles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 
}
%%  ~~who you are~~


\examples{
esteem<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pesteem<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
NAPbelow(esteem,pesteem,"A","B1")
}
