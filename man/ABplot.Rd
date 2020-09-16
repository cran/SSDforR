\name{ABplot}
\alias{ABplot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Simple line graph
%%  ~~function to do ... ~~
}
\description{This function builds a simple line graph for a given behavior across all phases.  A space separates each phase.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABplot(behavior, phaseX, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{ behavior variable 
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{ABxlab}{label for x-axis between quotation marks
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{label for y-axis between quotation marks
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title for graph between quotation marks
%%     ~~Describe \code{ABmain} here~~
}
}

\references{
{Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p46, p48,p50, p108,p128

Auerbach, C. & Schudrich, W. Z. (2013). SSD for R A Comprehensive Statistical Package to Analyze Single-System Data. Research on Social Work Practice, 23(3), 346-353. doi:10.1177/104973153477213

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

\examples{cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
# ABplot(cry,pcry,"week","amount","Crying")


}
