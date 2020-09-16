\name{Cchart}
\alias{Cchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SPC C-chart
%%  ~~function to do ... ~~
}
\description{This function builds a C-chart and is used with individual (i.e., ungrouped) data.  A space separates each phase.  For use when the outcome variable is a count (i.e., ratio-level) variable.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Cchart(behavior, phaseX, v1, bandX, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{phase letter analysis is based upon in quotation marks (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{bandX}{number of standard deviations desired (e.g., 2)
%%     ~~Describe \code{bandX} here~~
}
  \item{ABxlab}{label for x-axis between quotation marks (e.g., "weeks")
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{label for y-axis between quotation marks (e.g., "amount")
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title between quotation marks (e.g., Crying)
%%     ~~Describe \code{ABmain} here~~
}
}


\references{

Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p77, p140

{Bloom, M., Fischer, J. & Orme, J.G. (2009). Evaluating practice: Guidelines for the accountable professional (6th ed.).  New York:  Pearson.

Orme, J. & Cox, M.E. (2001). Analyzing single-subject design data using statistical proces control charts.  Social Work Research, 25(2), 115-127.

}
{Go to www.ssdanalysis.com for more information.}
%% ~put references to the literature/web site here ~
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
Cchart(cry,pcry,"A",2,"week","amount","Crying")
}
