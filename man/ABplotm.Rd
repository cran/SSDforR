\name{ABplotm}
\alias{ABplotm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Multiple line plot
%%  ~~function to do ... ~~
}
\description{This function should be used to create multiple line charts.  This function must be used after the environment is set up using the plotnum() function.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABplotm(behavior, phaseX, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
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


Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p108, p129

Go to www.analysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin, PhD; Yeshiva University; Wurzweiler School of Social Work
%%  ~~who you are~~
}



\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
yell<-c(3, 4, 2, 5, 5, 4, NA, 1, 2, 2, 2, 0, 0)
pyell<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B")
plotnum(2, 1)
ABplotm(cry,pcry,"week","amount","Crying")
ABplotm(yell,pyell,"week","amount","Yelling")
}
