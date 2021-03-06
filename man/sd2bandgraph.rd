\name{sd2bandgraph}
\alias{sd2bandgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{2-standard deviation band graph for one phase
%%  ~~function to do ... ~~
}
\description{Produces graph for one phase with mean and two standard deviation bands displayed.  Output in the Console displays the sd, the  mean, and values for the sd bands. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sd2bandgraph(behavior, phaseX, v1, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{phase letter (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{ABxlab}{x-axis label in quotation marks (e.g., "week")
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{y-axis label in quotation marks (e.g., "amount")
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title in quotation marks (e.g., "Crying")
%%     ~~Describe \code{ABmain} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p34, p132

Go www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 
2, 2, 3, 2, 1, 2, NA,
2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, 
"B", "B", "B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
sd2bandgraph(cry,pcry,"A","week","amount","Crying")
}
