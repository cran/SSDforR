\name{IQRbandgraph}
\alias{IQRbandgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Interquartile band graph for one phase
%%  ~~function to do ... ~~
}
\description{Draws an interquartile band graph for any phase.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
IQRbandgraph(behavior, phaseX, v1, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter for phase for which bands will be drawn (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{ABxlab}{label for x-axis in quotation marks (e.g., "weeks")
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{lable for y-axis in quotation marks (e.g., "amount")
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title for graph in quotation marks (e.g., "Crying")
%%     ~~Describe \code{ABmain} here~~
}
}

\references{
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p133

Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin, PhD; Yeshiva University,  Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B",
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
IQRbandgraph(cry,pcry,"A","week","amount","Crying")
}
