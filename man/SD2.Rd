\name{SD2}
\alias{SD2}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{2-standard deviation band graph
%%  ~~function to do ... ~~
}
\description{Produces graph for all phases with mean and two standard deviation bands displayed for a comparison phase across all phases.  Output in the Console displays the sd, the  mean, and values for the sd bands. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
SD2(behavior, phaseX, v1, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{letter for phase variable stats are base upon in quotation marks (e.g., "A")
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
Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014. p32-133

Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 
1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA,
"B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
SD2(cry,pcry,"A","week","amount","Crying")
}
