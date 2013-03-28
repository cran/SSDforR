\name{sd1bandgraph}
\alias{sd1bandgraph}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{1-standard deviation band graph for one phase
%%  ~~function to do ... ~~
}
\description{Produces graph for one phase with mean and one standard deviation bands displayed.  Output in the Console displays the sd, the  mean, and values for the sd bands.  
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sd1bandgraph(behavior, phaseX, v1, ABxlab, ABylab, ABmain)
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

\references{Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin, PhD; Yeshiva Univeresity, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, 
"B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
sd1bandgraph(cry,pcry,"A","week","amount","Crying")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
