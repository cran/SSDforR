\name{Xmrchart}
\alias{Xmrchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SPC XMR-chart
%%  ~~function to do ... ~~
}
\description{The X-mR-chart can be use to detect changes within and between phases.  Can be used with individual data, but it is not appropriate for group data. 
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Xmrchart(behavior, phaseX, v1, bandX, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
  \item{v1}{phase bands are based upon in quotation marks (e.g., "A")
%%     ~~Describe \code{v1} here~~
}
  \item{bandX}{number of standard deviations desired (e.g., 2)
%%     ~~Describe \code{bandX} here~~
}
  \item{ABxlab}{x-axis label in quotation marks (e.g., "week")
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{y-axis label in quotation marks (e.g., "amount")
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title for chart in quotation marks (e.g., "X-mR-Chart")
%%     ~~Describe \code{ABmain} here~~
}
}

\references{

{Bloom, M., Fischer, J. & Orme, J.G. (2009). Evaluating practice: Guidelines for the accountable professional (6th ed.).  New York:  Pearson.

Orme, J. & Cox, M.E. (2001). Analyzing single-subject design data using statistical proces control charts.  Social Work Research, 25(2), 115-127.

}
{Go to www.ssdanalysis.com for more information.}
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach, PhD & Wendy Zeitlin Schudrich, PhD; Yeshiva University, Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
Xmrchart(cry, pcry, "A", 2, "week", "amount", "X-mR-Chart")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
