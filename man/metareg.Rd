\name{metareg}
\alias{metareg}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Meta Regression
%%  ~~function to do ... ~~
}
\description{Meta regression for saved effect sizes in SSDforR. Use the Getcsv() function to open the saved effect size file.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
metareg(es, v)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{es}{effect size variable
%%     ~~Describe \code{behavior} here~~
}
  \item{v}{variance of effect size
%%     ~~Describe \code{phaseX} here~~
}
}
\references{Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~

Del Re, A. C. (2015). A practical tutorial on conducting meta-analysis in R. The Quantitative Methods for Psychology, 11(1), 37-50.

Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data. Oxford University Press, 2014.}
\author{Charles Auerbach, PhD &
Wendy Zeitlin, PhD; Yeshiva University,
Wurzweiler School of Social Work
%%  ~~who you are~~

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
ES<-c(.3, .4, .2, .5, .3, .4)
V<-c(.01, .03, .04, .02, .03, .02) 

metareg(ES,V)

}
