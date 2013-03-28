\name{ABanova}
\alias{ABanova}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Analysis of variance
%%  ~~function to do ... ~~
}
\description{computes one-way ANOVA and performs Tukey multiple comparison test. This test should only be used if there is not a trend in any phase AND there is no problem with autocorrelation in any phase.  Use ANOVA instead of  a t-test when comparing more than two phases
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABanova(behavior, phaseX)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{Phase Variable
%%     ~~Describe \code{phaseX} here~~
}
}

\author{Charles Auerbach,PHD 
Wendy Zeitlin Schudrich,PHD
Wurzweiler School of Social Work
%%  ~~who you are~~

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", "B", "B", "B", "B", NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABanova(cry,pcry)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
