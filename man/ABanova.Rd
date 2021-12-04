\name{ABanova}
\alias{ABanova}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Analysis of variance
%%  ~~function to do ... ~~
}
\description{Computes one-way ANOVA and performs Tukey multiple comparison post-hoc test. Use ANOVA instead of  a t-test when comparing more than two phases.
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ABanova(behavior, phaseX)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{phaseX}{phase variable
%%     ~~Describe \code{phaseX} here~~
}
}
\references{Go to www.ssdanalysis.com for more information.
%% ~put references to the literature/web site here ~

Auerbach, Charles, and Zeitlin Wendy. SSD for R: An R Package for Analyzing Single-Subject Data, 2nd ed. Oxford University Press, 2022. p91, p112-116, p191}
\author{Charles Auerbach, PhD
Wurzweiler School of Social Work 
Wendy Zeitlin, PhD
Montclair State University 

%%  ~~who you are~~

%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
cry<-c(3, 4, 2, 5, 3, 4, NA, 2, 2, 3, 2, 1, 2, NA, 2, 2, 
1, 2, 1, 0, 0, 0)
pcry<-c("A", "A", "A", "A", "A", "A", NA, "B", "B", 
"B", "B", "B", "B", 
NA, "B1", "B1", "B1", "B1", "B1", "B1", "B1", "B1")
ABanova(cry,pcry)

}

