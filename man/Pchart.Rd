\name{Pchart}
\alias{Pchart}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{SPC P-chart
%%  ~~function to do ... ~~
}
\description{When the target behavior has a binary outcome (i.e., completing or not completing a task), comparing the proportion of tasks completed over time or between phases can be measured.  Attendance (vs. non-attendance) in a group, child welfare workers completing records on time (vs. not completing on time), workers completing home visits (vs. not completing home visits), if a patient is compliant with medication (vs. non-compliant), or if a patient with hearing loss wears his or her hearing aid (vs. not hearing the hearing aid) are all possible examples of task completion that can be assessed using p-charts.   
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
Pchart(behavior, groupX, bandX, ABxlab, ABylab, ABmain)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{behavior}{behavior variable
%%     ~~Describe \code{behavior} here~~
}
  \item{groupX}{grouping variable
%%     ~~Describe \code{groupX} here~~
}
  \item{bandX}{number of standard deviations 1 2 or 3
%%     ~~Describe \code{bandX} here~~
}
  \item{ABxlab}{x label
%%     ~~Describe \code{ABxlab} here~~
}
  \item{ABylab}{y label
%%     ~~Describe \code{ABylab} here~~
}
  \item{ABmain}{main title
%%     ~~Describe \code{ABmain} here~~
}
}

\references{go to www.ssdanalysis.com for more info
%% ~put references to the literature/web site here ~
}
\author{Charles Auerbach,PHD & Wendy Zeitlin Schudrich,PHD Wurzweiler School of Social Work
%%  ~~who you are~~
}

\examples{
attend<-c(0,0,0,1,0,0,1,0,0,1,0,0,1,0,1,0,0,0,0,0,1,1,0,0,1,NA,
0,1,1,0,1,1,0,1,1,1,0,1,0,0,1,1,1,1,0,0,1,1,0,1,0,1,1,1,1,1,1,1,
1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)

day<-c(1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4,5,5,5,5,5,NA,6,6,6,6,6,7,7,7,7,7,
8,8,8,8,8,9,9,9,9,9,10,10,10,10,10,11,11,11,11,11,12,12,12,12,12,13,
13,13,13,13,14,14,14,14,14,15,15,15,15,15)
Pchart(attend, day, 2, "week", "amount", "Group attendance")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
