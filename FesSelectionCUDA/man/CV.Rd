\name{cv}
\alias{CV}

\title{
Cross Vaildiation
}
\description{
Cross Vaildiation
}

\usage{
cv(y,fload)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{y}{ The label vector, positive(1) or negative(0)}
  \item{fload}{The number of fold of cross validation used in the program.}
}

\examples{
y <- c(1,0,0,0,1,1,1,0,0,0)
cv(y,4) 
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ cv }
