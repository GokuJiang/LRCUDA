\name{CV}
\alias{VC}

\title{Cross Vaildiation}

\description{
Cross Vaildiation
}

\usage{
cv(y,fold)
}

\arguments{
  \item{y}{ The label vector, positive(1) or negative(0)}
  \item{fold}{The number of fold of cross validation used in the program.}
}

\examples{
y <- c(1,0,0,0,1,1,1,0,0,0)
cv(y,4) 
}


\keyword{ cv }
