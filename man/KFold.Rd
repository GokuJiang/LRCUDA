\name{KFold}
\alias{KFold}
\title{K-Fold Cross Vialidation}

\description{
A cross vialidation algorithm called k-fold cross vailidation
}
\usage{
KFold(features, X, Y, fold, n)
}


\arguments{
\item{features}{
There are features which will be  selected.
}
\item{X}{Gene expression profile matrix. The columns represent different prob sets. The rows represent different samples. The values in the matrix represent gene expression levels.}
\item{Y}{The label vector, positive(1) or negative(0)}
\item{fold}{The number of fold of cross validation used in the program.}
\item{n}{Error}
}


\keyword{ kfold }% use one of  RShowDoc("KEYWORDS")
