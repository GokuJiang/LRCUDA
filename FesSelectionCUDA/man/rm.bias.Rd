\name{rm.bias}
\alias{rm.bias}
\title{
Remove the bias of selected features by iFes.
}

\description{
The features which selected by iFes have bias, we conduct multiple cross validation to screen the features again to make sure the generality of features.
}

\usage{
rm.bias(result, x, y, fold, times, parallel = 1)
}


\arguments{
\item{result}{The data frame returned by iFes function.}
\item{x}{Gene expression profile matrix. The columns represent different prob sets. The rows represent different samples. The values in the matrix represent gene expression levels.}
\item{y}{The response vector, positive(1) or negative(0).}
\item{fold}{The number of fold to do cross validation.}
\item{times}{The repeated times of cross validation.}
\item{parallel}{The number of parallel progress to do the task.}
}

\value{
A data frame containing the average acc and auc for selected features by multiple cross validation.
}


\keyword{ bias }
