\name{rm.bias}

\title{
Remove the bias of selected features by iFes.
}

\description{
The features which selected by iFes have bias, we conduct multiple cross validation to screen the features again to make sure the generality of features.
}

\usage{
rm.bias(result, data, fold, times, count)
}


\arguments{
\item{result}{The data frame returned by iFes function.}
\item{data}{Gene expression profile matrix. The columns represent different prob sets. The rows represent different samples. The values in the matrix represent gene expression levels.}
\item{fold}{The number of fold to do cross validation.}
\item{times}{The repeated times of cross validation.}
\item{count}{The number of parallel progress to do the task.}
}

\value{
A data frame containing the average acc and auc for selected features by multiple cross validation.
}


\keyword{ bias }
