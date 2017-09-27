\name{f.ratio}
\alias{f.ratio}
\title{
Screen features by fisher's ratio(statistic).
}
\description{
Fisher's ratio is a measure for discriminating power of some variable. We use it to detect useless features.
}
\usage{
f.ratio(x, y, rank)
}
%- maybe also 'usage' for other objects documented here.


\arguments{
\item{x}{Gene expression profile matrix. The columns represent different prob sets. The rows represent different samples. The values in the matrix represent gene expression levels.}
\item{y}{The response vector, positive(1) or negative(0).}
\item{k}{Top k features are returned which are mostly correlated with response(y).}
}

\value{
{}{A vector containing selected feature index.}
}



\keyword{ ratio }

