\name{LRCUDA}
\alias{Logidstic Reg CUDA}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
An incremental feature selection algorithm for gene expression profile. We implement it using logistic regression which is accelerated by NVIDIA GPU technology.}
\description{
A gene feature selection algorithm acclerating by NIVIDIA CUDA technology.}

\usage{
LRCUDA(x, y, n.comb = 2, error.threshhold = 0 , fold = 10, device.id = 0, cl = NULL)
}
\arguments{
\item{x}{Gene expression profile matrix. The columns represent different prob sets. The rows represent different samples. The values in the matrix represent gene expression levels.}
\item{y}{The label vector, positive(1) or negative(0)}
\item{n.comb}{The max number of features which are selected.}
\item{fold}{The number of fold of cross validation used in the program.}
\item{device.id}{The GPU device ID. }
\item{cl}{The cluster of computers which you created. If it is NULL, the program will create clusters based on the number of devices automatically.}
}

\value{
\item{result}{The result of feature selection . Each row in result represent the one combination of selected features and its log loss.}
}

\keyword{ LRCUDA }
% use one of  RShowDoc("KEYWORDS")

