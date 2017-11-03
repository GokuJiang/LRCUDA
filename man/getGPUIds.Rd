\name{getGPUIds}
\alias{getGPUIds}
\title{Get GPU device IDs from localhost or a cluster.}

\description{
Query GPU device number and ID from the localhost or a cluster. 
}
\usage{
getGPUIds(cl = NULL)
}



\arguments{
\item{cl}{The cluster of computers which you created. If it is NULL, the program will execute in localhost.}
}

\value{
\item{Ids}{A vector contains devices' IDs.}
}


\examples{
gpu.ids <- getGPUIds()
print(gpu.ids)

}

\keyword{ gpu }

