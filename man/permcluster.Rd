\name{permcluster}
\alias{permcluster}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Generates a Permuted Data Set
}
\description{
This function generates a permuted data set that can be used to perform a permutation test for a variance component for 2-level, 3-level or 4-level data. 
}
\usage{
permcluster(cluster, data, leveltested)
}
\arguments{
  \item{cluster}{
A vector giving the name of the variables in the data frame \code{data} to specify the clustering configuration. The order is important. For 2-level data it is a vector of dimension 1 specifying the level 1 cluster. For 3-level data, it is a vector of dimension 2. The first
    element specifies the level 1 (outer) cluster and the second one specifies the level 2 (inner) cluster. For 4-level data, it is a vector of dimension 3. The first element specifies the level 1 (outer) cluster, the second one specifies the level 2 (middle) cluster, and the last one specifies the level 3 (inner) cluster.
}  
   \item{data}{
A data frame containing the data.
}
  \item{leveltested}{
An integer giving the level to be tested. It must be 1 for 2-level data, 1 or 2 for 3-level data, and 1, 2 or 3 for 4-level data. It corresponds to the element in \code{cluster}.
}
}
\details{
This is a utility function that allows to perform a permutation test with another test statistic than the one in the function \code{mvctm}.
}
\value{
A data frame that contains the original variables plus 1, 2 or 3 new columns. For 2-level data, a single new column called \code{clusperm1} contains the permuted indices for the level 1 cluster. For 3-level data, two new columns called \code{clusperm1} and \code{clusperm2} contain the permuted indices for the level 1 and level 2 clusters, respectively. For 4-level data, three new columns called \code{clusperm1}, \code{clusperm2} and \code{clusperm3} contain the permuted indices for the level 1, level 2, and level 3 clusters, respectively.
}
\references{
Larocque, D., Nevalainen, J. and Oja, H. (2018). Multivariate Variance Components Tests for Multilevel Data. Les Cahiers du GERAD G-2018-58.
}
\author{
Denis Larocque \code{<denis.larocque@hec.ca>}
}

\examples{

data(toydata)

# generates a permuted data set with 2-level data

permcluster(cluster=c("classroom"),data=toydata,leveltested=1)

# generates a permuted data set with 4-level data to test for level 2

permcluster(cluster=c("region","school","classroom"),
data=toydata,leveltested=2)

# performing a permutation test with another
# test statistic might look like this

\dontrun{
pval=0
# compute the statistic with the original data. Call in stato.
for (i in 1:npermut)
{
pdata=permcluster(cluster=c("classroom"),data=toydata,leveltested=1)
# compute the statistic with pdata using the new column
# clusperm1 instead of classroom as the cluster index.
# call it statp.
pval=pval+(statp>stato)
}
pval/npermut
}

}


