\name{mvctm}
\alias{mvctm}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multivariate Variance Components Tests for Multilevel Data
}
\description{
This function performs a permutation test for a variance component for 2-level, 3-level or 4-level data. The response can be univariate or multivariate.
}
\usage{
mvctm(fixed, cluster, data, leveltested, method = "ls", npermut = 1000, 
weight = "observation", affequiv = TRUE)
}
\arguments{
  \item{fixed}{
An object of class ``formula" describing the fixed effects part of the model using the
variables in the data frame \code{data}.
}
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
  \item{method}{
The scores to be used. The four choices \code{"ls"},  \code{"mixed"}, \code{"rank"} and \code{"sign"} are available. The default is \code{"ls"}. The choice \code{"mixed"} is only available for a univariate response.
}
  \item{npermut}{
The number of random permutation used to perform the test. The default is 1000.
}
  \item{weight}{
The weight function to be used. The three choices \code{"pair"}, \code{"observation"} and \code{"cluster"} are available. The default is \code{"observation"}.
}
  \item{affequiv}{
Whether or not we want to use the affine-equivariant version of the tests. This is only relevant for a multivariate response and \code{method}=\code{"rank"} or \code{"sign"}. The default is \code{TRUE}.
}
}
\details{
With \code{method="ls"}, the fixed effects are estimated by ordinary least-squares. Then the test is performed on the residuals from this fit. With \code{method="mixed"}, the fixed effects are estimated with a linear mixed model. Then the test is performed on the marginal (population) residuals from this fit. With \code{method="rank"}, a rank-based method is used to estimate the fixed effects. Then the test is performed on the ranks of the residuals from this fit. Finally, with \code{method="sign"}, a sign-based method is used to estimate the fixed effects. Then the test is performed on the signs of the residuals from this fit. For multivariate data, spatial ranks and signs are used.

With a univariate response, \code{method="sign"} is not recommended because
the test might be liberal. 

With, \code{weight="pair"},  observations in larger clusters at the level \code{leveltested} will have more weights. With, \code{weight="cluster"}, the same weight is given to each cluster at the level \code{leveltested}. As a compromise between these two, the default \code{weight="observation"}  gives an equal weight to each individual observation, with respect to the clusters at level \code{leveltested}.

}
\value{
A list with the following two elements:
  \item{pvalue}{The p-value of the test.}
  \item{statistic}{The value of the test statistic computed on the original data.}
}
\references{
Larocque, D., Nevalainen, J. and Oja, H. (2017). Multivariate Variance Components Tests for Multilevel Data: The mvctm package. Les Cahiers du GERAD G-2017-16.
}
\author{
Denis Larocque \code{<denis.larocque@hec.ca>}
}

\examples{

data(toydata)

# Bivariate 2-level model.
# Classroom as the clusters. 
# Only an intercept is in the fixed part of the model.
# Test based on 200 permutations
mvctm(fixed=cbind(y1,y2)~1,cluster=c("classroom"),
data=toydata,leveltested=1,npermut=200)

# Same as above but The two covariates are in the fixed part of the model.
# Test based on 1000 permutations (default).
\dontrun{
mvctm(fixed=cbind(y1,y2)~x1+x2,cluster=c("classroom"),
data=toydata,leveltested=1)
}

# Same as above but the rank scores are used.
\dontrun{
mvctm(fixed=cbind(y1,y2)~x1+x2,cluster=c("classroom"),
data=toydata,leveltested=1, method="rank")
}

# Univariate 4-level model. 
# Classrooms, nested within schools, nested within regions.
# The variance component at the region level is tested. 
# The fixed effects are estimated with a linear mixed model.
\dontrun{
mvctm(fixed=y1~x1+x2,cluster=c("region","school","classroom"),
data=toydata,leveltested=1,method="mixed")
}

# Same as above but the variance component at the school level is tested.
\dontrun{
mvctm(fixed=y1~x1+x2,cluster=c("region","school","classroom"),
data=toydata,leveltested=2,method="mixed")
}

# Same as above but the variance component at the classroom level is tested.
\dontrun{
mvctm(fixed=y1~x1+x2,cluster=c("region","school","classroom"),
data=toydata,leveltested=3,method="mixed")
}

# Univariate 3-level model.
# The variance component at the classroom level is tested.
# The fixed effects are removed with an M-estimator with the rlm function 
#      in the MASS package. 
# Then the residuals from this fit are used to perform the test. 
# The ~0 in the formula tells mvctm to use  mresid directly to perform 
#      the test without any centering or transformation.  
\dontrun{
library("MASS")
toydata[,"mresid"]=rlm(y1~x1+x2,data=toydata)$residuals
mvctm(fixed=mresid~0,cluster=c("school","classroom"),
data=toydata,leveltested=2)
}

}


