testvc3levelt2 <-
function(y,cluster,npermut,N,p,smat,m1,n1,m2,n2,weight){

# test the level 2 (innermost) for 3-level data. 

# y = Matrix Nxp (N obs, p=dimension) of (maybe standardized; signs or ranks) data 
# cluster = List of indices (each element gives the indices of the obs in a level 2 cluster nested within a level 1 cluster). From the "preparecluster" function.
# npermut = Number of permutations for the test
# N = Total number of observations
# p = Dimension
# smat = Transformation matrix to achieve affine-equivariance (from the "ystand" function)
# m1 = Number of level 1 clusters
# n1 = Number of observations in each level 1 cluster (vector m1X1)
# m2 = Number of level 2 clusters within each level 1 cluster (vector m1X1)
# n2 = Number of observations in each level 2 cluster within each level 1 cluster (list of size m1)
# weight = weights used to compute the dependence matrix
#		"observation" gives equal weight to each observation
#		"pair" gives equal weight to each pair of observations
#		"cluster" gives equal weight to each cluster


pvalmax=0

stato=statvc3levelt2(y,cluster,m1,n1,m2,n2,p,weight)
statoriginal=(smat %*% stato[[1]] %*% t(smat))/stato[[2]]

eig=eigen(statoriginal)
maxo=max(eig$values)

if(maxo<=0)
	{
	pvalmax=99
	return(c(pvalmax,maxo))
	}

for(i in 1:npermut)
	{
	clusterperm=lapply(cluster,permute3levelt2)
	statp=statvc3levelt2(y,clusterperm,m1,n1,m2,n2,p,weight)
	statperm=(smat %*% statp[[1]] %*% t(smat))/statp[[2]]
	maxperm=max(eigen(statperm)$values)
	pvalmax=pvalmax+(maxperm>maxo)
	}

c(pvalmax/npermut,maxo)

}
