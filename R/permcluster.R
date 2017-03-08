permcluster <-

function(cluster,data,leveltested)
{

if(!is.data.frame(data)){ stop("Argument data must be a data frame") }
if(!is.vector(cluster)){ stop("Argument cluster must be a vector") }

N=NROW(data) 			# Total number of observations
nlevel=length(cluster)+1	# Number of levels

cluster=as.matrix(data[,cluster])
if(sum(is.na(cluster))>0){ stop("The cluster memberships can not contain missing values") }
if(nlevel!=2 & nlevel!=3 & nlevel!=4){ stop("Works for 2, 3 or 4-level data only") }

if(is.numeric(leveltested)==FALSE){ stop("You must specify the level to be tested") }

if(length(leveltested)>1){stop("Only one value of leveltested can be given")}

if(nlevel==2){if(leveltested!=1){stop("The level to be tested must be 1 for 2-level data")}}
else if(nlevel==3){if(leveltested!=1 & leveltested!=2){stop("The level to be tested must be 1 or 2 for 3-level data")}}
else if(nlevel==4){if(leveltested!=1 & leveltested!=2 & leveltested!=3){stop("The level to be tested must be 1, 2 or 3 for 4-level data")}}	

# Put the cluster matrix into a usable form for the other functions

prepc=preparecluster(cluster)

if(nlevel==2)
	{
	cluster=prepc[[1]]
	m1=prepc[[2]]
	n1=prepc[[3]]
	clusperm1=rep(0,N)
	uclusterperm=unlist(relist(sample(1:N),cluster))
	t=1
	for(i in 1:m1)
		{
		for(j in 1:n1[i])
			{
			clusperm1[uclusterperm[t]]=i
			t=t+1
			}
		}
	out=cbind(data,clusperm1)
	}

else if(nlevel==3)
	{
	cluster=prepc[[1]]
	m1=prepc[[2]]
	n1=prepc[[3]]
	m2=prepc[[4]]
	n2=prepc[[5]]
	clusperm1=rep(0,N)
	clusperm2=rep(0,N)
	if(leveltested==2)
		{
		clusterperm=lapply(cluster,permute3levelt2)		
		uclusterperm=unlist(clusterperm)
		}
	else if(leveltested==1)
		{
		cperm=permute3levelt1(cluster,m1,m2)
		clusterperm=cperm[[1]]	
		uclusterperm=unlist(cperm[[1]])		
		}
		t=1
		for(i in 1:m1)
			{
			for(j in 1:length(clusterperm[[i]]))
				{
				for(k in 1:length(clusterperm[[i]][[j]]))
					{
				clusperm1[uclusterperm[t]]=i
				clusperm2[uclusterperm[t]]=j
				t=t+1
					}
				}
			}
	out=cbind(data,clusperm1,clusperm2)	
	}

else if(nlevel==4)
	{
	cluster=prepc[[1]]
	m1=prepc[[2]]
	n1=prepc[[3]]
	m2=prepc[[4]]
	n2=prepc[[5]]
	m3=prepc[[6]]
	n3=prepc[[7]]	
	clusperm1=rep(0,N)
	clusperm2=rep(0,N)
	clusperm3=rep(0,N)
	if(leveltested==3)
		{
		clusterperm=lapply(cluster,permute4levelt3)
		uclusterperm=unlist(clusterperm)
		}
	if(leveltested==2)
		{
		clusterp=permute4levelt2(cluster,m1,m2,m3) 
		clusterperm=clusterp[[1]]		
		uclusterperm=unlist(clusterperm)
		}
	if(leveltested==1)
		{
		clusterp=permute4levelt1(cluster,m1,m2,m3) 
		clusterperm=clusterp[[1]]
		uclusterperm=unlist(clusterperm)
		}
		t=1
		for(i in 1:m1)
			{
			for(j in 1:length(clusterperm[[i]]))
				{
				for(k in 1:length(clusterperm[[i]][[j]]))
					{
					for(l in 1:length(clusterperm[[i]][[j]][[k]]))
						{
					clusperm1[uclusterperm[t]]=i
					clusperm2[uclusterperm[t]]=j
					clusperm3[uclusterperm[t]]=k
					t=t+1
						}
					}
				}
			}
	out=cbind(data,clusperm1,clusperm2,clusperm3)	
	}

out

}