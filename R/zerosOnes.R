#this function will take the nexus file and generate the matrix needed to create the
#0s and 1s that are used to link to the geo block and will be inserted into the tree block

zerosOnes <- function(file)
{
	temp.nex <- read.nexus(file)
	output <- matrix(nrow=length(temp.nex$tip.label) + temp.nex$Nnode, 
		ncol=length(temp.nex$tip.label) + temp.nex$Nnode, 0)
	diag(output) <- 1
	return(output)
}
