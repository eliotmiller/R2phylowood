#function will read a nexus file, assign node labels to the file, save out a temporary
#version of this new file, then scan it to get the Newick style of the file with nodes
#returns the tree block needed for later use. make sure you don't have any files named
#"delete.nex" you need in your working directory

readTreeBlock <- function(file)
{
	require(ape)
	temp.nex <- read.nexus(file)
	nodeLabels <- (length(temp.nex$tip.label)+1):(length(temp.nex$tip.label)+1+temp.nex$Nnode)
	temp.nex$node.label = nodeLabels
	write.nexus(temp.nex, file="delete.nex")
	temp.nex <- scan(file = "delete.nex", what = "", sep = "\n", quiet = TRUE)
	treeBlock <- temp.nex[length(temp.nex)-1]
	treeBlock <- sub("\t", "", treeBlock)
	return(treeBlock)
}
