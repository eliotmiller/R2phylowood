#this function takes a nexus file and a desired root edge to add, and outputs a tree block
#for pasting into an nhx file

newTreeBlock <- function(file, root.edge)
{
	#load the phylogeny in
	tree <- read.nexus(file)

	#use the zeros and ones in the matrix to create a table of replacement character
	#strings to add after each node in the output tree block
	zeros.ones <- zerosOnes(file)

	replacementTable <- matrix(ncol=1, nrow=dim(zeros.ones)[1])
	for(i in 1:dim(zeros.ones)[1])
	{
		temp.string <- paste(zeros.ones[i,], collapse=",")
		replacementTable[i,] <- paste("[&area_pp={", temp.string, "}]:", sep="")
	}
	#create a lookup table of character strings to search for to replace with values from
	#the replacement table above. note that if a match isn't found, no replacement will be
	#made. also note that currently the root edge will be replaced later, to speed up the
	#function by not having to search for a large number of strings ending with ";"
	rownames(zeros.ones) <- 1:dim(zeros.ones)[1]
	lookupOption1 <- paste("(", rownames(zeros.ones), ":", sep="")
	lookupOption2 <- paste(",", rownames(zeros.ones), ":", sep="")
	lookupOption3 <- paste(")", rownames(zeros.ones), ":", sep="")
	lookupTable <- data.frame(lookupOption1, lookupOption2, lookupOption3)

	#run a for loop replacing all the instances in block with the new values from repTable
	outputBlock <- readTreeBlock(file)
	
	for(i in 1:dim(replacementTable)[1])
	{
		outputBlock <- sub(lookupTable[i,1], paste("(", i, replacementTable[i], sep=""), outputBlock, fixed=TRUE)
		outputBlock <- sub(lookupTable[i,2], paste(",", i, replacementTable[i], sep=""), outputBlock, fixed=TRUE)
		outputBlock <- sub(lookupTable[i,3], paste(")", i, replacementTable[i], sep=""), outputBlock, fixed=TRUE)
	}
	
	#replace the root edge with the right values from repTable
	
	rootNode <- length(tree$tip.label)+1
	
	outputBlock <- sub(pattern=paste(")", rootNode, ";", sep=""), 
		replacement=paste(")", rootNode, replacementTable[rootNode], root.edge, ";", sep=""),
		outputBlock, fixed=TRUE)
	
	write(outputBlock, file="newTreeBlock.txt")
	print("New tree block printed to working directory")
}
