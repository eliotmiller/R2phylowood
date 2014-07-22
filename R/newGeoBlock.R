#this function will take a table of species' range centroids. the table should have a
#columns named latitude, longitude, and species. it will then use phytools' fastAnc() to
#calculate the ancestral lat and long of the nodes, and concatenate all of this
#information into a new geo block to paste into the .nhx file

newGeoBlock <- function(coord.table, file)
{
	require(phytools)
	
	#read in the files
	tree <- read.nexus(file)
	coord.table <- read.csv(coord.table)
	#create vectors of latitude and longitude, then sort them in the same order as the
	#phylogeny
	tip.lat <- coord.table[,grep("latitude", names(coord.table), ignore.case=TRUE)]
	names(tip.lat) <- coord.table[,grep("species", names(coord.table), ignore.case=TRUE)]
	tip.lat <- tip.lat[tree$tip.label]
	tip.lon <- coord.table[,grep("longitude", names(coord.table), ignore.case=TRUE)]
	names(tip.lon) <- coord.table[,grep("species", names(coord.table), ignore.case=TRUE)]
	tip.lon <- tip.lon[tree$tip.label]
	
	#now calculate the ancestral states of both latitude and longitude
	node.lat <- fastAnc(tree, tip.lat)
	node.lon <- fastAnc(tree, tip.lon)
	
	#bind these onto the tip lat and lon
	lats <- c(tip.lat, node.lat)
	lons <- c(tip.lon, node.lon)
	
	#create a column of row names like in the .nhx file format
	rows <- 0:(length(lats)-1)
	
	#create output geo table. add a comma at end to be in keeping with .nhx
	outputBlock <- data.frame(rows, lats, lons=paste(lons, ",", sep=""))
	
	write.table(outputBlock, "newGeoBlock.txt", quote=FALSE, row.names=FALSE, col.names=FALSE)	
	print("New geo block printed to working directory")
}
