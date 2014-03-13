DAVID_info <-function(x){
check1 = which(as.character(HGNCdata[,2]) == x);
check1a = as.character(HGNCdata[check1,1]);
check2 = which(as.character(HGNC2UniprotID[,1]) == check1a[1]);
check3 = as.character(HGNC2UniprotID[check2,2]);
 check4= which(as.character(GeneToEntrez$V1) == check3[1])
check5 = as.character(GeneToEntrez[check4,2])
result=DAVIDQuery(ids=check5, type="ENTREZ_GENE_ID", annot=NULL, tool="geneReport")
print("DAVIDQueryResult for the given ID with ID, Gene.Name,and species respectively")
print(paste(result$DAVIDQueryResult))
print("link to see the result in the web")
print(paste(result$firstURL, sep=""))
}
