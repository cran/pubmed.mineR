R2S4 <-
function(x) {M = read.delim(x, header = TRUE, sep = "\t");  temp=new("Abstracts", Journal=as.character(M$Journal), Abstract=as.character(M$Abstract), PMID=as.numeric(M$PMID));   return(temp)}

