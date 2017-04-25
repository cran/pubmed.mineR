get_MedlinePlus = function(x){ test = x;test1 = unlist(strsplit(test," ",fixed=T)); test2 = paste(test1[1:length(test1)],collapse ="+");
check1 = getURL(paste("https://wsearch.nlm.nih.gov/ws/query?db=healthTopics&term=",test2,sep=""));
check2 = xmlTreeParse(check1,useInternalNodes = T);
check3 = unlist(lapply(getNodeSet(check2,"//term"),function(x){xmlValue(x)}));
check4 = unlist(lapply(getNodeSet(check2,"//file"),function(x){xmlValue(x)}));
check5 = unlist(lapply(getNodeSet(check2,"//server"),function(x){xmlValue(x)}));
check6 = unlist(lapply(getNodeSet(check2,"//count"),function(x){xmlValue(x)}));
if (check6 != 0) {check7 = getURL(paste("https://wsearch.nlm.nih.gov/ws/query?file=",check4,"&server=",check5,"&retstart=0&retmax=",check6,sep="")); check8 = xmlTreeParse(check7,useInternalNodes = T);
for (i in 0:as.numeric(check6)){check9 = unlist(lapply(getNodeSet(check8,paste("//list/document[@rank=",i,"]/content[@name='title']",sep="")), function(x){xmlValue(x)}));check10 = unlist(lapply(getNodeSet(check8,paste("//list/document[@rank=",i,"]/content[@name='altTitle']",sep="")), function(x){xmlValue(x)})) ;check11 = unlist(lapply(getNodeSet(check8,paste("//list/document[@rank=",i,"]/content[@name='FullSummary']",sep="")), function(x){xmlValue(x)}));write(paste(">",check9,sep = " "),file= "result_Medline_plus.html",append=T);write(paste(">",check10,sep = " "),file= "result_Medline_plus.html",append=T); write(paste(">",check11,sep = " "),file= "result_Medline_plus.html",append=T)}  } else write("No data", file="result_Medline_plus.html")}
