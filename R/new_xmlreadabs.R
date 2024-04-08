new_xmlreadabs=
function (file) 
{
  test1 <- xmlParse(file)
  test2 = getNodeSet(test1, "//PubmedArticle")
  test2a <- NULL
  test2b <- NULL
  test2c <- NULL
  for (i in 1:length(test2)) {
    saveXML(test2[[i]], "temp.txt")
    temp <- xmlParse("temp.txt")
    tempAA <- getNodeSet(temp, "//AbstractText")
    if (length(tempAA) == 0) 
      test2a <- c(test2a, "No Abstract Found")
    else {
          
      tempBB <- unlist(lapply(tempAA, function(x){c(xmlAttrs(x),xmlValue(x))}))
      tempBB <- gsub("UNASSIGNED", "", tempBB)
      tempBB <- union(tempBB,tempBB)
      tempBB <- paste(tempBB, collapse = " ")
      tempBB <- space_quasher(tempBB)
      test2a <- c(test2a, tempBB[1])
    }
    tempAA <- getNodeSet(temp, "//ISOAbbreviation")
    if (length(tempAA) == 0) 
      test2b <- c(test2b, "No Journal Found")
    else {
      tempBB <- xmlValue(tempAA)
      test2b <- c(test2b, tempBB[1])
    }
    tempAA = getNodeSet(temp, "//PMID")
    if (length(tempAA) == 0) 
      test2c <- c(test2c, "No PMID Found")
    else {
      tempBB <- xmlValue(tempAA)
      test2c <- c(test2c, tempBB[1])
    }
  }
  check = (length(test2a) == length(test2b)) & (length(test2b) == 
                                                  length(test2c))
  if (check) {
    resultabs = new("Abstracts", Journal = test2b, Abstract = test2a, 
                    PMID = as.numeric(test2c))
    return(resultabs)
  }
  else return("There is some problem in xml file. Please check")
}