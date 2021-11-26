co_occurrence_advance = function(abstract,term1,term2,n)
{
  term = data.frame()
  final_table_1 = data.frame()
  final_table = data.frame()
  i = NULL
  j = NULL
  
  for(i in term1){
  for(j in term2){
    term = paste(i,j,sep = "-")
    
abstractsS4 <- searchabsL(abstract, restrict = c(i, j))

abstracts <- abstractsS4@Abstract 
PMIDs <- abstractsS4@PMID

numberOfAbstracts <- length(abstracts)
iterator <- 1
result = NULL
tab = data.frame()
fin_tab = data.frame()
PMID = NULL
 


while (iterator <= numberOfAbstracts) {
  
  tokenizedAbstract <- SentenceToken(abstracts[iterator])
  
    occurrencesOfFirst <- grep(i, tokenizedAbstract)
      
      occurrencesOfSecond <- grep(j, tokenizedAbstract)
      differenceAllowed <- 0
      unusedOccurrencesOfFirst <- c()
      unusedOccurrencesOfSecond <- c()
      while (differenceAllowed <= n)
        {
          firstIterator <- 1
          secondIterator <- 1
    
          lengthOfFirst <- length(occurrencesOfFirst)
          lengthOfSecond <- length(occurrencesOfSecond)
    
          while (firstIterator <= lengthOfFirst && secondIterator <= lengthOfSecond) 
            {
      
             if (abs(occurrencesOfFirst[firstIterator] - occurrencesOfSecond[secondIterator]) == differenceAllowed) {
             minimumOfTwo <- min(c(occurrencesOfFirst[firstIterator], occurrencesOfSecond[secondIterator]))
             maximumOfTwo <- max(c(occurrencesOfFirst[firstIterator], occurrencesOfSecond[secondIterator]))
        
             #result = paste(tokenizedAbstract[minimumOfTwo:maximumOfTwo],collapse = "::")
             #result = tokenizedAbstract[minimumOfTwo:maximumOfTwo]
             if (length(tokenizedAbstract[minimumOfTwo:maximumOfTwo]) > 1) {
               pasted <- paste(tokenizedAbstract[minimumOfTwo:maximumOfTwo], collapse = ";")
               result <- c(result ,pasted)}
             else result  <- c(result ,tokenizedAbstract[minimumOfTwo:maximumOfTwo])
             firstIterator <- firstIterator+1
             secondIterator <- secondIterator+1
             } 
            else if (occurrencesOfSecond[secondIterator] < occurrencesOfFirst[firstIterator]) {
            unusedOccurrencesOfSecond <- c(unusedOccurrencesOfSecond, occurrencesOfSecond[secondIterator])
            secondIterator <- secondIterator + 1
             } 
            else {
            unusedOccurrencesOfFirst <- c(unusedOccurrencesOfFirst, occurrencesOfFirst[firstIterator])
            firstIterator <- firstIterator + 1
            }
            
           }
          differenceAllowed <- differenceAllowed+1
          occurrencesOfFirst <- unusedOccurrencesOfFirst
          occurrencesOfSecond <- unusedOccurrencesOfSecond
          if(length(result)>=1){
            PMID = PMIDs[iterator]
            tab = cbind(PMID,result,term)
            fin_tab = rbind(fin_tab,tab)}
          result = NULL
          PMID = NULL
        }
  
      
       iterator <- iterator + 1
  }
   
  final_table_1 = rbind(final_table_1,fin_tab)
  fin_tab = NULL
  }
  final_table = rbind(final_table,final_table_1)
  final_table_1 = NULL
  }
return(final_table)
}