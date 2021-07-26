additional_info = function(abs,pmid,keywords) 
   { 
         mydata2= NULL
         mydata1= NULL
         for(i in 1:length(pmid))
           {
                 
                   print(i)
                 pmid_1=pmid[i]
                 abst=pmids_to_abstracts(pmid_1,abs)
                 for(j in 1:length(keywords))
                   { print(j)
                         pattern = keywords[j]
                         Evidence= lapply(abst@Abstract,function(x) {
                               sentence = SentenceToken(x)
                               sentence_pattern = grep(pattern,  sentence, value=TRUE,ignore.case = TRUE)
                               sentence_pattern_combine = paste(sentence_pattern , collapse = ":")
                               return(sentence_pattern_combine)
                           })
                         if (length(Evidence) != 0)
                           {
                                 mydata1 = as.matrix(cbind(pmid_1,pattern,Evidence))
                                 mydata2 = as.matrix(rbind(mydata2,mydata1))
                                 Evidence = NULL
                             }
                     }
             }
         return(mydata2)
     } 
