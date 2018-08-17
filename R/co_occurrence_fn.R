co_occurrence_fn = function (terms1, abs, filename, terms2) 
{
    result_genes = NULL
    for (i in 1:length(terms1)) {
        if (terms1[i] != "NONE") {
            for (j in 1:1) result_genes = Give_Sentences(terms1[i], abs)
            if (length(result_genes) != 0) {
                print(c(i, j))
                write(paste(">>", terms1[i], sep = " "), file = paste(filename, 
                  "co_occurrence.txt", sep = ""), append = T)
                for (k in 1:length(result_genes)) {
                  for (l in 1:length(result_genes[[k]])) {
                    temp = result_genes[[k]][l]
                    for (s in 1:length(terms2)) {
                      temp1 = regexpr(terms2[s], temp)
                      if (temp1 != -1) 
                        write(c(attr(result_genes, "PMID")[k], 
                          temp), file = paste(filename, "co_occurrence.txt", 
                          sep = ""), append = T)
                    }
                  }
                }
            }
        }
    }
}