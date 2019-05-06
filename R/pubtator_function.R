pubtator_function = function (x) 
{
    test = getURL(paste("https://www.ncbi.nlm.nih.gov/research/pubtator-api/publications/export/pubtator?pmids=", 
        x, sep = ""))
    testa = unlist(strsplit(test, "\n", fixed = T))
    table1 = NULL
    for (i in 3:length(testa)) {
        temps = unlist(strsplit(testa[i], "\t", fixed = T))
        if (length(temps) == 5) {
            temps = c(temps, "No Data")
        }
        table1 = rbind(table1, temps)
    }
    if (ncol(table1) == 6) {
        table2 = table1
        colnames(table2) = c("PMID", "Start", "End", "Term", 
            "TermType", "TermID")
        gene = NULL
        disease = NULL
        mutation = NULL
        chemical = NULL
        species = NULL
        for (i in 1:length(table2[, 5])) {
            if (table2[i, 5] == "Gene") 
                gene = c(gene, paste(table2[i, 4], table2[i, 6],sep=">"))
            else if (table2[i, 5] == "Disease") 
                disease = c(disease, paste(table2[i, 4], table2[i, 6],sep=">"))
            else if (table2[i, 5] == "Mutation") 
                mutation = c(mutation, paste(table2[i, 4], table2[i, 6],sep=">"))
            else if (table2[i, 5] == "Chemical") 
                chemical = c(chemical, paste(table2[i, 4], table2[i, 6],sep=">"))
            else if (table2[i, 5] == "Species") 
                species = c(species, paste(table2[i, 4], table2[i, 6],sep=">"))
        }
        gene = union(gene, gene)
        disease = union(disease, disease)
        mutation = union(mutation, mutation)
        chemical = union(chemical, chemical)
        species = union(species, species)
        return(list(Genes = gene, Diseases = disease, Mutations = mutation, 
            Chemicals = chemical, Species = species, PMID = x))
    }
    else return(" No Data ")
}
