pubtator_function_JSON <- function(x){
  check <- isValidJSON(paste("https://www.ncbi.nlm.nih.gov/research/pubtator-api/publications/export/biocjson?pmids=",x,sep = ""))
  if (check != FALSE){
    test <- fromJSON(paste("https://www.ncbi.nlm.nih.gov/research/pubtator-api/publications/export/biocjson?pmids=",x,sep = ""))
    gene = NULL
    disease = NULL
    mutation = NULL
    chemical = NULL
    species = NULL
    temp <- test$passages[[2]]$annotations
    if (length(temp) != 0) {for ( i in 1:length(temp))
    { if (temp[[i]]$infons[2] == "Disease" ) disease = c(disease, paste(temp[[i]]$infons[1],temp[[i]]$text, sep = ">" ))
    else if (temp[[i]]$infons[2] == "Species" ) species = c(species, paste(temp[[i]]$infons[1],temp[[i]]$text, sep = ">" ))
    else if (temp[[i]]$infons[2] == "Mutation" ) mutation = c(mutation, paste(temp[[i]]$infons[1],temp[[i]]$text, sep = ">" ))
    else if (temp[[i]]$infons[2] == "Chemical" ) chemical = c(chemical, paste(temp[[i]]$infons[1],temp[[i]]$text, sep = ">" ))
    else if (temp[[i]]$infons[2] == "Gene" ) gene = c(gene, paste(temp[[i]]$infons[1],temp[[i]]$text, sep = ">" ))
    }
      gene = union(gene, gene)
      disease = union(disease, disease)
      mutation = union(mutation, mutation)
      chemical = union(chemical, chemical)
      species = union(species, species)
      return(list(Genes = gene, Diseases = disease, Mutations = mutation, Chemicals = chemical, Species = species, PMID = x))
      
    } else  
      temp <- test$passages[[1]]$annotations
    if (length(temp) != 0) {for ( i in 1:length(temp))
    { if (temp[[i]]$infons[2] == "Disease" ) disease = c(disease, paste(temp[[i]]$infons[1],temp[[i]]$text, sep = ">" ))
    else if (temp[[i]]$infons[2] == "Species" ) species = c(species, paste(temp[[i]]$infons[1],temp[[i]]$text, sep = ">" ))
    else if (temp[[i]]$infons[2] == "Mutation" ) mutation = c(mutation, paste(temp[[i]]$infons[1],temp[[i]]$text, sep = ">" ))
    else if (temp[[i]]$infons[2] == "Chemical" ) chemical = c(chemical, paste(temp[[i]]$infons[1],temp[[i]]$text, sep = ">" ))
    else if (temp[[i]]$infons[2] == "Gene" ) gene = c(gene, paste(temp[[i]]$infons[1],temp[[i]]$text, sep = ">" ))
    }
      gene = union(gene, gene)
      disease = union(disease, disease)
      mutation = union(mutation, mutation)
      chemical = union(chemical, chemical)
      species = union(species, species)
      return(list(Genes = gene, Diseases = disease, Mutations = mutation, Chemicals = chemical, Species = species, PMID = x))}
    else return(" No Data " )};return(" No Data ")}