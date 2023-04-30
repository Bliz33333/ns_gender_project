p_load(tidyverse, rentrez, XML, readxl)

# entrez_key <- "Your Key Here"
# save(entrez_key, file = "./data/entrez_key")

load("./data/entrez_key")
set_entrez_key(entrez_key)
#Sys.getenv("ENTREZ_KEY")



 

# count <- numeric(length = nrow(journals))
# for (i in 1:nrow(journals))
# {
#   temp_search <- (entrez_search(db="pubmed", term = paste0(journals[i,2],"[JOUR] AND 2010:2023[EDAT]")))
#   count[i] <- temp_search$count
# }

file_locs <- matrix(nrow = nrow(journals), ncol = num_years)
file_names <- file_locs
for (i in 1:nrow(file_locs)) 
{
  for(j in 1:ncol(file_locs))
  {
    file_names[i,j] <- paste0(journals[i,2],"_",(start_year-1+j))
    file_locs[i,j] <- paste0("./data/yearly/", journals[i,2],"_",(start_year-1+j))
  }
  
}
rownames(file_locs) <- unlist(journals[,2])
colnames(file_locs) <- start_year:end_year
rownames(file_names) <- unlist(journals[,2])
colnames(file_names) <- start_year:end_year
save(file_locs, file = "file_locs")
save(file_names, file = "file_names")


done_journals <- matrix(file.exists(file_locs), nrow = nrow(file_locs), ncol = ncol(file_locs))
rownames(done_journals) <- rownames((file_locs))
colnames(done_journals) <- colnames((file_locs))

attempts <- 0
while((done_journals %>% sum()) < (nrow(done_journals) * ncol(done_journals)) && (attempts <50))
{
  done_journals <- matrix(file.exists(file_locs), nrow = nrow(file_locs), ncol = ncol(file_locs))
  attempts <- attempts + 1
  try(
    for (i in 1:nrow(file_locs)) 
    {
      
      for(j in 1:ncol(file_locs))
      {
        if(!done_journals[i,j])
        {
          raw_data <- list()
          temp_search <- (entrez_search(db="pubmed", term = paste0(journals[i,2],"[JOUR] AND ",(start_year+j-1),"[EDAT]"), use_history = T))
          temp_count <- temp_search$count
          temp_history <- temp_search$web_history
          
          if(temp_count > 0)
          {
            by <- 100
            if(temp_count %% by == 1)
            {
              by <- 101
            }
            for( seq_start in seq(1,temp_count,by)){
              recs <- entrez_fetch(db="pubmed", web_history=temp_history,
                                   rettype="xml", retmax=by, retstart=seq_start, parsed = T)
              raw_data <- c(raw_data, xmlToList(recs))
              #save(raw_data, file="raw_data")
              paste(seq_start+by-1, "sequences downloaded\r") %>% print()
            }
            
          }

          saveit(raw_data, string = file_names[i,j], file = file_locs[i,j] )
        }
      }
      print(paste0("Done with ", journals[i,2]))
      done_journals <- matrix(file.exists(file_locs), nrow = nrow(file_locs), ncol = ncol(file_locs))
    }
  )
  done_journals <- matrix(file.exists(file_locs), nrow = nrow(file_locs), ncol = ncol(file_locs))
  
}




# load(paste0("./data/", journals[14,2]))



# full_raw_data <- list()
# for(i in 1:nrow(file_locs))
# {
#   for(j in 1:ncol(file_locs))
#   {
#     load(file_locs[i,j])
#     full_raw_data <- c(full_raw_data,get(file_names[i,j]))
#     rm(list = paste0(file_names[i,j]))
#     print(file_names[i,j])
#   }
# }
#save(full_raw_data, file = "./data/full_raw_data")





#----------------
# 
# entrez_db_searchable("pubmed")
# 
# 
# temp_3 <- (entrez_search(db="pubmed", term = paste0("Nature","[JOUR] AND 2010:2023[EDAT]")))
# temp_3
# temp_3$QueryTranslation
# 
# temp_3 <- (entrez_search(db="pubmed", term = paste0("Nature","[JOUR] AND 2010:2015[EDAT]")))
# temp_3$QueryTranslation
# temp_3
# 
# temp_3 <- (entrez_search(db="pubmed", term = paste0("Nature","[JOUR] AND 2016:2023[EDAT]")))
# temp_3$QueryTranslation
# temp_3
# 
# temp_id <- 36700752
# 
# temp_xml <- entrez_fetch(db = "pubmed", id = temp_id, rettype = "xml", parsed = T)
# auths<-xpathSApply(temp_xml, "//Author", xmlValue)
# affiliations<-xpathSApply(temp_xml, "//Affiliation", xmlValue)
# 
# 
# 
# temp_list <- xmlToList(temp_xml)
# temp_auth <- temp_list[["PubmedArticle"]][["MedlineCitation"]][["Article"]][["AuthorList"]]
# 
# 
# temp_ids <- entrez_search(db="pubmed", term = "Science[JOUR] AND 2010:2023[PDAT]")
# temp_ids
# 
# temp_xml_2 <- entrez_fetch(db = "pubmed", id = temp_ids, rettype = "xml", parsed = T)
# temp_list_2 <- xmlToList(temp_xml_2)
