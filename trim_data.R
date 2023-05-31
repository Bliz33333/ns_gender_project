p_load(tidyverse, readxl, XML, data.table)

load("file_locs")
load("file_names")




trim <- function(ref)
{
  trimmed <- matrix(nrow = 1, ncol = 5)
  trimmed <- data.frame(trimmed)
  medline <- ref[["MedlineCitation"]]
  article <- medline[["Article"]]
  PublicationTypes <- article[["PublicationTypeList"]] %>% unlist()
  PublicationTypes <- paste(PublicationTypes[names(PublicationTypes) == "PublicationType.text"], collapse = "!")
  
  trimmed <- 
    data.table(
      PMID = medline[["PMID"]][["text"]],
      Type = PublicationTypes,
      PubDate = article[["Journal"]][["JournalIssue"]][["PubDate"]][["Year"]],
      Journal = article[["Journal"]][["Title"]],
      AuthorList = I(list(article[["AuthorList"]]))
    )
  
}

if(!file.exists("./data/trimmed_data"))
{
  trimmed_data <- data.table()
  for(i in 1:nrow(file_locs))
  {
    for(j in 1:ncol(file_locs))
    {
      load(file_locs[i,j])
      trimmed_data <- rbind(trimmed_data,data.table(rbindlist(lapply(get(file_names[i,j]), trim), fill = T)))
      rm(list = paste0(file_names[i,j]))
      print(file_names[i,j])
    }
  }
  save(trimmed_data, file = "./data/trimmed_data")
} else {
  load(file = "./data/trimmed_data")
}





# TODO: deal with case where >1 affiliation
extracted <- 0
extract_author <- function(auths)
{
  #auths <- trimmed_data[,"AuthorList"][[1]][[1355]]
  
  extracted <<- extracted + 1
  
  auth_data <- data.table(matrix(data = NA, nrow = 1, ncol = 6))
  colnames(auth_data) <- c("FA_LastName","FA_ForeName", "FA_Affiliation", "LA_LastName","LA_ForeName", "LA_Affiliation")
  
  if(is.null(auths))
  {
    return(auth_data)
  }
  
  
  num_auths <- (names(auths) == "Author")
  # auths <- auths[num_auths]
  num_auths <- sum(num_auths)
  
  while(!is.null(auths[[num_auths]][["CollectiveName"]]))
  {
    num_auths <- num_auths - 1
    if(num_auths == 0)
    {
      return(auth_data)
    }
  }
  
  index_adjust <- 0
  
  while(!is.null(auths[[1+index_adjust]][["CollectiveName"]]))
  {
    index_adjust <- index_adjust + 1
    if(index_adjust >= num_auths)
    {
      print("this should not run")
      return(auth_data)
    }
  }
  
  FA <- auths[[1+index_adjust]]
  

  
  auth_data$FA_LastName <- FA[["LastName"]]
  auth_data$FA_ForeName <- FA[["ForeName"]]
  
  FA_afs <- (names(FA) == "AffiliationInfo")
  FA_num_afs <- sum(FA_afs)
  FA_afs <- which(FA_afs)
  
  FA_aff <- ""
  for (i in FA_afs) {
    if(grepl(pattern = "!", x = FA[[i]][["Affiliation"]], fixed = T))
    {
      print("AFFILIATION CONTAINS INTERROBANG")
    }
    
    if(FA_aff == "")
    {
      FA_aff <- FA[[i]][["Affiliation"]]
    } else {
      FA_aff <- paste(FA_aff, FA[[i]][["Affiliation"]] ,sep = "!")
    }
    
  }
  
  # FA_aff <- FA[["AffiliationInfo"]][["Affiliation"]]
  if(!is.null(FA_aff))
  {
    auth_data$FA_Affiliation <- FA_aff
  }
  
  # 
  # if(FA[["AffiliationInfo"]] %>% length() > 1)
  # {
  #   print("MORE THAN ONE AFFILIATION: ONLY FIRST ONE GRABBED")
  # }
  # 
  
  #remove if no issues
  if((is.null(auth_data$FA_LastName)) & (is.null(auth_data$FA_ForeName)))
  {
    print(paste("First Author does not have standard formatting",(extracted)))
  }
  
  if(num_auths == 1)
  {
    return(auth_data)
  }
  
  LA <- auths[[num_auths]]
  
  auth_data$LA_LastName <- LA[["LastName"]]
  auth_data$LA_ForeName <- LA[["ForeName"]]
  
  
  LA_afs <- (names(LA) == "AffiliationInfo")
  LA_num_afs <- sum(LA_afs)
  LA_afs <- which(LA_afs)
  
  LA_aff <- ""
  for (i in LA_afs) {
    if(grepl(pattern = "!", x = LA[[i]][["Affiliation"]], fixed = T))
    {
      print("AFFILIATION CONTAINS INTERROBANG")
    }
    
    if(LA_aff == "")
    {
      LA_aff <- LA[[i]][["Affiliation"]]
    } else {
      LA_aff <- paste(LA_aff, LA[[i]][["Affiliation"]] ,sep = "!")
    }
    
  }

  # LA_aff <- LA[["AffiliationInfo"]][["Affiliation"]]
  if(!is.null(LA_aff))
  {
    auth_data$LA_Affiliation <- LA_aff
  }
  # 
  # if(LA[["AffiliationInfo"]] %>% length() > 1)
  # {
  #   print("MORE THAN ONE AFFILIATION: ONLY FIRST ONE GRABBED")
  # }
  
  #remove if no issues
  if((is.null(auth_data$LA_LastName)) & (is.null(auth_data$LA_ForeName)))
  {
    print(paste("Last Author does not have standard formatting",(extracted)))
  }
  
  
  if(extracted %% 1000 == 0)
  {
    print(extracted)
  }
  #print(extracted)
  return(auth_data)
}



#mini <- trimmed_data[1:100,]
if(!file.exists("./data/tabulated_data"))
{
  extracted <- 0
  tabulated_data <- cbind(trimmed_data[,-"AuthorList"],lapply(trimmed_data[,"AuthorList"][[1]], extract_author) %>% rbindlist(fill = T))
  save(tabulated_data, file = "./data/tabulated_data")
} else {
  load(file = "./data/tabulated_data")
}



if(file.exists("./data/tabulated_data_recoded"))
{
  load(file = "./data/tabulated_data_recoded")
} else {
  
  # object.size(tabulated_data)
  tabulated_data <- tabulated_data[, PMID := as.integer(PMID)]
  # object.size(tabulated_data)
  tabulated_data <- tabulated_data[, PubDate := as.integer(PubDate)]
  # object.size(tabulated_data)
  tabulated_data <- tabulated_data[, Journal := as.factor(Journal)]
  # object.size(tabulated_data)
  
  save(tabulated_data, file = "./data/tabulated_data_recoded")
  
}


#--------------------
# 
# load("./data/yearly/JAMA_2018")
# for (i in 1:length(JAMA_2018)) {
#   if(JAMA_2018[[i]][["MedlineCitation"]][["PMID"]][["text"]] == 29710162)
#   {
#     print(i)
#   }
#   
# }
# 
# temp <- JAMA_2018[[1112]]
# 
# 
# # -----------------
# (Neurosurgery_2008[[37]][["MedlineCitation"]][["Article"]][["PublicationTypeList"]] %>% unlist())["PublicationType.text"]
# 
# 
# temp <- 
#   lapply(mini, trim) %>% setDT()
# 
# temp <- data.table(do.call(rbind, lapply(mini, trim)))
# 
# colnames(temp) <- as.character(1:10)
# 
# data.table(do.call(rbind, lapply(get(file_names[i,j]), trim)))
# load(file_locs[1,1])
# 
# mini <- Neurosurgery_2008[1:10]
# # full_raw_data <- list()
# # for(i in 1:nrow(file_locs))
# # {
# #   for(j in 1:ncol(file_locs))
# #   {
# #     load(file_locs[i,j])
# #     full_raw_data <- c(full_raw_data,get(file_names[i,j]))
# #     rm(list = paste0(file_names[i,j]))
# #     print(file_names[i,j])
# #   }
# # }