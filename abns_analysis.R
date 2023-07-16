p_load(stringr, stringi)
load(file = "./data/tabulated_data_recoded")
load(file = "./data/abns_names")
load(file = "./data/abns_raw")
load(file = "./data/prev_gender_table")

tabulated_data <- tibble(tabulated_data)
abns_tibble <- matrix(data = unlist(abns_tibble), nrow = nrow(abns_tibble), ncol = 2) %>% as.data.frame() %>% as_tibble()
colnames(abns_tibble) <- c("ForeName","LastName")

fore_name_clean_indiv <- function(fname)
{
  
  fname <- tolower(fname)
  
  fname <- word(fname,1)
  
  
  # if(length(grepl(pattern = ".", x=fname ,fixed = T)) > 1)
  # {
  #   print(fname)
  # }
  
  if(grepl(pattern = ".", x=fname ,fixed = T))
  {
    return(NULL)
  }
  
  
  if(nchar(fname)==1)
  {
    return(NULL)
  }
  
  fname <- trimws(fname)
  
  fname <- stri_trans_general(str = fname, id = "Latin-ASCII")
  
  return(fname)
}

last_name_clean_indiv <- function(lname)
{
  lname <- tolower(lname)
  
  lname <- stri_trans_general(str = lname, id = "Latin-ASCII")
  
  lname = trimws(lname)
  
  return(lname)
}

fore_name_clean <- function(fnames)
{
  return(sapply(fnames, fore_name_clean_indiv))
}

last_name_clean <- function(lnames)
{
  return(sapply(lnames, last_name_clean_indiv))
}

#cleaning: remove leading/trailing whitespace?

abns_cleaned <-
  abns_tibble %>% 
  mutate(ForeName = fore_name_clean(ForeName))

abns_cleaned <-
  abns_cleaned %>% 
  mutate(LastName = last_name_clean(LastName))

abns_cleaned <-
  abns_cleaned %>% 
  filter(!grepl(pattern = "NULL", x=ForeName ,fixed = T)) %>% 
  filter(!grepl(pattern = "NULL", x=LastName ,fixed = T))

#uneeded
#-------------
#match criteria: first name is at start of string and last name is later in the string, seperated from the first name by at least one character
matches <- list(length = nrow(abns_cleaned))
for (i in 1:nrow(abns_cleaned)) {
  
  freg <- regexpr(abns_cleaned$ForeName[i], gender_table$Name, fixed = T) %>% as.integer()
  
  lreg <- regexpr(abns_cleaned$LastName[i], gender_table$Name, fixed = T) %>% as.integer()
  
  res <- (((freg +nchar(abns_cleaned$ForeName[i])) < lreg) & (freg == 1)) %>% which()

  if((i %% 100) == 0)
  {
    print(i)
  }
  
  matches[[i]] <- res
}

length(unlist(matches))
#4741
#4522
#4389

matches_vec <- integer(length(matches))
for (i in 1:length(matches)) 
{
  if(length(matches[[i]]) != 0)
  {
    gend <- unique(gender_table$Gender[matches[[i]]])
    if(length(gend) > 1)
    {
      print(i)
      print((gender_table$Gender[matches[[i]]]))
    } else {
      matches_vec[i] <- gend
    }
  }
}
sum(matches_vec != 0)
#3292

abns_gender <- cbind(abns_cleaned, tibble(matches_vec)) %>% tibble()
#----------------


#link to changes in reconcile
fullname_data <-
  tabulated_data %>%
  mutate(FA_FullName = paste(FA_ForeName,FA_LastName)) %>% 
  mutate(FA_FullName = stri_trans_general(str = FA_FullName, id = "Latin-ASCII")) %>% 
  mutate(FA_FullName = tolower(FA_FullName)) %>% 
  mutate(FA_FullName = trimws(FA_FullName)) %>% 
  mutate(LA_FullName = paste(LA_ForeName,LA_LastName)) %>% 
  mutate(LA_FullName = stri_trans_general(str = LA_FullName, id = "Latin-ASCII")) %>% 
  mutate(LA_FullName = tolower(LA_FullName)) %>% 
  mutate(LA_FullName = trimws(LA_FullName))

fullname_data <-
  fullname_data %>% 
  mutate(FA_Gender = character(1)) %>% 
  mutate(LA_Gender = character(1))

for (i in j:nrow(gender_table)) {
  fullname_data$FA_Gender[fullname_data$FA_FullName == gender_table$Name[i]] <- gender_table$Gender[i]
  fullname_data$LA_Gender[fullname_data$LA_FullName == gender_table$Name[i]] <- gender_table$Gender[i]
  if(i %% 500 == 0) {print(i)}
  j <- i
}

fullname_data <-
  fullname_data %>% 
  mutate(FA_NS = logical(1)) %>% 
  mutate(LA_NS = logical(1)) %>% 
  as.tibble()

#match criteria: first name is at start of string and last name is later in the string, seperated from the first name by at least one character

#FA
{
matches <- list(length = nrow(abns_gender))
for (i in 1:nrow(abns_gender)) {
  
  freg <- regexpr(abns_gender$ForeName[i], fullname_data$FA_FullName, fixed = T) %>% as.integer()
  
  lreg <- regexpr(abns_gender$LastName[i], fullname_data$FA_FullName, fixed = T) %>% as.integer()
  
  res <- (((freg +nchar(abns_gender$ForeName[i])) < lreg) & (freg == 1)) %>% which()
  
  if((i %% 100) == 0)
  {
    print(i)
  }
  
  matches[[i]] <- res
}

fullname_data$FA_NS[unique(unlist(matches))] <- T

matches_vec <- logical(length = length(matches))
for(i in 1:length(matches))
{
  if(length(matches[[i]]) > 0)
  {
    matches_vec[i] <- T
  }
}
sum(matches_vec)
length(matches_vec)

matches_vec_fa <- matches_vec
}

#LA
{
  matches <- list(length = nrow(abns_gender))
  for (i in 1:nrow(abns_gender)) {
    
    freg <- regexpr(abns_gender$ForeName[i], fullname_data$LA_FullName, fixed = T) %>% as.integer()
    
    lreg <- regexpr(abns_gender$LastName[i], fullname_data$LA_FullName, fixed = T) %>% as.integer()
    
    res <- (((freg +nchar(abns_gender$ForeName[i])) < lreg) & (freg == 1)) %>% which()
    
    if((i %% 100) == 0)
    {
      print(i)
    }
    
    matches[[i]] <- res
  }
  
  fullname_data$LA_NS[unique(unlist(matches))] <- T

  matches_vec <- logical(length = length(matches))
  for(i in 1:length(matches))
  {
    if(length(matches[[i]]) > 0)
    {
      matches_vec[i] <- T
    }
  }
  sum(matches_vec)
  length(matches_vec)
  matches_vec_la <- matches_vec
}





if(!file.exists("./data/fullname_data"))
{
  save(fullname_data, file = "./data/fullname_data")
}

#other
#-----------------
articles <- 
  tabulated_data %>% 
  filter(grepl(pattern = "Article", x = Type,fixed = T)) %>% 
  filter(PubDate >= 2010)

temp2 <- sapply(res_data, "[[", c("profile","userType"))

nulls2 <- sapply(temp2, is.null)
nulls2 <- which(nulls2)



# #lowercase everything
# abns_cleaned <-
#   abns_tibble %>% 
#   mutate(ForeName = tolower(ForeName)) %>% 
#   mutate(LastName = tolower(LastName))
# 
# #first word of first names
# abns_cleaned <-
#   abns_cleaned %>% 
#   mutate(ForeName = word(ForeName,1)) 
# 
# #eliminate initials with .
# abns_cleaned <-
#   abns_cleaned %>% 
#   filter(!grepl(pattern = ".", x=ForeName ,fixed = T))
# 
# #eliminate initials by size
# abns_cleaned <-
#   abns_cleaned %>% 
#   filter(!nchar(ForeName) == 1)