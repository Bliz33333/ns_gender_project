
abns_gendering <- abns_tibble


abns_gendering <- matrix(data = unlist(abns_tibble), nrow = nrow(abns_tibble), ncol = 2)

abns_gendering <- as_tibble(abns_gendering)


abns_gendering[,3] <- "US"

colnames(abns_gendering) <- c("First Name", "Last Name", "Country")


write_csv(abns_gendering, file = "./data/abns_gendering.csv")


abns_gendering_enriched <- read_csv("data/abns_gendering_enriched.csv")




abns_gender_filtered <- 
  abns_gendering_enriched %>% 
  filter(ga_accuracy >= 75) %>% 
  filter(ga_gender %in% c("male", "female")) %>% 
  select(`First Name`, `Last Name`, ga_gender)



name_clean_indiv <- function(fname)
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


name_clean <- function(names)
{
  return(sapply(names, name_clean_indiv))
}

abns_gender_filtered$`First Name` <- abns_gender_filtered$`First Name` %>% name_clean()
abns_gender_filtered$`Last Name` <- abns_gender_filtered$`Last Name` %>% name_clean()

na_na_cleaner <- function(name)
{
  if(name == "na na")
  {
    return(NA)
  } else {
    return(name)
  }
}


fullname_data_trim <- 
  fullname_data %>% 
  select(PMID, FA_FullName, LA_FullName)

for (i in 1:nrow(fullname_data_trim)) {
  
  fullname_data_trim$FA_FullName[i] <- na_na_cleaner(fullname_data_trim$FA_FullName[i])
  fullname_data_trim$LA_FullName[i] <- na_na_cleaner(fullname_data_trim$LA_FullName[i])
  
  if((i %% 1000) == 0)
  {
    print(i)
  }
}

temp <- (fullname_data_trim$FA_FullName == "na na")
fullname_data_trim$FA_FullName[temp] <- NA

temp <- (fullname_data_trim$LA_FullName == "na na")
fullname_data_trim$LA_FullName[temp] <- NA

name_match <- function(first, last, full)
{
  first_match <- str_locate_all(full, first)[[1]][1,1]
  
  last_match <- str_locate_all(full, last)[[1]][,1]
  
  last_match <- last_match[length(last_match)]
  
  spaces <- str_locate_all(full, " ")[[1]][,1]
  
  if(is_empty(spaces))
  {
    print("Full name without a space!")
    return(F)
  }
  
  if(is_empty(first_match))
  {
    return(F)
  }
  
  if(is_empty(last_match))
  {
    return(F)
  }
  
  if(sum((spaces > first_match) & (spaces < last_match)) >= 1)
  {
    return(T)
  } else {
    return(F)
  }
  
}
#test code
{
first <- "an"
last <- "ne"
full <- "ananenesda dne"

name_match(first, last, full)
}

#matching
#only last match, may be dupe matches
paper_genders <- 
  fullname_data_trim %>% 
  select(PMID) %>% 
  mutate(fa_male = 0) %>% 
  mutate(fa_female = 0) %>%
  mutate(la_male = 0) %>%
  mutate(la_female = 0)

sum(paper_genders$PMID == fullname_data_trim$PMID)
nrow(paper_genders)

#i = 5100
for (i in 5100:nrow(abns_gender_filtered)) 
{
  fname <- abns_gender_filtered$`First Name`[i] %>% unlist()
  lname <- abns_gender_filtered$`Last Name`[i] %>% unlist()
  gender <- abns_gender_filtered$ga_gender[i] %>% unlist()
  
  if(!(is.null(fname) | is.null(lname) | is.null(gender)))
  {
    poss <- (grepl(fname, fullname_data_trim$FA_FullName) & grepl(lname, fullname_data_trim$FA_FullName)) %>% which()
    if(length(poss) > 0)
    {
      for (j in poss) 
      {
        if(name_match(fname, lname, unlist(fullname_data_trim$FA_FullName[j])))
        {
          if(gender == "male")
          {
            paper_genders$fa_male[j] <- i
          } else {
            paper_genders$fa_female[j] <- i
          }
        }
      }
    }
    
    poss <- (grepl(fname, fullname_data_trim$LA_FullName) & grepl(lname, fullname_data_trim$LA_FullName)) %>% which()
    if(length(poss) > 0)
    {
      for (j in poss) 
      {
        if(name_match(fname, lname, unlist(fullname_data_trim$LA_FullName[j])))
        {
          if(gender == "male")
          {
            paper_genders$la_male[j] <- i
          } else {
            paper_genders$la_female[j] <- i
          }
        }
      }
    }
  }
  


  if((i %% 10) == 1 )
  {
    print(i)
  }
}

{
  i <- 1
}

sum(paper_genders$PMID == tabulated_data$PMID)
nrow(paper_genders)

gendered_paper_data <- cbind(tabulated_data, paper_genders[,-1])
gendered_paper_data <-
  gendered_paper_data %>% 
  filter((fa_male > 0 | fa_female > 0| la_male > 0 | la_female) > 0) %>% 
  mutate(fa_gender = "none") %>% 
  mutate(la_gender = "none")

gendered_paper_data$fa_gender[((gendered_paper_data$fa_male == 0) & !(gendered_paper_data$fa_female == 0))] <- "female"
gendered_paper_data$fa_gender[(!(gendered_paper_data$fa_male == 0) & (gendered_paper_data$fa_female == 0))] <- "male"
gendered_paper_data$fa_gender[(!(gendered_paper_data$fa_male == 0) & !(gendered_paper_data$fa_female == 0))] <- "mix"

gendered_paper_data$la_gender[((gendered_paper_data$la_male == 0) & !(gendered_paper_data$la_female == 0))] <- "female"
gendered_paper_data$la_gender[(!(gendered_paper_data$la_male == 0) & (gendered_paper_data$la_female == 0))] <- "male"
gendered_paper_data$la_gender[(!(gendered_paper_data$la_male == 0) & !(gendered_paper_data$la_female == 0))] <- "mix"

if(!file.exists("./data/gendered_paper_data"))
{
  save(gendered_paper_data, file = "./data/gendered_paper_data")
}


gendered_paper_data %>% 
  filter(fa_gender != "none") %>%
  filter(PubDate >= 2010) %>%
#  filter(la_gender == "female") %>% 
  ggplot(aes(x = PubDate, fill = fa_gender)) +
  geom_bar(position = position_dodge())

gendered_paper_data %>% 
  filter(PubDate >= 2010) %>%
  filter(la_gender != "none") %>% 
  #  filter(la_gender == "female") %>% 
  ggplot(aes(x = PubDate, fill = la_gender)) +
  geom_bar(position = position_dodge())

gendered_paper_data %>% 
  filter(PubDate >= 2010) %>%
  filter(fa_gender != "none") %>% 
  #  filter(la_gender == "female") %>% 
  ggplot(aes(x = PubDate, fill = fa_gender)) +
  geom_bar(position = "fill")

gendered_paper_data %>% 
  filter(PubDate >= 2010) %>%
  filter(la_gender != "none") %>% 
  #  filter(la_gender == "female") %>% 
  ggplot(aes(x = PubDate, fill = la_gender)) +
  geom_bar(position = "fill")


gendered_paper_data %>% 
  filter(fa_gender != "none") %>% 
  filter(Journal %in% journals$common_name[journals$type == "nsurg"]) %>% 
  ggplot(aes(x = PubDate, fill = fa_gender)) +
  geom_bar(position = position_dodge())


papers_per_index <-
  c(gendered_paper_data$fa_male, gendered_paper_data$la_male, gendered_paper_data$fa_female, gendered_paper_data$la_female) %>%
  table(dnn = "Index") %>% 
  as_tibble() %>% 
  filter(Index > 0)

index_to_name <- function(this_index)
{
  return(paste(abns_gender_filtered[this_index,]$"First Name", abns_gender_filtered[this_index,]$"Last Name"))
}

papers_per_name <-
  papers_per_index %>% 
  mutate(name <- sapply(Index,index_to_name))

papers_per_index %>% 
  select(n) %>% 
  unlist() %>% 
  log2() %>% 
  hist()

(c(gendered_paper_data$fa_male, gendered_paper_data$la_male, gendered_paper_data$fa_female, gendered_paper_data$la_female) ) %>%
  table(dnn = "Index") %>% 
  as_tibble() %>% 
  filter(Index > 0) %>%
  select(n) %>% 
  unlist() %>% 
  table(dnn = "Prolif") %>% 
  as_tibble() %>% 
  View()

