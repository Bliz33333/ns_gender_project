p_load(tidyverse, data.table, stringi)

load(file = "./data/tabulated_data_recoded")

prev_data <- read_excel("data/Master File.xlsx", sheet = "Cumulative ")
prev_journals <- c("Neurosurgery", "Journal of neurosurgery", "Neurosurgical focus", "World neurosurgery", "Journal of neurosurgery. Spine", "Journal of neurosurgery. Pediatrics", "Journal of neuro-oncology", "Spine", "Journal of clinical neuroscience : official journal of the Neurosurgical Society of Australasia", "Epilepsia")


first_authors <- prev_data[,c("First Author", "First Author_gender", "ga_accuracy...5")]
colnames(first_authors) <- c("Name", "Gender", "Accuracy")

last_authors <- prev_data[,c("Last Author", "Last Author_gender", "ga_accuracy...8")]
colnames(last_authors) <- c("Name", "Gender", "Accuracy")

gender_table <- rbind(first_authors, last_authors)
gender_table <-
  gender_table %>% 
  filter(Gender %in% c("male","female"))
gender_table <- 
  gender_table %>% 
  filter(!duplicated(Name))

#90% accuracy+ filter
gender_table <- 
  gender_table %>% 
  filter(Accuracy >= 90)

#cleaning: remove leading/trailing whitespace?
#lowercase + to ascii
#link to changes in abns_analysis
gender_table <-
  gender_table %>% 
  mutate(Name = stri_trans_general(str = Name, id = "Latin-ASCII")) %>% 
  mutate(Name = tolower(Name)) %>% 
  mutate(Name = trimws(Name))


if(!file.exists("./data/prev_gender_table"))
{
  save(gender_table, file = "./data/prev_gender_table")
}

# Probably unneeded

forename_gender_table <-
  gender_table

forename_gender_table <- 
  forename_gender_table %>% 
  rowwise() %>% 
  mutate(Name = strsplit(unlist(Name), split = " ",fixed = T,)[[1]][1]) %>% 
  ungroup()

forename_gender_table <-
  forename_gender_table %>% 
  filter(nchar(Name) > 1)

forename_gender_table <-
  forename_gender_table %>% 
  mutate(Accuracy  = Accuracy * (-1 + (Gender == "male")*2))

unique_forenames <- unique(forename_gender_table$Name)
temp_matrix <- data.table(matrix(nrow = length(unique_forenames), ncol = 2))
colnames(temp_matrix) <- c("Name","Accuracy")
temp_matrix$Name <- as.character(temp_matrix$Name)
temp_matrix$Accuracy <- as.numeric(temp_matrix$Accuracy)
for (i in 1:length(unique_forenames)) 
{
  
  temp_matrix[i,1] <- unique_forenames[i]
  
  selection <- 
    forename_gender_table %>% 
    filter(Name == unique_forenames[i]) %>% 
    select(Accuracy) %>% 
    unlist()
  
  temp_matrix[i,2] <- mean(selection)
  
  if(i %% 1000 == 0)
  {
    print(i)
  }
}
temp_matrix <- 
  temp_matrix %>% 
  filter(abs(Accuracy) > 75)

temp_matrix <-
  temp_matrix %>% 
  mutate(Gender = case_when(Accuracy < 0 ~ "female", Accuracy > 0 ~ "male"))

temp_matrix <-
  temp_matrix %>% 
  mutate(Accuracy = -abs(Accuracy))

forename_gender_table <- temp_matrix

tabulated_data <-
  tabulated_data %>% 
  rowwise() %>% 
  mutate(FA_FullName = construct_full_names(FA_LastName,FA_ForeName)) %>% 
  ungroup()

tabulated_data <-
  tabulated_data %>% 
  rowwise() %>% 
  mutate(LA_FullName = construct_full_names(LA_LastName,LA_ForeName)) %>% 
  ungroup()



construct_full_names <- function(lname, fname)
{
  if(is.na(lname))
  {
    if(is.na(fname))
    {
      return(NA)
    }
    print("First Name Without Last Name")
    return(fname)
  }
  if(is.na(fname))
  {
    return(lname)
  }
  return(paste(fname,lname))
}

#201968
colnames(gender_table) <- c("FA_FullName", "FA_Gender", "FA_Confidence")
tabulated_data <- merge(tabulated_data,gender_table,by="FA_FullName", all.x = T)
colnames(gender_table) <- c("LA_FullName", "LA_Gender", "LA_Confidence")
tabulated_data <- merge(tabulated_data,gender_table,by="LA_FullName", all.x = T)
colnames(gender_table) <- c("Name", "Gender", "Accuracy")

prev_filter <-
  tabulated_data %>% 
  filter(PubDate <= 2020) %>% 
  filter(PubDate >= 2010) %>% 
  filter(Journal %in% prev_journals)

prev_filter$FA_FullName %>% length()
sum(prev_filter$FA_FullName %in% gender_table$Name)

prev_filter$LA_FullName %>% length()
sum(prev_filter$LA_FullName %in% gender_table$Name)

tabulated_data$FA_FullName %>% length()
sum(tabulated_data$FA_FullName %in% gender_table$Name)

tabulated_data$LA_FullName %>% length()
sum(tabulated_data$LA_FullName %in% gender_table$Name)
