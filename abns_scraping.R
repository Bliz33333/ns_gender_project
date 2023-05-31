p_load(httr, rjson)

headers = c(
  `authority` = "abns-fan.abns.org",
  `accept` = "*/*",
  `accept-language` = "en-US,en;q=0.9",
  `content-type` = "application/json",
  `origin` = "https://abns-neuro.abns.org",
  `referer` = "https://abns-neuro.abns.org/",
  `sec-ch-ua` = '"Google Chrome";v="113", "Chromium";v="113", "Not-A.Brand";v="24"',
  `sec-ch-ua-mobile` = "?0",
  `sec-ch-ua-platform` = '"Windows"',
  `sec-fetch-dest` = "empty",
  `sec-fetch-mode` = "cors",
  `sec-fetch-site` = "same-site",
  `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/113.0.0.0 Safari/537.36"
)

data_raw <- c('{"operationName":"paginatedNeurosurgeons","variables":{"clientName":"ABNS","input":{"first":',',"skip":','}},"query":"query paginatedNeurosurgeons($clientName: String!, $input: PaginatedNeurosurgeonsInput!) {\\n  paginatedNeurosurgeons(clientName: $clientName, input: $input) {\\n    total\\n    data {\\n      id\\n      firstName\\n      lastName\\n      profile {\\n        id\\n        suffix\\n        associationPublicStatus\\n        userType\\n        profilePictureURL\\n        addresses {\\n          id\\n          street1\\n          street2\\n          city\\n          state\\n          country\\n          zipCode\\n          primary\\n          __typename\\n        }\\n        contactNumbers {\\n          id\\n          contactNumber\\n          primary\\n          __typename\\n        }\\n        userCertifications {\\n          id\\n          beginDate\\n          endDate\\n          status\\n          certification {\\n            id\\n            name\\n            sortOrder\\n            hideEndDateInFAP\\n            ifActiveShowFirstDayOfCurrentYearInFAP\\n            __typename\\n          }\\n          recertifications {\\n            id\\n            cycleBeginDate\\n            cycleEndDate\\n            __typename\\n          }\\n          __typename\\n        }\\n        __typename\\n      }\\n      __typename\\n    }\\n    __typename\\n  }\\n}\\n"}')

num_pull <- 500
skip <- 0

res <- httr::POST(url = "https://abns-fan.abns.org/", httr::add_headers(.headers=headers), body = paste0(data_raw[1],num_pull,data_raw[2],skip,data_raw[3]))
num_results <- (res[["content"]] %>% rawToChar() %>% fromJSON())[["data"]][["paginatedNeurosurgeons"]][["total"]]
res_data <- (res[["content"]] %>% rawToChar() %>% fromJSON())[["data"]][["paginatedNeurosurgeons"]][["data"]]

for (i in seq(num_pull,num_results, by = num_pull)) 
{
  skip <- i
  temp_request <- res <- httr::POST(url = "https://abns-fan.abns.org/", httr::add_headers(.headers=headers), body = paste0(data_raw[1],num_pull,data_raw[2],skip,data_raw[3]))
  temp_data <- (temp_request[["content"]] %>% rawToChar() %>% fromJSON())[["data"]][["paginatedNeurosurgeons"]][["data"]]
  res_data <- c(res_data, temp_data)
  print(i)
  Sys.sleep(5)
}

first_names <- character()
last_names <- character()

first_names <- sapply(res_data, "[[", "firstName")
last_names <- sapply(res_data, "[[", "lastName")

abns_tibble <- tibble(cbind(first_names, last_names))

if(!file.exists("./data/abns_raw"))
{
  save(res_data, file = "./data/abns_raw")
}
if(!file.exists("./data/abns_names"))
{
  save(abns_tibble, file = "./data/abns_names")
}
