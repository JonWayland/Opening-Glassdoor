#########################################
### Data Science Job Posts Web-Scrape ###
#########################################
# Written by Jon Wayland
# 1/21/2019


#################################
### Defining Helper Functions ###
#################################

# To be able to use substr from the right-hand side
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# To produce the url for a search on 'data scientist' for a specific location
urlByLocation <- function(location_code){
  paste0("https://www.glassdoor.com/Job/jobs.htm?suggestCount=0&suggestChosen=false&clickSource=searchBtn&typedKeyword=data+scientist&sc.keyword=data+scientist&locT=C&locId=",
         location_code,
         "&jobType=")
}


######################
### Data Gathering ###
######################

# Pulling the first page of ~30 results by location

# Compiling list of locations
Location_Codes <- c(
  "1152990", # Pittsburgh
  "1152672", # Philadelphia
  "1132348", # New York
  "1154532", # Boston
  "1153527", # Baltimore
  "1138213", # Washington DC
  "1138960", # Raleigh
  "1147401", # San Francisco
  "1139761", # Austin
  "1154170", # Miami
  "1148170", # Denver
  "1131850", # Buffalo
  "1144541", # Nashville
  "1149603", # Las Vegas
  "1146821", # Los Angeles
  "1140494", # San Antonio
  "1150505", # Seattle
  "1151614", # Portland
  "1128808", # Chicago
  "1133904", # Phoenix
  "1139977", # Dallas
  "1147311", # San Diego
  "1147436", # San Jose
  "1154093", # Jacksonville
  "1145845", # Columbus
  "1139993", # Fort Worth
  "1145013", # Indianapolis
  "1138644", # Charlotte
  "1134644", # Detroit
  "1144463", # Memphis
  "1136950", # Oklahoma City
  "1137724", # Louisville
  "1133579", # Milwaukee
  "1133996", # Tucson
  "1145778", # Cleveland
  "1137959", # Albuquerque
  "1147229", # Sacramento
  "1131040", # Kansas City
  "1155583", # Atlanta
  "1147380", # Oakland
  "1131270", # St. Louis
  "1130337", # Arlington
  "1154247", # Orlando
  "1154429", # Tampa
  "1140171", # Houston
  "1148136", # Colorado Springs
  "1130324"  # Virginia Beach
)

# Defining DataFrame
DSdata <- data.frame(
  URL = as.character(),
  Description = as.character(),
  Job_Title = as.character(),
  Company_Rating = as.character(),
  Company_Name = as.character(),
  Company_Location = as.character(),
  Estimated_Salary = as.character(),
  Location_Code = as.character()
)

########################
### Pulling All Data ###
########################

for(j in 1:length(Location_Codes)){
  
  print(paste("----Step 1 for location ", Location_Codes[j], "(",j,")")) #*******************************************************
  
  # Clearing the url_lsit
  url_list<-data.frame(URL=as.character())
  
  # Pulling the information for specified URL
  d<-read_html(urlByLocation(Location_Codes[j])) %>% 
    html_nodes(xpath='//*[@id="JobSearch"]') %>%
    html_text()
  
  print(paste("----Step 2 for location ", Location_Codes[j], "(",j,")")) #*******************************************************
  
  # Pulling back all specific posting urls
  dt<-data.frame(str_extract_all(d, 'https://.*'))
  colnames(dt)<-c("URL")
  url_list<-rbind(url_list,dt)
  
  print(paste("----Step 3 for location ", Location_Codes[j], "(",j,")")) #*******************************************************
  
  # Defining the URL field
  url_list$URL<-as.character(url_list$URL)
  url_list$URL<-substr(url_list$URL,1,nchar(url_list$URL)-1)
  
  print(paste("----Step 4 for location ", Location_Codes[j], "(",j,")")) #*******************************************************
  
  # Pulling the job description
  url_list$Description<-NA
  
  print(paste("----Step 5 for location ", Location_Codes[j], "(",j,")")) #*******************************************************
  
  for(i in 1:nrow(url_list)){
    tryCatch({
      link <- as.character(url_list$URL[i])
      web<-read_html(link)
      url_list$Description[i] <- web %>% html_node(".desc") %>% html_text()
    }, error=function(e){})
  }
  
  print(paste("----Step 6 for location ", Location_Codes[j], "(",j,")")) #*******************************************************
  
  # Pulling the job title
  url_list$Job_Title<-NA
  
  for(i in 1:nrow(url_list)){
    tryCatch({
      link <- as.character(url_list$URL[i])
      web<-read_html(link)
      url_list$Job_Title[i] <- web %>% html_nodes(xpath='//*[@id="HeroHeaderModule"]/div[3]/div[1]/div[2]/div[1]/h2') %>% html_text()
    }, error=function(e){})
  }
  
  print(paste("----Step 7 for location ", Location_Codes[j], "(",j,")")) #*******************************************************
  
  # Pulling the company rating
  url_list$Company_Rating<-NA
  
  for(i in 1:nrow(url_list)){
    tryCatch({
      link <- as.character(url_list$URL[i])
      web<-read_html(link)
      url_list$Company_Rating[i] <- web %>% html_nodes(xpath='//*[@id="HeroHeaderModule"]/div[3]/div[1]/div[2]/span[1]') %>% html_text()
    }, error=function(e){})
  }
  ## Note: If the rating isn't populated then the company name will appear here
  
  print(paste("----Step 8 for location ", Location_Codes[j], "(",j,")")) #*******************************************************
  
  # Pulling the company name
  url_list$Company_Name<-NA
  
  for(i in 1:nrow(url_list)){
    tryCatch({
      link <- as.character(url_list$URL[i])
      web<-read_html(link)
      url_list$Company_Name[i] <- web %>% html_nodes(xpath='//*[@id="HeroHeaderModule"]/div[3]/div[1]/div[2]/span[2]') %>% html_text()
    }, error=function(e){})
  }
  ## Note: If the rating isn't populated then the company location will appear here
  
  print(paste("----Step 9 for location ", Location_Codes[j], "(",j,")")) #*******************************************************
  
  # Pulling the company location
  url_list$Company_Location<-NA
  
  for(i in 1:nrow(url_list)){
    tryCatch({
      link <- as.character(url_list$URL[i])
      web<-read_html(link)
      url_list$Company_Location[i] <- web %>% html_nodes(xpath='//*[@id="HeroHeaderModule"]/div[3]/div[1]/div[2]/span[3]') %>% html_text()
    }, error=function(e){})
  }
  ## Note: If the rating isn't populated then the company location will appear BLANK here
  
  print(paste("----Step 10 for location ", Location_Codes[j], "(",j,")")) #*******************************************************
  
  # Pulling the company location
  url_list$Estimated_Salary<-NA
  
  for(i in 1:nrow(url_list)){
    tryCatch({
      link <- as.character(url_list$URL[i])
      web<-read_html(link)
      url_list$Estimated_Salary[i] <- web %>% html_nodes(xpath='//*[@id="salWrap"]/h2') %>% html_text()
    }, error=function(e){})
  }
  ## Note: If the rating isn't populated then the company location will appear BLANK here
  
  #########################################################
  ### Making Adjustments for Where Rating Not Populated ###
  #########################################################
  #library(tidyverse)
  
  print(paste("----Step 11 for location ", Location_Codes[j], "(",j,")")) #*******************************************************
  
  url_list <- url_list %>% 
    mutate(
      Company_Location = case_when(nchar(Company_Rating) > 4 ~ Company_Name, TRUE ~ Company_Location),
      Company_Name = case_when(nchar(Company_Rating) > 4 ~ Company_Rating, TRUE ~ Company_Name)
    ) %>% 
    mutate(Company_Rating = case_when(nchar(Company_Rating) > 4 ~ "NA", TRUE ~ Company_Rating))
  
  print(paste("----Step 12 for location ", Location_Codes[j], "(",j,")")) #*******************************************************
  
  url_list$Location_Code<-Location_Codes[j]
  
  DSdata <- rbind(DSdata,url_list)
  rm(url_list)
}

# Save as a dataframe and csv file
#write.csv(DSdata, file = "C:\\Users\\jonwa\\Desktop\\Data Science Certificate - Harvard\\Capstone Project Files\\DSdata.csv")
#save(DSdata, file = "C:\\Users\\jonwa\\Desktop\\Data Science Certificate - Harvard\\Capstone Project Files\\DSdata.Rda")
