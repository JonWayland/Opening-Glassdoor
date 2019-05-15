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

# Defining the location cities from search
LocationDF <- data.frame(Location_Code = Location_Codes, City = c(
  "Pittsburgh","Philadelphia","New York","Boston","Baltimore","Washington DC","Raleigh",
  "San Francisco","Austin","Miami","Denver","Buffalo","Nashville","Las Vegas","Los Angeles",
  "San Antonio","Seattle","Portland","Chicago","Phoenix","Dallas","San Diego","San Jose",
  "Jacksonville","Columbus","Fort Worth","Indianapolis","Charlotte","Detroit","Memphis",
  "Oklahoma City","Louisville","Milwaukee","Tucson","Cleveland","Albuquerque","Sacramento",
  "Kansas City","Atlanta","Oakland","St. Louis","Arlington","Orlando","Tampa","Houston",
  "Colorado Springs","Virginia Beach"))

# Pull substrings from the right side of a string
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# Read in the scraped data
DSdata <- read.csv("https://raw.githubusercontent.com/JonWayland/Opening-Glassdoor/master/DSdata.csv")

# Load libraries
library(data.table)
library(tidyverse)
library(tm)
library(SnowballC)
library(wordcloud)
library(RColorBrewer)
library(tidytext)
library(scales)

###########################
### Feature Engineering ###
###########################

# Cleaning up Company_Rating
DSdata <- DSdata %>% 
  mutate(Company_Rating = trimws(Company_Rating, which = "left"))

# Looking at the frequency of each rating
DSdata %>%
  group_by(Company_Rating) %>%
  summarize(N=n()) %>% 
  knitr::kable()

# Removing observations where Company_Rating is missing (sign that Glassdoor does not have much data on company)
DSdata <- DSdata %>% 
  filter(!is.na(Company_Rating)) %>%
  mutate(Company_Rating = as.numeric(Company_Rating))

# Looking at the distribution of ratings (visually)
DSdata %>% 
  ggplot(aes(Company_Rating))+
  geom_histogram(color="darkblue",fill="lightblue",binwidth=0.1)+
  xlab("Company Rating") +
  theme_classic()

# Looking at the distribution of estimated salaries
DSdata %>%
  filter(Company_Rating != "NA") %>% 
  group_by(Estimated_Salary) %>%
  summarize(N=n()) %>% 
  knitr::kable()

# Converting salary to numeric and accounting for measurement (i.e. per year or per hour)
DSdata<-DSdata %>% 
  mutate(Estimated_Salary = str_replace_all(Estimated_Salary, "[^[:alnum:]]", " ")) %>%
  mutate(Conversion = case_when(Estimated_Salary %like% 'hour' ~ 1, TRUE ~ 0)) %>%
  mutate(Estimated_Salary = gsub("year","",Estimated_Salary)) %>%
  mutate(Estimated_Salary = gsub("hour","",Estimated_Salary)) %>%
  mutate(Estimated_Salary = gsub(" ","",Estimated_Salary)) %>%
  mutate(Estimated_Salary = as.numeric(Estimated_Salary)) %>%
  mutate(Estimated_Salary = case_when(Conversion == 1 ~ round((Estimated_Salary*40*52)/1000)*1000, TRUE ~ Estimated_Salary)) %>%
  select(-c(Conversion))


####################################
### Mining the description field ###
####################################

# Defining the text
text <- VCorpus(VectorSource(DSdata$Description))

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))

# Certain Special Characters
text <- tm_map(text, toSpace, "/") 
text <- tm_map(text, toSpace, "@") 
text <- tm_map(text, toSpace, "\\|")

# Non-English Characters and Shared Links
text <- tm_map(text, toSpace, "[^[:graph:]]")

# Convert the text to lower case
text <- tm_map(text, content_transformer(tolower))

# Remove numbers
text <- tm_map(text, removeNumbers)

# Remove common English stopwords
stops <- stopwords("english")

text <- tm_map(text, removeWords, stops)

# Remove oother stopwords as a character vector
text <- tm_map(text, removeWords, c("use","etc","also","like","one","will",
                                    "gender","identity","sexual","national","origin",
                                    "disability")) 

# Remove punctuations
text <- tm_map(text, removePunctuation)

# Eliminate extra white spaces
text <- tm_map(text, stripWhitespace)

# Converting to matrix, sorting, and converting to dataframe
dtm <- TermDocumentMatrix(text)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)

# Plotting the word cloud
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

## Two word ngram

desc<-data.frame(line = 1:nrow(DSdata), text = DSdata$Description)

desc <- desc %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 3) # n = 2 - pairs of consecutive words

desc <- desc %>%
  count(bigram, sort = TRUE)

# 165820 unique 3-word combinations

# Creating a new column for each word
desc<- desc %>% 
  separate(bigram, c("word1", "word2", "word3"), sep = " ")

# Filtering out stop words
bigrams_filtered <- desc %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)


bigrams_filtered %>%
  mutate(word_combination = paste0(word1,"-",word2,"-",word3)) %>%
  filter(!word_combination %in% c(
    "equal-opportunity-employer","race-color-religion","sexual-orientation-gender","orientation-gender-identity",
    "color-religion-sex","protected-veteran-status","sex-sexual-orientation","affirmative-action-employer",
    'national-origin-age',
    "equal-employment-opportunity","identity-national-origin","gender-identity-national","sex-national-origin",
    'national-origin-disability', "religion-sex-sexual","religion-sex-national","opportunity-affirmative-action","gender-gender-identity",
    "equal-opportunity-affirmative","color-religion-gender","sexual-orientation-national","orientation-national-origin",
    "status-sexual-orientation","national-origin-ancestry","religion-national-origin","color-national-origin",
    "gender-identity-gender","gender-sexual-orientation","expression-sexual-orientation","medical-dental-vision",
    "identity-sexual-orientation","gender-identity-sexual","religion-gender-gender","identity-gender-expression",
    "veteran-status-disability"
  )) %>%
  mutate(word_combination = reorder(word_combination,n)) %>%
  head(20) %>%
  ggplot(aes(x = word_combination, y = n))+
  geom_bar(stat="identity", color = "darkblue", fill = "lightblue")+
  xlab("Word Combination")+
  ylab("Frequency")+
  coord_flip()+
  theme_bw()


desc<-data.frame(line = 1:nrow(DSdata), text = DSdata$Description)

desc <- desc %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) # n = 2 - pairs of consecutive words

desc <- desc %>%
  count(bigram, sort = TRUE)

# Creating a new column for each word
desc<- desc %>% 
  separate(bigram, c("word1", "word2"), sep = " ")

# Filtering out stop words
bigrams_filtered <- desc %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_filtered %>%
  mutate(word_combination = paste0(word1,"-",word2)) %>%
  filter(!word_combination %in% c(
    "equal-opportunity", "sexual-orientation", "national-origin", "gender-identity","opportunity-employer","veteran-status",
    "race-color","color-religion","orientation-gender","religion-sex","receive-consideration"
  )) %>%
  mutate(word_combination = reorder(word_combination,n)) %>%
  head(20) %>%
  ggplot(aes(x = word_combination, y = n))+
  geom_bar(stat="identity", color = "darkblue", fill = "lightblue")+
  xlab("Word Combination")+
  ylab("Frequency")+
  coord_flip()+
  theme_bw()


# Skills and Education Features
DSdata<-DSdata %>%
  mutate(Description_Upper = toupper(str_replace_all(Description, "[^[:alnum:]]", " ")),
         Title_Upper = toupper(str_replace_all(Job_Title, "[^[:alnum:]]", " "))) %>%
  mutate(MS = case_when(Description_Upper %like% 'MASTERS' | Description_Upper %like% ' MS ' | substrRight(Description_Upper,2) == ' MS' ~ 1, TRUE ~ 0)) %>%
  mutate(PHD = case_when(Description_Upper %like% 'DOCTORATE' | Description_Upper %like% 'PHD' ~ 1, TRUE ~ 0)) %>%
  mutate(Statistics_Mathematics = case_when(Description_Upper %like% 'STATISTICS' | Description_Upper %like% 'MATHEMATICS' ~ 1, TRUE ~ 0)) %>%
  mutate(ComputerScience = case_when(Description_Upper %like% 'COMPUTER SCIENCE' | Description_Upper %like% ' CS ' ~ 1, TRUE ~ 0)) %>%
  mutate(Python = case_when(Description_Upper %like% 'PYTHON' ~ 1, TRUE ~ 0)) %>%
  mutate(R = case_when(Description_Upper %like% ' R ' | substrRight(Description_Upper,2) == ' R' ~ 1, TRUE ~ 0)) %>%
  mutate(Scala = case_when(Description_Upper %like% 'SCALA' ~ 1, TRUE ~ 0)) %>%
  mutate(SAS = case_when(Description_Upper %like% 'SAS' ~ 1, TRUE ~ 0)) %>%
  mutate(TensorFlow = case_when(Description_Upper %like% 'TENSORFLOW' ~ 1, TRUE ~ 0)) %>%
  mutate(Matlab = case_when(Description_Upper %like% 'MATLAB' ~ 1, TRUE ~ 0)) %>%
  mutate(MachineLearning = case_when(Description_Upper %like% 'MACHINE LEARNING' ~ 1, TRUE ~ 0)) %>%
  mutate(DeepLearning = case_when(Description_Upper %like% 'DEEP LEARNING' ~ 1, TRUE ~ 0)) %>%
  mutate(NeuralNetwork = case_when(Description_Upper %like% 'NEURAL NETWORK' ~ 1, TRUE ~ 0)) %>%
  mutate(StatisticalProgramming = case_when(Description_Upper %like% 'STATISTICAL PROGRAMMING' ~ 1, TRUE ~ 0)) %>%
  mutate(EntryLevel = case_when(Description_Upper %like% 'ENTRY LEVEL' | Title_Upper %like% 'ENTRY LEVEL' ~ 1, TRUE ~ 0)) %>%
  mutate(Sr_Title = case_when(Title_Upper %like% 'SR' | Title_Upper %like% 'SENIOR' ~ 1, TRUE ~ 0)) %>%
  mutate(Jr_Title = case_when(Title_Upper %like% 'JR' | Title_Upper %like% 'JUNIOR' ~ 1, TRUE ~ 0)) %>%
  mutate(Intern = case_when(Title_Upper %like% 'INTERN' ~ 1, TRUE ~ 0)) %>%
  mutate(Specialized_R_Python = case_when(Title_Upper %like% ' R ' | substrRight(Title_Upper,2) == ' R' | Title_Upper %like% 'PYTHON' ~ 1, TRUE ~ 0)) %>%
  mutate(Consultant = case_when(Title_Upper %like% 'CONSULTANT' ~ 1, TRUE ~ 0)) %>%
  mutate(Lead = case_when(Title_Upper %like% 'LEAD' ~ 1, TRUE ~ 0)) %>%
  mutate(Analyst = case_when(Title_Upper %like% 'ANALYST' ~ 1, TRUE ~ 0)) %>%
  mutate(Scientist = case_when(Title_Upper %like% 'SCIENTIST' ~ 1, TRUE ~ 0)) %>%
  mutate(ResearchDevelopment = case_when(Description_Upper %like% 'RESEARCH DEVELOPMENT' | 
                                           Description_Upper %like% ' RD ' | 
                                           Description_Upper %like% 'RESEARCH DEV' ~ 1, TRUE ~ 0),
         DataManagement = case_when(Description_Upper %like% 'DATA MANAGEMENT' | Description_Upper %like% 'DATA MGMT' ~ 1,
                                    TRUE ~ 0))



DSdata$Location_Code<-as.factor(DSdata$Location_Code)

data <- DSdata %>% 
  select(-c(URL, Description, Description_Upper, Job_Title)) %>% 
  inner_join(LocationDF, by = "Location_Code")

data$City <- as.factor(data$City)

data %>% 
  ggplot(aes(Company_Rating))+
  geom_histogram(color = "darkblue", fill = "lightblue", binwidth=0.1)+
  xlab("Company Rating") +
  theme_bw()

data %>%
  ggplot(aes(Company_Rating,Estimated_Salary))+
  geom_point(color = "lightgreen") +
  xlab("Company Rating")+
  scale_y_continuous(name="Estimated Salary", label = scales::dollar)+
  geom_smooth()+
  theme_bw()


########################
### Predictive Model ###
########################

# Setting up the formula for prediction
salaryFormula <- formula(Estimated_Salary ~ Company_Rating + MS + PHD + Python + R + Scala + SAS + TensorFlow + Matlab + MachineLearning + NeuralNetwork + StatisticalProgramming +
                           DeepLearning + EntryLevel + Sr_Title + Jr_Title + Intern + Consultant + Analyst + Scientist + Statistics_Mathematics +
                           ComputerScience + City + DataManagement)

# Setting aside the data without estimated salaries to predict at the end
forPrediction <- data %>% filter(is.na(Estimated_Salary))
data <- data %>% filter(!is.na(Estimated_Salary))

# Mean Salary Estimate
meanSal<-mean(data$Estimated_Salary)

# Distribution of salaries by city
data %>%
  mutate(City = reorder(City, Estimated_Salary)) %>%
  ggplot(aes(x = City, y = Estimated_Salary)) +
  geom_boxplot(color="darkgreen", fill = "lightgreen")+
  geom_hline(yintercept = meanSal, lty = 2, col = "black", size = 1)+
  ggtitle("Glassdoor\nData Scientist Salary Estimates by City")+
  scale_y_continuous(name = "Salary Estimate", label = scales::dollar)+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 65, hjust = 1))



# Re-level city variable as Denver (middle of the pack)
data$City <- relevel(data$City, ref="Denver")


######################
### Splitting Data ###
######################
library(caret)
set.seed(7)
test_index <- createDataPartition(data$Estimated_Salary, times = 1, p = 0.1, list = FALSE)
train <- data[-test_index,]
test <- data[test_index,]

nrow(train)
nrow(test)

# Distribution of Salary
train %>%
  ggplot(aes(Estimated_Salary))+
  geom_histogram(fill="pink", color="red")+
  scale_x_continuous(name = "Estimated Salary", label = dollar)


############################################
### Baseline Model and Linear Regression ###
############################################

results <- data.frame(Model = "Baseline (Mean)",
                      RMSE = RMSE(test$Estimated_Salary, mean(train$Estimated_Salary)))

# Linear Model
fit <- lm(salaryFormula, data = train)

results <- rbind(results,
                 data.frame(Model = "Linear Regression",
                            RMSE = RMSE(test$Estimated_Salary, predict(fit, newdata = test))))


###########################################################
### Experimenting with Machine Learning Models in caret ###
###########################################################
# Checking a few models for their un-tuned performance
seed <- 7

# 10-Fold Cross-Validation
control<- trainControl(method="cv", 
                       number=10)

########
# CART #
########
set.seed(seed)
fit.cart <- train(salaryFormula, 
                  data=train, 
                  method="rpart",
                  metric="RMSE", 
                  trControl=control)


#################
# Random Forest #
#################
set.seed(seed)
fit.rf <- train(salaryFormula, 
                data=train, 
                method="rf", 
                metric="RMSE",
                trControl=control)


###############################################################
# Stochastic Gradient Boosting (Generalized Boosted Modeling) #
###############################################################
set.seed(seed)
fit.gbm <- train(salaryFormula, 
                 data=train, 
                 method="gbm", 
                 metric="RMSE",
                 trControl=control,
                 verbose = FALSE)


#############################
# Bayesian Ridge Regression #
#############################
set.seed(seed)
fit.bridge <- train(salaryFormula, 
                 data=train, 
                 method="bridge", 
                 metric="RMSE",
                 trControl=control,
                 verbose = FALSE)

##########################
# Least Angle Regression #
##########################
set.seed(seed)
fit.lars <- train(salaryFormula, 
                    data=train, 
                    method="lars2", 
                    metric="RMSE",
                    trControl=control,
                    verbose = FALSE)

############################################
# Linear Regression with Forward Selection #
############################################
set.seed(seed)
fit.leap <- train(salaryFormula, 
                  data=train, 
                  method="leapForward", 
                  metric="RMSE",
                  trControl=control,
                  verbose = FALSE)

####################
# Ridge Regression #
####################
set.seed(seed)
fit.ridge <- train(salaryFormula, 
                  data=train, 
                  method="ridge", 
                  metric="RMSE",
                  trControl=control,
                  verbose = FALSE)

##################
# Neural Network #
##################
set.seed(seed)
fit.nn <- train(salaryFormula, 
                  data=train, 
                  method="neuralnet", 
                  metric="RMSE",
                  trControl=control,
                  verbose = FALSE)






# Comparisons
performance_grid <- resamples(list(
  cart=fit.cart, 
  rf=fit.rf, 
  gbm=fit.gbm,
  bridge = fit.bridge,
  lars = fit.lars,
  leap = fit.leap,
  ridge = fit.ridge,
  nn = fit.nn
))

performance_grid %>% knitr::kable()


results <- rbind(results, data.frame(
  Model = c("Decision Tree", "Random Forest", "Gradient Boosting"),
  RMSE = c(RMSE(test$Estimated_Salary,predict(fit.cart, newdata = test)), 
           RMSE(test$Estimated_Salary,predict(fit.rf, newdata = test)),
           RMSE(test$Estimated_Salary,predict(fit.gbm, newdata = test)))
))

results %>% knitr::kable()

# GBM has the best RMSE, but still worse than our linear regression model


# Optimizing the gbm model's hyperparameter choices
gbmGrid <-  expand.grid(interaction.depth = seq(2,9,1), 
                        n.trees = (1:30)*50, 
                        shrinkage = 0.05,
                        n.minobsinnode = 10)

set.seed(seed)
gbmFit <- train(salaryFormula, 
                data=train, 
                method="gbm", 
                metric="RMSE",
                trControl=control,
                verbose = FALSE,
                tuneGrid = gbmGrid)

plot(gbmFit)

results <- rbind(results, data.frame(
  Model = c("Optimized Gradient Boosting"),
  RMSE = RMSE(test$Estimated_Salary,predict(gbmFit, newdata = test)
  )))

# Seeing the final results
results %>% knitr::kable()

# Linear model still wins

#################################################
### Making Final Predictions on Hold-Out Data ###
#################################################

# Re-fitting on the full dataset
fit<-lm(salaryFormula, data = data)

# Removing instances where Tucson is in the data as there are no reported salaries for these postings to learn from
forPrediction <- forPrediction %>% filter(City != 'Tucson')

forPrediction$SalaryEstimate <- predict(fit, newdata = forPrediction)

forPrediction %>% 
  select(Company_Name, Company_Rating, City, Title_Upper, SalaryEstimate) %>%
  arrange(desc(SalaryEstimate)) %>% 
  head(5) %>%
  knitr::kable()

forPrediction %>% 
  select(Company_Name, Company_Rating, City, Title_Upper, SalaryEstimate) %>%
  arrange(SalaryEstimate) %>% 
  head(5) %>%
  knitr::kable()

###########################
### Additional Analysis ###
###########################

# Linear Model
fit <- lm(salaryFormula, data = data)

summary(fit)



df <- data %>% filter(!is.na(Estimated_Salary))

df$Pred <- predict(fit,newdata=df)

df %>% 
  mutate(MachineLearning = case_when(MachineLearning == 1 ~ "Yes", TRUE ~ "No")) %>%
  mutate(`Machine Learning` = as.factor(MachineLearning)) %>%
  ggplot(aes(Estimated_Salary, Pred, color = `Machine Learning`))+
  geom_point(size=2) +
  xlab("Glassdoor Estimated Salary")+
  ylab("Linear Model Estimate")+
  ggtitle("Is Machine Learning Mentioned in the Job Description?")+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))
