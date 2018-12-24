### Kaggle Project ###
### How much for your Airbnb? ###

# Read the data
data = read.csv(file = "analysisData.csv")
                  
# read in scoring data and apply model to generate predictions
scoringData = read.csv(file = "scoringData.csv")

library(dplyr)
library(tidyr)
library(ggplot2)

### Appendix 1 - Identifier Variables ###

## visually inspect for identifier variables

# invoke the Viewer in the RStudio console to view the data
View(data)
# returns the first 6 vectors from the data
head(data)

# example – id represents a unique classification for the record
# although I was able to calculate various metrics, these results were not meaningful 
summary(data$id)

# example – R output was truncated but showed 29,000 unique listings
summary(data$listing_url)



### Appendix 2 - No Unique Values ###

## deep dive into variables that have no unique values

summary(data$thumbnail_url)
# 29142 NA's

summary(data$host_acceptance_rate)
# 29142 N/A

summary(data$has_availability)
# 29142 True

summary(data$requires_license)
# 29142 False

summary(data$experiences_offered)
#29142 blanks fields

summary(data$jurisdiction_names)
# 29410 blank fields



### Appendix 3 - Lising Variables ###

# concatenate all relevant listing variables into one column (analysis data)
data$listing_data_consolidated <- paste(data$name,
                                   data$summary,
                                   data$space,
                                   data$description,
                                   data$neighborhood_overview,
                                   data$notes,
                                   data$transit,
                                   data$access,
                                   data$interaction,
                                   data$house_rules,
                                   data$amenities,
                                   sep = " ")
                                   
# concatenate all relevant listing variables into one column (scoring data)
scoringData$listing_data_consolidated <- paste(scoringData$name,
                                               scoringData$summary,
                                               scoringData$space,
                                               scoringData$description,
                                               scoringData$neighborhood_overview,
                                               scoringData$notes,
                                               scoringData$transit,
                                               scoringData$access,
                                               scoringData$interaction,
                                               scoringData$house_rules,
                                               scoringData$amenities,
                                               sep = " ")


# create vector for guest experience keywords that I want to experiment with
guest_experience_keywords <- c("luxury", 
                               "spacious",
                               "renovated",
                               "private")

# combine search terms in grep/grepl pattern with the pipe character | 
ge_combined <- paste(guest_experience_keywords, collapse = "|")

# search for guest_experience_keywords in both analysis and scoring data
# add in the argument to ignore the case
data$listing_data_analysis = grepl(pattern = ge_combined, x = data$listing_data_consolidated, ignore.case = TRUE)
scoringData$listing_data_analysis = grepl(pattern = ge_combined, x = scoringData$listing_data_consolidated, ignore.case = TRUE)

summary(data$listing_data_analysis)
# 69% of the analysis data had a keyword match

summary(scoringData$listing_data_analysis)
# 68% of the scoring data had a keyword match



### Appendix 4 - Location Variables ###

## check for duplicative / inconsistent entries for location

# example visualizes Staten Island comparing neighborhood group to city

library(dplyr)
library(ggplot2)

data %>% 
  filter(neighbourhood_group_cleansed == "Staten Island") %>%
  ggplot(mapping = aes(x=factor(city))) +
  geom_histogram(stat = 'count')
# based on results for Staten Island, neighborhood group and city are 
# highly correlated and hold duplicative / non-unique data


## check for location variables that are highly correlated

# example compares country code with country through visualization
ggplot(data = data, mapping = aes(x = country, fill = country_code)) +
  geom_bar()

# there’s a 100% match between country and country_code


## examine the structure of latitude and longitude
str(data$latitude)
str(data$longitude)
# with over 29,000 unique values, latitude and longitude will be too precise

# Location Analysis #

# create location keywords based on Airbnb research
location_keywords <- c("Union Square",
                       "Theater District",
                       "Soho",
                       "Brooklyn Heights",
                       "Williamsburg",
                       "Chinatown",
                       "Dumbo",
                       "Meatpacking",
                       "Little Italy",
                       "East Village",
                       "West Village",
                       "Park Slope",
                       "Chelsea",
                       "Lower East Side",
                       "Greenwich Village",
                       "Gramercy Park",
                       "Astoria",
                       "Battery Park City",
                       "Garment District",
                       "Financial District",
                       "Fort Greene",
                       "Nolita",
                       "Flatiron District",
                       "Bushwick",
                       "Sunset Park")

# combine search terms in grep/grepl pattern with the pipe character | 
location_combined <- paste(location_keywords, collapse = "|")

# search for location_keywords
# add in argument to make the search not case sensitive
data$location_analysis = grepl(pattern = location_combined, 
                               x = data$neighbourhood_cleansed,
                               ignore.case = TRUE)

scoringData$location_analysis = grepl(pattern = location_combined, 
                               x = scoringData$neighbourhood_cleansed,
                               ignore.case = TRUE)


### Appendix 6 - Property Variables ###

# check correlation between number of beds vs. bedrooms
cor(data$beds,data$bedrooms, use = "pairwise.complete.obs")
# eliminate beds as a predictor since there is a significant correlation between the two variables

# address missing values in square_feet
summary(data$square_feet)
# because more than 98% of the fields for square_feet are NA, I decided to eliminate this field as a potential predictor rather than artificially reducing my sample set or artificially inputting numbers for missing data points

summary(data$bedrooms)
# max bedrooms is 11 (analysis data)

summary(data$bathrooms)
# max bathrooms is 17 (analysis data)

# reduce all bedrooms and bathrooms greater than 4 to equal 4 (training set)
data$bedrooms_new = replace(data$bedrooms, data$bedrooms >= 4, 4)
data$bathrooms_new = replace(data$bathrooms, data$bathrooms >= 4, 4)


summary(scoringData$bedrooms)
# max bedrooms is 10 (scoring data)

summary(scoringData$bathrooms)
# max bathrooms is 7 (scoring data)

# reduce all bedrooms and bathrooms greater than 4 to equal 4 (scoring data)
scoringData$bedrooms_new = replace(scoringData$bedrooms, scoringData$bedrooms >= 4, 4)
scoringData$bathrooms_new = replace(scoringData$bathrooms, scoringData$bathrooms >= 4, 4)

data %>%
  count(data$bedrooms)

ggplot(data = data, mapping = aes(x = data$bedrooms)) +
  geom_bar()

# check the levels for property_type
levels(data$property_type)
levels(scoringData$property_type)
# scoring data includes new factors, Cottage and Hut, that are not included in the analysis data

# assess the frequency of Cottage and Hut in the scoring data
scoringData %>%
  filter(property_type == "Cottage") %>%
  count()

scoringData %>%
  filter(property_type == "Hut") %>%
  count()

# assign 'Cottage' and 'Hut' to 'Other' in the scoring data
data$property_type_new = data$property_type
scoringData$property_type_new = scoringData$property_type
scoringData$property_type_new[scoringData$property_type == "Hut"] <- "Other"
scoringData$property_type_new[scoringData$property_type == "Cottage"] <- "Other"



### Appendix 7 - Review Variables ###

# investigate the summary and structure of review variables
summary(data$review_scores_accuracy)
summary(data$review_scores_cleanliness)
summary(data$review_scores_checkin)
summary(data$review_scores_communication)
summary(data$review_scores_location)
summary(data$review_scores_value)
# integer numbers from 1 to 10

str(data$review_scores_accuracy)
str(data$review_scores_cleanliness)
str(data$review_scores_checkin)
str(data$review_scores_communication)
str(data$review_scores_location)
str(data$review_scores_value)

# review variables (except for rating) are integer numbers between 0 to 10 – no transformations required

# bucket review scores (analysis data)
summary(data$review_scores_rating)
str(data$review_scores_rating)
data$review_scores_rating_bucket = cut(data$review_scores_rating, 
                                       breaks = c(0,10,20,30,40,50,60,70,80,90,100),
                                       include.lowest = TRUE)
summary(data$review_scores_rating_bucket)

# identify the 1st and 3rd quartile to be the thresholds
summary (data$number_of_reviews)
# 1st quartile = 3
# 3rd quartile = 29

# bucket number of reviews (analysis data)
summary(data$number_of_reviews)
str(data$number_of_reviews)
data$number_of_reviews_bucket = cut(data$number_of_reviews, 
                                       breaks = c(0,3,29,600),
                                       include.lowest = TRUE)
summary(data$number_of_reviews_bucket)

# bucket review scores (scoring data)
scoringData$review_scores_rating_bucket = cut(scoringData$review_scores_rating, 
                                       breaks = c(0,10,20,30,40,50,60,70,80,90,100),
                                       include.lowest = TRUE)
summary(scoringData$review_scores_rating_bucket)

# bucket number of reviews (scoring data)
scoringData$number_of_reviews_bucket = cut(scoringData$number_of_reviews, 
                                    breaks = c(0,3,29,600),
                                    include.lowest = TRUE)
summary(scoringData$number_of_reviews_bucket)



### Appendix 8 - Financial Variables ###

# Analysis of Cleaning Fees #

# clean up the missing values first (analysis data)
cleaning_fee_missing_data <- which(is.na(data$cleaning_fee))
# assign missing values with default value of 0 (analysis data)
data$cleaning_fee[cleaning_fee_missing_data] <- 0

# apply logic - less than 150  = TRUE (analysis data)
data$acceptable_fee <- TRUE
data$acceptable_fee[data$cleaning_fee > 150] <- FALSE

# clean up the missing values first (scoring data)
cleaning_fee_missing_scoring <- which(is.na(scoringData$cleaning_fee))
# assign missing values with default value of 0 (scoring data)
scoringData$cleaning_fee[cleaning_fee_missing_scoring] <- 0

# apply logic - less than 150  = TRUE (scoring data)
scoringData$acceptable_fee <- TRUE
scoringData$acceptable_fee[scoringData$cleaning_fee > 150] <- FALSE


# Analysis of Security Deposit #

# apply logic (greater than 0 = TRUE (analysis data)
data$SD_requirement <- TRUE
data$SD_requirement[scoringData$security_deposit > 0] <- FALSE

# apply logic (greater than 0 = TRUE (scoring data)
scoringData$SD_requirement <- TRUE
scoringData$SD_requirement[scoringData$security_deposit > 0] <- FALSE


#######################################################################


### Appendix 9 - Feature Selection ###

# forward stepwise variable selection – start with an empty model
start_mod_forward = lm(price ~ 1, data = data)
empty_mod_forward = lm(price ~ 1, data = data)
full_mod = lm(price ~ listing_data_analysis +
                location_analysis +
                property_type_new +
                room_type +
                accommodates +
                bathrooms_new +
                bedrooms_new +
                bed_type +
                review_scores_rating_bucket +
                number_of_reviews_bucket +
                review_scores_accuracy +
                review_scores_cleanliness +
                review_scores_checkin +
                review_scores_communication +
                review_scores_location +
                review_scores_value +
                acceptable_fee +
                SD_requirement +
                host_is_superhost +
                require_guest_phone_verification +
                require_guest_profile_picture +
                host_identity_verified +
                host_has_profile_pic +
                is_business_travel_ready +
                instant_bookable +
                cancellation_policy,
              data = data)

forwardStepwise = step(start_mod_forward, 
                       scope = list(upper = full_mod,
                                    lower = empty_mod_forward,
                                    direction = 'forward'))

summary(forwardStepwise)

# make predictions
pred = predict(model, newdata = scoringData)

# construct submission from predictors
submissionFile = data.frame(id = scoringData$id, price = pred)

# write the submission file
write.csv(submissionFile, 'sample_submission_forward.csv',row.names = F)

# backward stepwise variable selection – start with a full model 
start_mod_backward = lm(price ~ listing_data_analysis +
                 location_analysis +
                 property_type_new +
                 room_type +
                 accommodates +
                 bathrooms_new +
                 bedrooms_new +
                 bed_type +
                 review_scores_rating_bucket +
                 number_of_reviews_bucket +
                 review_scores_accuracy +
                 review_scores_cleanliness +
                 review_scores_checkin +
                 review_scores_communication +
                 review_scores_location +
                 review_scores_value +
                 acceptable_fee +
                 SD_requirement +
                 host_is_superhost +
                 require_guest_phone_verification +
                 require_guest_profile_picture +
                 host_identity_verified +
                 host_has_profile_pic +
                 is_business_travel_ready +
                 instant_bookable +
                 cancellation_policy,
               data = data)
                 
empty_mod_backward = lm(price ~ 1, data = data)

backwardStepwise = step(start_mod_backward, 
                       scope = list(upper = full_mod,
                                    lower = empty_mod_backward,
                                    direction = 'backward'))

summary(backwardStepwise)

# make predictions
pred = predict(model, newdata = scoringData)

# construct submission from predictors
submissionFile = data.frame(id = scoringData$id, price = pred)

# write the submission file
write.csv(submissionFile, 'sample_submission_backward.csv',row.names = F)

# hybrid stepwise variable selection – start with empty model
start_mod_stepwise = lm(price ~ 1, data = data)
empty_mod_stepwise = lm(price ~ 1, data = data)

hybridStepwise = step(start_mod_stepwise,
                      scope = list(upper = full_mod,
                                   lower = empty_mod_stepwise,
                                   direction = 'both'))

summary(hybridStepwise)

# make predictions
pred = predict(model, newdata = scoringData)

# construct submission from predictors
submissionFile = data.frame(id = scoringData$id, price = pred)

# write the submission file
write.csv(submissionFile, 'sample_submission_hybrid.csv',row.names = F)


# Shrinkage Method - Lasso with 10-fold cross validation

library(glmnet)
x = model.matrix(price ~ listing_data_analysis +
                   location_analysis +
                   property_type_new +
                   room_type +
                   accommodates +
                   bathrooms_new +
                   bedrooms_new +
                   bed_type +
                   review_scores_rating_bucket +
                   number_of_reviews_bucket +
                   review_scores_accuracy +
                   review_scores_cleanliness +
                   review_scores_checkin +
                   review_scores_communication +
                   review_scores_location +
                   review_scores_value +
                   acceptable_fee +
                   SD_requirement +
                   host_is_superhost +
                   require_guest_phone_verification +
                   require_guest_profile_picture +
                   host_identity_verified +
                   host_has_profile_pic +
                   is_business_travel_ready +
                   instant_bookable +
                   cancellation_policy +
                   square_feet_new,
                 data = data)

y = data$price
# alpha = 1 performs lasso
lassoModel = glmnet(x,y,alpha = 1)
lassoModel

# use 10-fold cross validation
cv.lasso = cv.glmnet(x,y,alpha = 1)

coef(cv.lasso)

# resulting model
model = lm(price ~ location_analysis +
                          property_type_new +
                          room_type +
                          accommodates +
                          bathrooms_new +
                          bedrooms_new +
                          review_scores_rating_bucket +
                          review_scores_cleanliness +
                          review_scores_location +
                          review_scores_value +
                          acceptable_fee +
                          instant_bookable +
                          cancellation_policy,
                        data = data)

# make predictions
pred = predict(model, newdata = scoringData)

# construct submission from predictors
submissionFile = data.frame(id = scoringData$id, price = pred)

# write the submission file
write.csv(submissionFile, 'sample_submission_lasso.csv',row.names = F)



### Appendix 10 - Regression Tree ###

# simple regression tree

library(rpart)
library(rpart.plot)

regression_tree = rpart(price ~ location_analysis +
                          property_type_new +
                          room_type +
                          accommodates +
                          bathrooms_new +
                          bedrooms_new +
                          review_scores_rating_bucket +
                          review_scores_cleanliness +
                          review_scores_location +
                          review_scores_value +
                          acceptable_fee +
                          instant_bookable +
                          cancellation_policy,
                        data = data)

rpart.plot(regression_tree)

pred = predict(regression_tree, newdata = scoringData)

submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'sample_submission_tree.csv',row.names = F)


# advanced regression tree
# use 10-fold cross validation to tune tree complexity

library(caret)

# 10-fold cross validation
trControl = trainControl(method = 'cv', number = 10)

tuneGrid = expand.grid(.cp = seq(0,0.1,0.001))

cv.data = train(price ~ location_analysis +
                    property_type_new +
                    room_type +
                    accommodates +
                    bathrooms_new +
                    bedrooms_new +
                    review_scores_rating_bucket +
                    review_scores_cleanliness +
                    review_scores_location +
                    review_scores_value +
                    acceptable_fee +
                    instant_bookable +
                    cancellation_policy,
                  data = data,
                  method = 'rpart',
                  trControl = trControl,
                  tuneGrid = tuneGrid)

# identify the cp that yielded the lowest cross-validation error
cv.data$bestTune
# best cp is .001

# apply tree model with optimal complexity value to scoringData
treeCV = rpart(price ~ location_analysis +
                 property_type_new +
                 room_type +
                 accommodates +
                 bathrooms_new +
                 bedrooms_new +
                 review_scores_rating_bucket +
                 review_scores_cleanliness +
                 review_scores_location +
                 review_scores_value +
                 acceptable_fee +
                 instant_bookable +
                 cancellation_policy,
               data = data,
               control = rpart.control(cp=cv.data$bestTune))

rpart.plot(treeCV)

predCV = predict(treeCV, newdata = scoringData)

submissionFile = data.frame(id = scoringData$id, price = predCV)
write.csv(submissionFile, 'sample_submission_treeCV.csv',row.names = F)


### Appendix 11 - Abandoned Methods ###

# best subset selection
library(leaps)
subsets = regsubsets(price ~ listing_data_analysis +
                       location_analysis +
                       property_type +
                       room_type +
                       accommodates +
                       bathrooms_new +
                       bedrooms_new +
                       bed_type +
                       review_scores_rating_bucket +
                       number_of_reviews_bucket +
                       review_scores_accuracy +
                       review_scores_cleanliness +
                       review_scores_checkin +
                       review_scores_communication +
                       review_scores_location +
                       review_scores_value +
                       acceptable_fee +
                       SD_requirement +
                       host_is_superhost +
                       require_guest_phone_verification +
                       require_guest_profile_picture +
                       host_identity_verified +
                       host_has_profile_pic +
                       is_business_travel_ready +
                       instant_bookable +
                       cancellation_policy,
                     data = data,
                     nvmax = 26,
                     really.big = T)
# after over a few hours, this exhaustive computation crashed my R terminal so I decided to abandon this method


# principal component analysis

# extract the predictors to be reduced
variables <- c("listing_data_analysis",
               "location_analysis",
               "property_type_new",
               "room_type",
               "accommodates",
               "bathrooms_new",
               "bedrooms_new", 
               "bed_type",
               "review_scores_rating_bucket",
               "number_of_reviews_bucket",
               "review_scores_accuracy",
               "review_scores_cleanliness", 
               "review_scores_checkin", 
               "review_scores_communication",
               "review_scores_location",
               "review_scores_value",
               "acceptable_fee",
               "SD_requirement",
               "host_is_superhost",
               "require_guest_phone_verification",
               "require_guest_profile_picture",
               "host_identity_verified",
               "host_has_profile_pic",
               "is_business_travel_ready",
               "instant_bookable",
               "cancellation_policy")

variables_combined <- paste(variables, collapse = "|")

# find the relevant column numbers by using grep
grep(variables_combined, colnames(data))
grep(variables_combined, colnames(scoringData))

dataPredictors = data[,c(29,36,37,53,54,58,81,82,83,84,85,86,90,91,92,93,94,98,99,100,101,102,103,104,105,106)]
scoringDataPredictors = scoringData[,c(29,36,37,53,54,58,80,81,82,83,84,85,89,90,91,92,93,97,98,99,100,101,102,103,104,105)]

# Now, rather than selecting individual variables,
# we will capture the essence in a few components so as to retain at least
# 90% of the information. 

library(caret)
x = preProcess(x = dataPredictors, method = 'pca', thresh = 0.7)
dataComponents = predict(x, newdata = dataPredictors)
dataComponents$price = data$price

head(dataComponents)

model = lm(price~., data = dataComponents)
scoringDataComponents = predict(x,newdata=scoringDataPredictors)

submissionFile = data.frame(id = scoringData$id, price = scoringDataComponents)

write.csv(submissionFile, 'sample_submission_pca.csv',row.names = F)
# testing data did not include price – output file submission failed

# Random Forest

library(randomForest)

forest = randomForest(price ~ location_analysis +
                        property_type_new +
                        room_type +
                        accommodates +
                        bathrooms_new +
                        bedrooms_new +
                        review_scores_rating_bucket +
                        review_scores_cleanliness +
                        review_scores_location +
                        review_scores_value +
                        acceptable_fee +
                        instant_bookable +
                        cancellation_policy,
                      data = data,
                      ntree = 1000)

predForest = predict(forest, newdata = scoringData)
# prediction failed as forest created new factors

submissionFile = data.frame(id = scoringData$id, price = predForest)
write.csv(submissionFile, 'sample_submission_treeForest.csv',row.names = F)

# Random Forest Model with Cross Validation

trControl = trainControl(method = 'cv', number = 10)

tuneGrid = expand.grid(mtry=1:5)

cvForest = train(price ~ location_analysis +
                   property_type_new +
                   room_type +
                   accommodates +
                   bathrooms_new +
                   bedrooms_new +
                   review_scores_rating_bucket +
                   review_scores_cleanliness +
                   review_scores_location +
                   review_scores_value +
                   acceptable_fee +
                   instant_bookable +
                   cancellation_policy,
                 data = data,
                 method = 'rf',
                 ntree = 1000,
                 trControl = trControl,
                 tuneGrid = tuneGrid)

cvForest
# best mtry was 5

randforest = randomForest(price ~ location_analysis +
                            property_type_new +
                            room_type +
                            accommodates +
                            bathrooms_new +
                            bedrooms_new +
                            review_scores_rating_bucket +
                            review_scores_cleanliness +
                            review_scores_location +
                            review_scores_value +
                            acceptable_fee +
                            instant_bookable +
                            cancellation_policy,
                          data = data,
                          ntree = 100,
                          mtry = 5)

predForest = predict(randforest, newdata = scoringData)

submissionFile = data.frame(id = scoringData$id, price = pred)
write.csv(submissionFile, 'sample_submission_treeForestCV.csv',row.names = F)
