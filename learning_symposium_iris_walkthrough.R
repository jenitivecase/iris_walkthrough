# standard options #############################################################
source("https://raw.githubusercontent.com/jenitivecase/Settings/master/options.R")

## load packages ###############################################################
needed_packages <- c("tidyverse", "ggplot2", "class", "rpart", "rpart.plot")
sapply(needed_packages, load_packages)

################################################################################
## Some incredibly basic Exploratory Data Analysis (EDA) #######################
################################################################################
#look at the dataframe
View(iris)

#basic stats
summary(iris)

#histograms of each of the numeric variables
ggplot(iris) +
  geom_histogram(aes(x = Sepal.Length))

ggplot(iris) +
  geom_histogram(aes(x = Sepal.Width))

ggplot(iris) +
  geom_histogram(aes(x = Petal.Length))

ggplot(iris) +
  geom_histogram(aes(x = Petal.Width))


#correlation matrix
iris %>%
  select(-Species) %>%
  cor()

################################################################################
## Visualizing correlations + some simple regressions ##########################
################################################################################

#an example of a weak correlation - variables are only loosely related 
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "Weak negative correlation", 
       subtitle = "r = -0.12")

#an example of a strong correlation - variables are closely related
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(title = "Strong positive correlation", 
       subtitle = "r = 0.96")

#a simple linear regression
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(color = Species)) + 
  geom_smooth(method = "lm") 

#note that the coefficient is NOT a significant predictor
model = lm(Sepal.Width ~ Sepal.Length, data = iris)
summary(model)

#an illustration of group effects
ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width)) + 
  geom_point(aes(color = Species)) + 
  geom_smooth(aes(group = Species), method = "lm")

#now the coefficients are significant predictors because we're portioned the variance
model = lm(Sepal.Width ~ Sepal.Length + Species, data = iris)
summary(model)

#a regression that works right out of the gate
ggplot(iris, aes(x = Petal.Length, y = Petal.Width)) + 
  geom_point(aes(color = Species)) + 
  geom_smooth(method = "lm")

model = lm(Petal.Width ~ Petal.Length, data = iris)
summary(model)

################################################################################
## Predicting species (categorical) from the other variables ###################
################################################################################

### knn model 
#-------------------------------------------------------------------------------


#add row numbers
iris <- iris %>%
  mutate(id = row_number())

#split into train and test data - necessary to check your model's accuracy
train <- iris %>% 
  sample_frac(0.70) 

test  <- iris %>%
  anti_join(train, by = "id") %>%
  select(-id)

train <- train %>%
  select(-id)

head(train)
head(test)

#get predictions from the KNN algorithm. 
#let's try with only one of the variables first
predictions <- knn(train = select(train, Sepal.Width), 
                   test = select(test, Sepal.Width), 
                   cl = train$Species, k = 1, prob = TRUE)

#accuracy is really bad, but we're not using most of the available data.
sum(predictions == test$Species)/nrow(test)

#confusion matrix
table(predictions, test$Species)


#let's try with all the predictor variables. additional info improves our predictions
predictions <- knn(train = select(train, -Species), 
                   test = select(test, -Species), 
                   cl = train$Species, k = 1)

#now accuracy is great. this is a tiny dataset with highly explanatory variables, so this is expected!
sum(predictions == test$Species)/nrow(test)

#confusion matrix
table(predictions, test$Species)


### decision tree model 
#-------------------------------------------------------------------------------

#same - let's check out a single variable first
dtree <- rpart(Species ~ Sepal.Width, data = train)

#visualize the tree
rpart.plot(dtree)

#get predictions
predictions <- predict(dtree, select(test, Sepal.Width), type = "class")

#accuracy
sum(predictions == test$Species)/nrow(test)

#confusion matrix
table(predictions, test$Species)

#okay, now let's do it with all possible predictors
dtree <- rpart(Species ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width,
               data = train)
rpart.plot(dtree)

predictions <- predict(dtree, select(test, -Species), type = "class")

#accuracy
sum(predictions == test$Species)/nrow(test)

#confusion matrix
table(predictions, test$Species)



# (with all the predictor variables, obviously!!)
