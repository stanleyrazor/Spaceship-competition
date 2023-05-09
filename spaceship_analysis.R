

# dir ---------------------------------------------------------------------

setwd("/home/nc-workforce/Documents/Kaggle/Spaceship competition")

# libs --------------------------------------------------------------------

library(pacman)
p_load(dplyr, ggplot2, lubridate, stringr, caret, pROC, recipes, ModelMetrics, 
       gridExtra, naniar, glmnet, corrplot, e1071, recipes, ggmosaic)


# data --------------------------------------------------------------------

train <- read.csv('Data/train.csv')
test <- read.csv('Data/test.csv')


# glimpse and summary -----------------------------------------------------

View(train)
dim(train)
glimpse(train)
summary(train)

#missing values
miss_var_summary(train)
gg_miss_var(train)
gg_miss_upset(train)

# Feature construction ----------------------------------------------------

# splitting passenger ID
odd_id <- seq(from = 1, to = nrow(train)*2, by = 2)
id_loc <- function(vec, id) vec[id] # function for vector location '[.]'

train <- train |>
  mutate(Cabin = ifelse(test = Cabin == '', yes = '//', no = Cabin)) |>
  mutate(Group = (str_split(PassengerId, '_') |> unlist())[odd_id],
         PID = (str_split(PassengerId, '_') |> unlist())[-odd_id],
         CabinDeck = lapply((str_split(Cabin, '/')), id_loc, 1) |> unlist(),
         CabinNum = lapply((str_split(Cabin, '/')), id_loc, 2) |> unlist(),
         CabinSide = lapply((str_split(Cabin, '/')), id_loc, 3) |> unlist()) |>
  select(PID, Group, HomePlanet, CryoSleep, CabinDeck, CabinNum, CabinSide,
         Destination, Age, VIP, RoomService,
         FoodCourt, ShoppingMall, Spa, VRDeck, Transported)


# mean-value imputation based on Analysis of Variance:
# i select the top two explanatory variables
# imputting VRDeck
aov(VRDeck ~ . - Group - CabinNum - Transported, data = na.omit(train)) |>
  stats::step(direction = 'backward') |>
  summary()
newVR <- train |>
  aggregate(VRDeck ~ HomePlanet + CryoSleep, mean, na.omit = T) |>
  setNames(c('HomePlanet', 'CryoSleep', 'newVR'))

# imputing room service
aov(RoomService ~ . - Group - CabinNum - Transported, data = na.omit(train)) |>
  stats::step(direction = 'backward') |>
  summary()
newRoomService <- train |>
  aggregate(RoomService ~ HomePlanet + CryoSleep, mean, na.omit = T) |>
  setNames(c('HomePlanet', 'CryoSleep', 'newRoomService'))

# imputting food court
aov(FoodCourt ~ . - Group - CabinNum - Transported, data = na.omit(train)) |>
  stats::step(direction = 'backward') |>
  summary()
newFoodCourt <- train |>
  aggregate(FoodCourt ~ HomePlanet + CryoSleep, mean, na.omit = T) |>
  setNames(c('HomePlanet', 'CryoSleep', 'newFoodCourt'))

# imputting shopping mall
aov(ShoppingMall ~ . - Group - CabinNum - Transported, data = na.omit(train)) |>
  stats::step(direction = 'backward') |>
  summary()
newShoppingMall <- train |>
  aggregate(ShoppingMall ~ HomePlanet + CryoSleep, mean, na.omit = T) |>
  setNames(c('HomePlanet', 'CryoSleep', 'newShoppingMall'))

# imputting spa
aov(Spa ~ . - Group - CabinNum - Transported, data = na.omit(train)) |>
  stats::step(direction = 'backward') |>
  summary()
newSpa <- train |>
  aggregate(Spa ~ HomePlanet + CryoSleep, mean, na.omit = T) |>
  setNames(c('HomePlanet', 'CryoSleep', 'newSpa'))

# Age estimatred using deck and cabin side
aov(Age ~ . - Group - CabinNum - Transported, data = na.omit(train)) |>
  stats::step(direction = 'backward') |>
  summary()
newAge <- train |>
  aggregate(Age ~ PID + HomePlanet, mean, na.omit = T) |>
  setNames(c('PID', 'HomePlanet', 'newAge'))

train <- train |>
  merge(newVR,
        by = c('HomePlanet', 'CryoSleep'),
        all.x = T) |>
  merge(newRoomService,
        by = c('HomePlanet', 'CryoSleep'),
        all.x = T) |>
  merge(newFoodCourt,
        by = c('HomePlanet', 'CryoSleep'),
        all.x = T) |>
  merge(newShoppingMall,
        by = c('HomePlanet', 'CryoSleep'),
        all.x = T)|>
  merge(newSpa,
        by = c('HomePlanet', 'CryoSleep'),
        all.x = T) |>
  merge(newAge,
        by = c('PID', 'HomePlanet'),
        all.x = T)

train <- train |>
  mutate(
    RoomService = ifelse(is.na(RoomService), newRoomService, RoomService),
    FoodCourt = ifelse(is.na(FoodCourt), newFoodCourt, FoodCourt),
    ShoppingMall = ifelse(is.na(ShoppingMall), newShoppingMall, ShoppingMall),
    Spa = ifelse(is.na(Spa), newSpa, Spa),
    VRDeck = ifelse(is.na(VRDeck), newVR, VRDeck),
    Age = ifelse(is.na(Age), newAge, Age)
  ) |>
  select(c("PID", "VIP", "HomePlanet", "CryoSleep", "CabinDeck", 
           "CabinSide", "Destination", "Age", "RoomService", 
           "FoodCourt", "ShoppingMall", "Spa", "VRDeck", "Transported"))

# fill '' wih Unclassfied
rep_unclassified <- function(vec) ifelse(vec == '', 'Unclassified', vec)

train <- train |>
  mutate(
    across(
      .cols = where(is.character),
      .fns = rep_unclassified
    )
  ) |>
  mutate(
    across(
      .cols = where(is.character),
      .fns = as.factor
    )
  )


# summary and structure
glimpse(train)
colnames(train)

# Visualization -----------------------------------------------------------

theme_set(theme_minimal())

# PID
train |>
  ggplot()+
  geom_bar(aes(x = PID)) +
  labs(title = 'Personal Identification Number in Groups')

# VIP
train |>
  ggplot()+
  geom_bar(aes(x = VIP)) +
  labs(title = 'VIP Paid')

# Homeplanet
train |>
  ggplot()+
  geom_bar(aes(x = HomePlanet)) +
  labs(title = 'Home Planet')

train |> 
  ggplot()+
  geom_mosaic(aes(x = product(VIP, HomePlanet), fill = VIP))
# Conclusion: No one from earth paid VIP, most VIP was paid by people from Europa
#             Europa is most likely a rich nation

train |>
  ggplot()+
  geom_bar(aes(x = HomePlanet)) +
  labs(title = 'Home Planet and VIP stations')+
  facet_wrap(.~VIP)+
  theme_linedraw()

# CryoSleep
train |>
  ggplot()+
  geom_bar(aes(x = CryoSleep)) +
  labs(title = 'Home Planet')

grid.arrange(
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(PID, CryoSleep), fill = CryoSleep),
                show.legend = F),
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(HomePlanet, CryoSleep), fill = CryoSleep)),
  nrow = 1
)
# PIDs = 1, are the ones who have been put to cryosleep majority.

# Cabin status
grid.arrange(
  train |>
    ggplot()+
    geom_bar(aes(x = CabinDeck))+
    labs(title = 'Cabin Deck')+
    coord_flip(),
  train |>
    ggplot()+
    geom_bar(aes(x = CabinSide))+
    labs(title = 'Cabin Side'),
  nrow = 1
)

# Relationship  between cabin side and VIP
grid.arrange(
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(CabinSide, CabinDeck), fill = CabinSide),
                show.legend = F),
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(CabinSide, VIP), fill = CabinSide)),
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(CabinSide, PID), fill = CabinSide),
                show.legend = F),
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(CabinSide, CryoSleep), fill = CabinSide)),
  nrow = 2
)
# Conclusion: balanced relationship

# Destination
train |>
  ggplot()+
  geom_bar(aes(x = Destination))+
  labs(title = 'Destination')

grid.arrange(
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(Destination, HomePlanet), fill = Destination),
                show.legend = F),
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(Destination, CryoSleep), fill = Destination)),
  nrow = 1
)
# Conclusion: Majority people are going to TRAPPIST

# Age
train |>
  ggplot()+
  geom_histogram(aes(x = Age), col = 'white', bins = 60)+
  labs(title = 'Age distribution')

train |>
  group_by(PID) |> # Change the GroupBy variable
  summarise(Count = length(Age),
            minAge = min(Age),
            midAge = median(Age),
            maxAge = max(Age))

# The bills
grid.arrange(
  train |>
    ggplot()+
    geom_histogram(aes(x = RoomService), col = 'white', bins = 30)+
    labs(title = 'Room service', x = ''),
  train |>
    ggplot()+
    geom_histogram(aes(x = FoodCourt), col = 'white', bins = 30)+
    labs(title = 'Food Court', x = ''),
  train |>
    ggplot()+
    geom_histogram(aes(x = ShoppingMall), col = 'white', bins = 30)+
    labs(title = 'Shopping Mall', x = ''),
  train |>
    ggplot()+
    geom_histogram(aes(x = Spa), col = 'white', bins = 30)+
    labs(title = 'Spa', x = ''),
  train |>
    ggplot()+
    geom_histogram(aes(x = VRDeck), col = 'white', bins = 30)+
    labs(title = 'VR Deck', x = ''),
  nrow = 3
)

# correlation plot
corrplot(
  cor(train[, 8:13]),
  method = 'square',
  type = 'full',
  order = 'hclust'
)

# constructing plots for the response variable
grid.arrange(
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(Transported, PID), fill = Transported),
                show.legend = F),
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(Transported, VIP), fill = Transported),
                show.legend = F)+
    labs(y = ''),
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(Transported, HomePlanet), fill = Transported),
                show.legend = F)+
    labs(y = ''),
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(Transported, CryoSleep), fill = Transported),
                show.legend = F),
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(Transported, CabinDeck), fill = Transported),
                show.legend = F)+
    labs(y = ''),
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(Transported, CabinSide), fill = Transported),
                show.legend = F)+
    labs(y = ''),
  train |> 
    ggplot()+
    geom_mosaic(aes(x = product(Destination, Transported), fill = Transported)),
  nrow = 3
)

grid.arrange(
  train |>
    ggplot()+
    geom_boxplot(aes(x = Transported, y = Age)),
  train |>
    ggplot()+
    geom_boxplot(aes(x = Transported, y = RoomService)),
  train |>
    ggplot()+
    geom_boxplot(aes(x = Transported, y = FoodCourt)),
  train |>
    ggplot()+
    geom_boxplot(aes(x = Transported, y = ShoppingMall)),
  train |>
    ggplot()+
    geom_boxplot(aes(x = Transported, y = Spa)),
  train |>
    ggplot()+
    geom_boxplot(aes(x = Transported, y = VRDeck)),
  nrow = 3
)


# Model fitting -----------------------------------------------------------

# Replicating the feature engineering on test data
source('transform_test_function.R')
test <- transform_data(test)


# Logistic regression model -----------------------------------------------

m1 <- glm(Transported ~ .,
          data = train,
          family = 'binomial')
summary(m1)

# stepwise selection
s.m1 <- stats::step(m1, direction = 'backward')
summary(s.m1)

# using the ROC functionality
roc_stats <- roc(train$Transported, 
                 predict(s.m1, type = 'response')) %>%
  coords(ret = "all", transpose = FALSE)
View(roc_stats) # best thresh (youden): 0.4538147

roc(train$Transported, 
    predict(s.m1, type = 'response'),
    percent=T) |>
  ggroc()+
  labs(title = 'Logistic regression')+
  theme_classic()

# using CARET functionality to compare several models at once
c_glm <- caret::train(
  Transported ~ .,
  method = 'glm',
  family = 'binomial',
  # TuneLength = 5,
  trControl = trainControl(method = 'boot'),
  data = train
)
c_glm #  Acc: 0.7905284, Kappa: 0.5808556
c_glm$results


# Linear discriminant analysis --------------------------------------------

lmd <- MASS::lda(Transported ~ ., data = train)
lda_pred <- predict(lmd, test)$class

sub_s4.m1 <- data.frame(
  PassengerId = str_c(test$Group, '_', test$PID),
  Transported = lda_pred
)
head(sub_s4.m1)
write.csv(sub_s4.m1, 'Data/sub_s4.m1.csv', row.names = F)


# K nearest Neighbour models ----------------------------------------------

c_knn <- caret::train(
  Transported ~ .,
  method = 'knn',
  trControl = trainControl(method = 'cv'),
  tuneLength = 10,
  data = train
)
c_knn; plot(c_knn) # k = 17:  Acc: 0.7886809, Kappa: 0.5771691

# Fitting the model to the test data
xtrain <- model.matrix(Transported ~ ., data = train)
xtest$Transported <- rep(0, nrow(test))
xtest <- xtest[, colnames(train)]
xtest <- model.matrix(Transported ~ ., data = xtest)

knn_model <- knn3Train(train = xtrain,
                       test = xtest,
                       cl = train[, ncol(train)],
                       k = 17,
                       prob = F)

sub_s3.m1 <- data.frame(
  PassengerId = str_c(test$Group, '_', test$PID),
  Transported = knn_model
)
head(sub_s3.m1)
write.csv(sub_s3.m1, 'Data/sub_s3.m1.csv', row.names = F)



# Support vector machine --------------------------------------------------

c_svm_radial <- caret::train(
  Transported ~ .,
  method = 'svmRadial',
  trControl = trainControl(method = 'cv'),
  #tuneLength = 10,
  tuneGrid = expand.grid(C = seq(from = 1.666667, to = 2.666667, length = 10),
                         sigma = 0.03313168),
  data = train
)
c_svm_radial; plot(c_svm_radial)
# sigma = 0.03313168 and C = 2.333333.
# C         Accuracy   Kappa    
# 1.000000  0.7870704  0.5742088
# 1.333333  0.7883368  0.5766931
# 1.666667  0.7896021  0.5792246
# 2.000000  0.7900634  0.5801374
# 2.333333  0.7904087  0.5808231
# 2.666667  0.7889131  0.5778291
# 3.000000  0.7883380  0.5766725
# 3.333333  0.7862669  0.5725305
# 3.666667  0.7863822  0.5727537
# 4.000000  0.7855770  0.5711372

# Fitting the best fitted model
svm.m1 <- kernlab::ksvm(Transported ~ ., data = train,
                        kernel = 'rbfdot', kpar = list(sigma = 0.03313168),
                        C = 2.333333)
svm1.pred <- kernlab::predict(svm.m1, test)

sub_s.m6 <- data.frame(
  PassengerId = str_c(test$Group, '_', test$PID),
  Transported = svm1.pred
)
head(sub_s.m6)
write.csv(sub_s.m6, 'Data/sub_s6.m1.csv', row.names = F)

# Polynomial kernel
c_svm_poly <- caret::train(
  Transported ~ .,
  method = 'svmPoly',
  trControl = trainControl(method = 'cv'),
  tuneLength = 5,
  data = train
)
c_svm_poly; plot(c_svm_poly)




# Random forests ----------------------------------------------------------

c_rnf <- caret::train(
  Transported ~ .,
  method = 'rf',
  data = train,
  trControl = trainControl(method = 'cv'),
  tuneGrid = expand.grid(mtry = 4:14)
)
c_rnf; plot(c_rnf)
rnf_final <- c_rnf$finalModel
barplot(t(varImp(rnf_final)))

md_test <- model.matrix(Group ~ ., data = test)
rnf_pred <- predict(rnf_final, md_test)

sub_s8.m1 <- data.frame(
  PassengerId = str_c(test$Group, '_', test$PID),
  Transported = rnf_pred
)
head(sub_s8.m1)
write.csv(sub_s8.m1, 'Data/sub_s8.m1.csv', row.names = F)

# Using the models to predict ---------------------------------------------

test <- transform_data(test)
prob_s.m1 <- predict(s.m1, test, type = 'response')
pred_s.m1 <- ifelse(prob_s.m1 > 0.4538147, 'True', 'False') |> 
  factor(levels = c('False', 'True'))

sub_s.m1 <- data.frame(
  PassengerId = str_c(test$Group, '_', test$PID),
  Transported = pred_s.m1
)
head(sub_s.m1)
write.csv(sub_s.m1, 'Data/sub_s2.m1.csv', row.names = F)
