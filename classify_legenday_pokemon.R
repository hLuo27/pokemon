library(dplyr)

pokemon = read.csv('pokemon.csv', stringsAsFactors = FALSE)

# Features selected are base total, capture rate, and experience growth. Name, pokedex number, and legenadry status kept for identification purposes.

pokemon = pokemon %>% select(name, pokedex_number, base_total, capture_rate, experience_growth, is_legendary)
head(pokemon)

# Now we normalize each of the features
normalize_feature <- function(x){
  x = as.integer(x)
  (x-mean(x,na.rm = TRUE))/sd(x, na.rm = TRUE)
}

for (i in 3:5){
  pokemon[,i] = normalize_feature(pokemon[,i]) # Note: Minior has a string for capture rate so that value is coerced to NA
}

pokemon = pokemon %>% filter(!is.na(capture_rate)) # Remove 1 row with missing value (Minior)

n = nrow(pokemon) # total number of pokemon in pokedex

# We divide the data into an 80-20 split between training and testing data

set.seed(123)
training_indices = sort(sample(1:n, size = round(0.8*n), replace = FALSE))
testing_indices = numeric()
for (i in 1:n){
  if (!(i %in% training_indices)){
    testing_indices = c(testing_indices, i)
  }
}

training = pokemon[training_indices,]
testing = pokemon[testing_indices,] 

# Distance function to calculate distance between features of two pokemon

distance <- function(train_row = training[1,], test_row = testing[1,]){
  total = 0
  for (i in 3:5){
    total = total + (test_row[,i] - train_row[,i])^2
  }
  return(sqrt(total))
}

# Classify row (x) in testing set

classify <- function(x){
  closest = training %>% mutate('Distance' = distance(training, x)) %>% arrange(Distance) %>% slice(1:3) # Select 3 closest pokemon in training set
  names(which.max(table(closest$is_legendary)))
}

# Dataframe with predictions based on tables
predicted_class = numeric()
for(i in 1:nrow(testing)){
  predicted_class = c(predicted_class, classify(testing[i,]))
}

predicted_testing = testing %>% mutate('predicted_legendary' = predicted_class) 

sum(predicted_testing$is_legendary == predicted_testing$predicted_legendary)/nrow(predicted_testing)
