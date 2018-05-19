library(dplyr)

pokemon = read.csv('pokemon.csv')

# Features selected are base total, capture rate, and experience growth. Name, pokedex number, and legenadry status kept for identification purposes.

pokemon = pokemon %>% select(name, pokedex_number, base_total, capture_rate, experience_growth, is_legendary)
head(pokemon)

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