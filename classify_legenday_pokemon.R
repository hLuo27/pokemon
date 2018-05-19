library(dplyr)

pokemon = read.csv('pokemon.csv')

# Features selected are base total, capture rate, and experience growth. Name and legenadry status kept for identification purposes.

pokemon = pokemon %>% select(name, base_total, capture_rate, experience_growth, is_legendary)
head(pokemon)

n = nrow(pokemon) # total number of pokemon in pokedex

# We divide the data into an 80-20 split between training and testing data

training_indicies = sample(1:n, size = round(0.8*n), replace = FALSE) # Indicies of pokemon to use for training set
test_indicies = 1:n - training_indicies