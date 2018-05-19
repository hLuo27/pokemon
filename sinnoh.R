library(stringr)
library(dplyr)

#' @title trainer
#' @description Constructor file that creates a trainer object
#' @param name Name of trainer(s)
#' @param hometown Hometown of trainer(s)
#' @param pokedex Number of pokemon seen in pokedex
#' @param badges Number of badges won
#' @return A 'trainer' object
trainer <- function(name = 'Diamond', hometown = 'Twinleaf Town', pokedex = 0, badges = 0){
  if (pokedex < 0 | pokedex > 151){
    stop('Error: There are 151 Pokemon in Sinnoh')
  }
  if (badges < 0 | badges > 8){
    stop('Error: There are 8 possible gym badges')
  }
  output = c(name, hometown, pokedex, badges)
  attr(output, "name") = name
  attr(output, "hometown") = hometown
  attr(output, "pokedex") = pokedex
  attr(output, "badges")= badges
  class(output) = "trainer"
  output
}

#' @title print.trainer
#' @description Function that prints a 'trainer' object
#' @param x A 'trainer' object
#' @return Print statement for the dataframe of trainer object with attributes
#' @export
print.trainer <- function(x) {
  cat('POKEMON TRAINERS IN SINNOH\n')
  trainer_as_df = data.frame('name' = attr(x,"name"), 'hometown' = attr(x, "hometown"), 
                   'number of pokemon seen' = attr(x, "pokedex"), 'number of badges' = attr(x, "badges"))
  names(trainer_as_df) = str_replace_all(names(trainer_as_df),pattern = "\\.", replacement = " ") # replace dots in dataframe headers with spaces
  trainer_as_df = trainer_as_df %>% arrange(name)
  print(trainer_as_df)
}

ash = trainer('Ash', hometown = 'Pallet Town', pokedex = 100, badges = 8)
ash
