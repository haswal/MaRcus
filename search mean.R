d <- rnorm(100, 4, 3)

guesses <- seq(range(d)[1], 
               range(d)[2],
               0.1)

dists <- function(guess){
  sqrt(sum((guess - d)^2))
}

my_stupid_mean <- function(vector){
  g1 <- seq(range(vector)[1], 
            range(vector)[2],
            round((range(vector)[2] - range(vector)[1])/100, 1))
  
  est1 <- map_dbl(g1, function(guess){
    sqrt(sum((guess - vector)^2))
  }) %>% 
    as_tibble() %>% 
    add_column(g1) %>% 
    arrange(value) %>% 
    slice(1) %>% 
    pull(g1)
  
  g2 <- seq(est1 - 0.1, 
            est1 + 0.1,
            0.000001)
  
  map_dbl(g2, function(guess){
    sqrt(sum((guess - vector)^2))
  }) %>% 
    as_tibble() %>% 
    add_column(g2) %>% 
    arrange(value) %>% 
    slice(1) %>% 
    pull(g2)
  
}

round((range(d)[2] - range(d)[1])/100, 1)

map_dbl(guesses, function(guess){
  sqrt(sum((guess - d)^2))
})

map(guesses, dists) %>% 
  unlist() %>% 
  as_tibble() %>% 
  add_column(guesses) %>% 
  arrange(value)

guesses_2 <- seq(3.95, 4.15, 0.000001)

map(guesses_2, dists) %>% 
  unlist() %>% 
  as_tibble() %>% 
  add_column(guesses_2) %>% 
  arrange(value)

guesses_3 <- seq(4.05, 4.07, 0.001)

options(pillar.sigfig = 7)

map(guesses_3, dists) %>% 
  unlist() %>% 
  as_tibble() %>% 
  add_column(guesses_3) %>% 
  arrange(value)

mean(d)
