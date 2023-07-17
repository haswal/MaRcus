library(correlation)

eg1 <- data_sim("eg1", n = 200, seed = 1)

m <- gam(y ~ s(x0) + s(x1) + s(x2) + s(x3),
         data = eg1, method = "REML")


sm <- smooth_estimates(m)

ar1_cor <- function(n, rho) {
  exponent <- abs(matrix(1:n - 1, nrow = n, ncol = n, byrow = TRUE) - 
                    (1:n - 1))
  rho^exponent
}

mus <- sm %>%
  filter(smooth == "s(x2)") %>% 
  select(x2, est) %>% 
  mutate(est2 = est - min(est) -0.5,
         est2 = ifelse(est2 < 0 , 0, est2)) %>% 
  slice(1:90)

sds <- c(rep(0.1, 2),
         rep(0.2, 3),
         rep(0.4, 5),
         rep(0.6, 10),
         rep(0.8, 10),
         rep(1, 30),
         rep(0.8, 10),
         rep(0.6, 10),
         rep(0.4, 5),
         rep(0.2, 3),
         rep(0.1, 2))

dd <- tibble(mu = mus %>% pull(est2),
             sd = sds,
             time = 1:90)

sim <- function(x){
  n <- rnbinom(n = 1, mu = 12, size = 10000)
  
  cc <- 90 / (n-1)
  
  tt <- round(seq(1 + cc, 90 - cc, length.out = (n-2)) + 
                rnorm(n = (n-2), 
                      0,
                      1))
  
  tt <- c(1, tt, 90)
  
  dt <- dd %>% 
    filter(time %in% tt)
  
  ss <- ar1_cor(n = n, rho = 0.7)
  
  Sigma <- cor_to_cov(ss, sd = dt %>% pull(sd))
  mu <- dt %>% pull(mu)
  
  ddd <- MASS::mvrnorm(n = 1,
                       Sigma = Sigma, 
                       mu = mu)
  
  dt %>% 
    add_column(ddd) %>% 
    mutate(ddd = ifelse(ddd < 0, rnorm(1, 0.1, 0.01), ddd)) %>% 
    mutate(ID = x)
  
}

du <- do.call(rbind, lapply(1:100, function(x) sim(x)))

du %>% 
  mutate(cgroup = as.character(ntile(time, 6)),
         cgroup2 = ntile(time, 48),
         cg = ifelse(cgroup2 == 8|cgroup2 == 9, 
                     sample(c(1,2), 
                            size = 19, 
                            replace = TRUE)[1:18],
                     ifelse(cgroup2 == 16|cgroup2 == 17,
                            sample(c(2,3), 
                                   size = 19, 
                                   replace = TRUE)[1:18],
                            ifelse(cgroup2 == 24|cgroup2 == 25, 
                                   sample(c(3,4), 
                                          size = 19, 
                                          replace = TRUE)[1:18],
                                   ifelse(cgroup2 == 32|cgroup2 == 33, 
                                          sample(c(4,5), 
                                                 size = 19, 
                                                 replace = TRUE)[1:18],
                                          ifelse(
                                            cgroup2 == 40|cgroup2 == 41, 
                                            sample(c(5,6), 
                                                   size = 19, 
                                                   replace = TRUE)[1:18],
                                            cgroup
                                          )
                                          
                                   )
                            )))) %>% 
  ggplot(aes(x = time,
             y = ddd)) +
  geom_point(aes(color = cg)) +
  geom_smooth(color = "black") +
  theme_minimal()
  #geom_smooth(aes(group = ID),
   #           se = FALSE,
    #          linewidth = 0.2,
     #         color = "black")
# add more than 6 individuals to show what happens when using
# a shape aes. 

#Time shift data by adding integer to time variable. 
#Or even better, stretch time to show variablity in longevity.

