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

set.seed(124)
du <- do.call(rbind, lapply(1:100, function(x) sim(x)))

dud <- du %>% 
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
  group_by(cg) %>% 
  mutate(sds = (1/sd(ddd)*4)) %>%
  ungroup() %>% 
  group_by(ID) %>% 
  mutate(time2 = ifelse(time == max(time), max(time) + round(rnorm(1, 0, 1.7)), time),
         time2 = ifelse(time2 == max(time2), ifelse(max(time2) > 91, 91, max(time2)), time2)
  ) %>%
  ungroup() %>% 
  mutate(sds = ifelse(sds > 1.2, sds - 1, sds),
         sds = ifelse(cg == 1|cg == 6, sds / 10, sds)) %>%
  mutate(d3 = abs(ddd^1.7+rnorm(n = n(), mean = sds^2, sd = sds)),
         d3 = d3 - (min(d3)+0.00001)) %>%
  mutate(Obs = ifelse(ID <= 16,
                      1,
                      ifelse(ID <= 31, 
                             2, 
                             ifelse(ID <= 49,
                                    3,
                                    ifelse(ID <= 60,
                                           4,
                                           ifelse(ID <= 80,
                                                  5,
                                                  ifelse(ID <= 96,
                                                         6,
                                                         7)))))),
         d3 = ifelse(Obs == 7, d3/1000, d3),
         Observer = paste0("raID-0", Obs),
         CloudBuddy = paste0("cbID-", stringr::str_pad(ID, 3, pad = 0)),
         Phase = case_match(cg, 
                            "1" ~ "lightgray",
                            "2" ~ "darkgray",
                            "3" ~ "darkorange",
                            "4" ~ "orange",
                            "5" ~ "orangered",
                            "6" ~ "red")) %>% 
  rownames_to_column() %>% 
  mutate(ddd = ifelse(rowname %in% c(5, 154, 255, 435, 634, 737, 776, 843, 912, 1111), -99, ddd),
         d3 = ifelse(rowname %in% c(7, 123, 225, 375, 434, 637, 726, 813, 952, 1093), -99, d3)) %>% 
  select(CloudBuddy, Observer, time2, ddd, d3, Phase) %>% 
  rename("Volume" = ddd,
         "Weight" = d3,
         "Age_in_days" = time2,
         "Cloudbuddy" = CloudBuddy,
         "Phase (color)" = Phase)

  #filter(Observer == "raID-07") %>% 
  #group_by(cg) %>% 
  #summarize(cor(ddd, d3)^2)
  #filter(cg == 2) %>%
dud %>% 
  filter(Observer != "raID-07") %>% 
  ggplot(aes(x = Age_in_days,
             y = Volume)) +
  geom_point(aes(color = `Phase (color)`), 
            alpha = 1) +
  geom_smooth(color = "black") + 
  scale_color_manual(values = c("darkgray" = "darkgray",
                                "darkorange" = "darkorange",
                                "lightgray" = "lightgray",
                                "orange" = "orange",
                                "orangered" = "orangered",
                                "red" = "red")) + 
  theme_minimal() +
  ylim(0,11)
  
dud %>%   
  as.data.frame() %>% 
    xlsx::write.xlsx(file = "CB_data_2023.xlsx",
                     row.names = FALSE)

    #facet_wrap(~cg,
               scales = "free_x",
               ncol = 6)
  #geom_smooth(aes(group = ID),
   #           se = FALSE,
    #          linewidth = 0.2,
     #         color = "black")
# add more than 6 individuals to show what happens when using
# a shape aes. 

#Time shift data by adding integer to time variable. 
#Or even better, stretch time to show variablity in longevity.

