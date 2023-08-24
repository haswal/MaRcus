#Loading libraries 
library(tidyverse)
library(readxl)

#Reading in data per usual
cb_data <- read_xlsx(path = "CB_data_2023.xlsx",
                     skip = 11,
                     na = "-99")

#dplyr continued 
#Next verb: mutate
#Use mutate to create new variables (columns)
#Here creating Weight variable in kg instead of grams
cb_data %>% 
  mutate(Weight_kg = Weight / 1000) %>% 
  select(Weight, Weight_kg) %>% 
  view()

#Mutate multiple columns at once
cb_data %>% 
  mutate(grams_per_m3 = Weight / Volume, 
         grams_per_m3_sq = grams_per_m3^2,
         grams_per_m3_sq_log2 = log2(grams_per_m3_sq)) %>% 
  view()
  
#Returning to the issue regarding the wonky weight obs
ggplot(cb_data, 
       aes(x = Age_in_days,
           y = Weight)) +
  geom_point(aes(color = Observer))

#Lets make two plots
#One with only "raID-07"
#One with all other Observers
#plot side-by-side
p1 <- cb_data %>% 
  filter(Observer == "raID-07") %>% 
  ggplot(aes(x = Age_in_days,
             y = Weight)) +
  geom_point()

p2 <- cb_data %>% 
  filter(Observer != "raID-07") %>% 
  ggplot(aes(x = Age_in_days,
             y = Weight)) +
  geom_point()

cowplot::plot_grid(p1, p2)

#Use mutate() to create new variable fixing the issue
#ifelse() helps create variable based on values of other variable
cb_data_fixed <- cb_data %>% 
  mutate(Weight_fixed = ifelse(Observer == "raID-07",
                                              Weight * 1000, 
                                              Weight))

#plot new variable
cb_data_fixed %>%
  ggplot(aes(x = Age_in_days,
             y = Weight_fixed)) +
  geom_point(aes(color = Observer))

#Steps piped together
cb_data %>% 
  mutate(Weight_fixed = ifelse(Observer == "raID-07",
                               Weight * 1000, 
                               Weight)) %>% 
  ggplot(aes(x = Age_in_days,
             y = Weight_fixed)) +
  geom_point(aes(color = Observer))

#Mutate adds new variables at the end of tibble
#Transmute keeps only new variables
cb_data %>% 
  transmute(WV_sum = Weight + Volume,
            WV_diff = Weight - Volume)

#What happens here?
cb_data %>% 
  mutate(mean_Age = mean(Age_in_days)) %>% 
  view()

#Next verb is summarise (or summarize)
#Used to calculate summary statistics
cb_data %>% 
  summarise(mean(Age_in_days))

#Output can be named (column name)
cb_data %>% 
  summarise(mean_Age = mean(Age_in_days))

#Summarise is most useful together with group_by
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  summarise(mean_Age = mean(Age_in_days))

#Multiple summary stats can be calculated at the same time
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  summarise(mean_Age = mean(Age_in_days),
            stdev_Age = sd(Age_in_days),
            n = n())

#And can be combined with logical operations
#sum counts
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  summarise(sum(Age_in_days > 10))

#mean() gives proportion
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  summarise(mean(Age_in_days > 10))

#We can also group by multiple variables
cb_data %>% 
  group_by(`Phase (color)`, Observer) %>% 
  summarise(mean(Age_in_days)) %>% 
  view()

#What happens here?
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  summarise(mean(Volume))

#NAs are "contagious"; if any NA calculations will be NA
#Solution with filter
cb_data %>% 
  filter(!is.na(Volume)) %>% 
  group_by(`Phase (color)`) %>% 
  summarise(mean(Volume))

#Use na.rm (na remove) to change default behavior
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  summarise(mean(Volume, na.rm = TRUE))

#group_by can be used with other functions than summarise
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  mutate(mean_Vol = mean(Volume, na.rm = TRUE)) %>% 
  arrange(`Phase (color)`) %>% 
  view()

cb_data %>% 
  group_by(`Phase (color)`) %>% 
  count()

#Use ungroup() to remove grouping
cb_data %>% 
  group_by(`Phase (color)`) %>% 
  mutate(mean_vol = mean(Volume, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(grand_mean_vol = mean(Volume, na.rm = TRUE)) %>% 
  view()

#A somewhat more complicated example
#Here calculating what is called the "sum of squares between"
cb_data %>% 
  filter(!is.na(Volume)) %>% 
  mutate(grand_mean_vol = mean(Volume)) %>% 
  group_by(`Phase (color)`) %>% 
  summarise(mean_vol = mean(Volume),
            grand_mean_vol = first(grand_mean_vol),
            n = n()) %>% 
  mutate(squares = (mean_vol - grand_mean_vol)^2,
         squares_n = squares * n) %>% 
  summarise(sum_of_squares = sum(squares_n))
