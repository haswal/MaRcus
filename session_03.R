#Loading libraries 
library(tidyverse)
library(readxl)

#Reading in data per usual
cb_data <- read_xlsx(path = "CB_data_2023.xlsx",
                     skip = 11,
                     na = "-99")

#For all dplyr functions (verbs):  
#-First argument is data
#-Subsequent arguments describe what to do
#-Result is a new data frame
#Here using filter() to subset (rows) on Observer raID-07
filter(cb_data,
       Observer == "raID-07")

#Transforming data often involves many dplyr verbs
#Best way to string R commands together is to use pipe: %>% 
#Keyboard shortcut: cmd+shift+m
#When using the pipe, tabbing is really useful
cb_data %>% 
  filter(Observer == "raID-07")

#Pipe to filter, then view()
cb_data %>% 
  filter(Observer == "raID-07") %>% 
  view()

#Pipe to filter, then ggplot
cb_data %>% 
  filter(Observer == "raID-07") %>% 
  ggplot(aes(x = Age_in_days,
              y = Weight)) +
  geom_point()

#Filter uses conditions: ==, !=, >, <, >=, <=
#All Observers BUT raID-07
cb_data %>% 
  filter(Observer != "raID-07") %>% 
  ggplot(aes(x = Age_in_days,
             y = Weight)) +
  geom_point(aes(color = Observer))

#Ages greater than 50 days (inclusive)
cb_data %>% 
  filter(Age_in_days >= 50) %>% 
  ggplot(aes(x = Age_in_days,
             y = Weight)) +
  geom_point()

#Filter using multiple conditions
cb_data %>% 
  filter(Age_in_days >= 50, 
         Observer != "raID-07") %>% 
  ggplot(aes(x = Age_in_days,
             y = Weight)) +
  geom_point()

#Separating conditions with comma same as using "&"

cb_data %>% 
  filter(Age_in_days >= 50 &
           Observer != "raID-07") %>% 
  ggplot(aes(x = Age_in_days,
             y = Weight)) +
  geom_point()

#Use logical OR (|) to check for either condition
cb_data %>% 
  filter(Cloudbuddy == "cbID-001" |
           Cloudbuddy == "cbID-002")

#Shortcut: %in% 
cb_data %>% 
  filter(Cloudbuddy %in% c("cbID-001",
                           "cbID-002"))

#Can also be used with "!"
cb_data %>% 
  filter(!Cloudbuddy %in% c("cbID-001",
                            "cbID-002"))

#Cannot use variable == NA, must use is.na()
cb_data %>% 
  filter(Volume == NA)

cb_data %>% 
  filter(is.na(Volume))

cb_data %>% 
  filter(!is.na(Volume))

#dplyr verbs will not save new data, assign to save
cb_data_new <- cb_data %>% 
  filter(Cloudbuddy %in% c("cbID-001",
                           "cbID-002")) 

#Next verb is arrange 
#Will arrange a column's values in ascending order by default
cb_data %>% 
  arrange(Volume)

#To view in descending order, use desc()
cb_data %>% 
  arrange(desc(Volume))

#Use select() to pick columns in data
#Can be done by name
#Output ordered
cb_data %>% 
  select(Weight, 
         Volume, 
         Cloudbuddy)

#Or range of names 
cb_data %>% 
  select(Age_in_days:`Phase (color)`)

#Or exclude range
cb_data %>% 
  select(!Age_in_days:`Phase (color)`)

#Or helper functions
cb_data %>% 
  select(starts_with("W"))

cb_data %>% 
  select(contains("_"))

#By variable type
cb_data %>% 
  select(where(is.numeric))

cb_data %>% 
  select(where(is.character))

#Use rename() to rename columns (variables)
#new_name = old_name
cb_data %>% 
  rename(Phase = `Phase (color)`)

