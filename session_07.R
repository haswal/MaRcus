#Loading libraries 
library(tidyverse)
library(readxl)

#Reading in data per usual
cb_data <- read_xlsx(path = "CB_data_2023.xlsx",
                     skip = 11,
                     na = "-99")

#We have previously used filter() to find exact matches in data
cb_data %>% 
  filter(Cloudbuddy == "cbID-001")

#What if we wanted to find all Cloudbuddies with 2 in ID
#We can use str_detect() from the stringr package
#All stringr functions start with str_
cb_data %>% 
  filter(str_detect(Cloudbuddy, "2")) %>% 
  view()

#String detection in str_detect uses regular expressions (regex)
#regex is a mini-language used for describing string patterns  
#regex uses characters with special meaning
#For example if we want to find all cbIDs ending with 9 -> 9$
cb_data %>% 
  filter(str_detect(Cloudbuddy, "9$")) %>% 
  view()

#To find all cbIDs with 7 or 9 -> use []
cb_data %>% 
  filter(str_detect(Cloudbuddy, "[79]")) %>% 
  view()

#To find all cbIDs with 7, 8, or 9 -> use [] to specify range
cb_data %>% 
  filter(str_detect(Cloudbuddy, "[7-9]")) %>% 
  view()

#Specify number of times a character appears -> use {}
cb_data %>% 
  filter(str_detect(Cloudbuddy, "[7-9]{2}")) %>% 
  view()

#New data!
cb_mood <- read_csv("mood_data.csv", 
                    skip = 3)

#Very messy!
cb_mood %>% 
  view()

#Time to do some cleaning! We will start with the mood column
cb_mood %>% 
  count(mood) %>% 
  view()

#First thing I notice is mix of upper and lower case characters
#str_to_lower changes all characters to lower case
#See also str_to_upper, str_to_title, str_to_sentence
cb_mood %>% 
  mutate(mood = str_to_lower(mood)) %>% 
  count(mood) %>% 
  view("2")

#Next, "medium" should probably be "neutral" 
#str_replace can be used as find-replace
cb_mood %>% 
  mutate(mood = str_to_lower(mood),
         mood = str_replace(mood, "medium", "neutral")) %>% 
  count(mood) %>% 
  view("3")

#All strings with something before "happy" should be "very happy"
#And all strings with something before "sad" should be "very sad"
#regex wildcard (matches any character) -> .
#Wildcard can be used with quantifier, + means once or more times
cb_mood %>% 
  mutate(mood = str_to_lower(mood),
         mood = str_replace(mood, "medium", "neutral"),
         mood = str_replace(mood, ".+happy", "very happy"),
         mood = str_replace(mood, ".+sad", "very sad")) %>% 
  count(mood) %>% 
  view("4")

#Now let's clean the Age column 
#The problem here is that "-days" is added to some obs
cb_mood %>% 
  view()

#To match non-digits we use \\D (\\d for digits)
#IMPORTANT! Only R uses two slashes (\\)
#Online you will therefore more often see something like \D
#We use + again as a quantifier and replace with nothing ("")
cb_mood %>% 
  mutate(mood = str_to_lower(mood),
         mood = str_replace(mood, "medium", "neutral"),
         mood = str_replace(mood, ".+happy", "very happy"),
         mood = str_replace(mood, ".+sad", "very sad"),
         Age = str_replace(Age, "\\D+", "")) %>% 
  view()

#We also need to turn Age into numeric 
#The "-days" part forced it to be character before we removed it
cb_mood %>% 
  mutate(mood = str_to_lower(mood),
         mood = str_replace(mood, "medium", "neutral"),
         mood = str_replace(mood, ".+happy", "very happy"),
         mood = str_replace(mood, ".+sad", "very sad"),
         Age = str_replace(Age, "\\D+", ""),
         Age = as.numeric(Age)) %>% 
  glimpse()

#Next, the Cloudbuddy column
cb_mood %>% 
  view()

#The only informative part of the cbID is the number
#We keep that part the same way as before ("\\D+", "")
cb_mood %>% 
  mutate(mood = str_to_lower(mood),
         mood = str_replace(mood, "medium", "neutral"),
         mood = str_replace(mood, ".+happy", "very happy"),
         mood = str_replace(mood, ".+sad", "very sad"),
         Age = str_replace(Age, "\\D+", ""),
         Age = as.numeric(Age),
         ID2 = str_replace(Cloudbuddy, "\\D+", "")) %>% 
  view()

#Next problem: the number of leading zeros
#Single digit non-zeros in ID should have two leading zeros
#We use regex "look around" to find zeros followed by non-zero
#regex "followed by" -> a(?=b); finds "a" if followed by "b"
#The * quantifier -> zero or more times
cb_mood %>% 
  mutate(mood = str_to_lower(mood),
         mood = str_replace(mood, "medium", "neutral"),
         mood = str_replace(mood, ".+happy", "very happy"),
         mood = str_replace(mood, ".+sad", "very sad"),
         Age = str_replace(Age, "\\D+", ""),
         Age = as.numeric(Age),
         ID2 = str_replace(Cloudbuddy, "\\D+", ""),
         ID3 = str_replace(ID2, "0*(?=[1-9])", "00")) %>% 
  view()

#The code above also added two zeros to IDs with two non-zeros
#Fix by finding zeros followed by two digits 
#Replace with one zero
cb_mood %>% 
  mutate(mood = str_to_lower(mood),
         mood = str_replace(mood, "medium", "neutral"),
         mood = str_replace(mood, ".+happy", "very happy"),
         mood = str_replace(mood, ".+sad", "very sad"),
         Age = str_replace(Age, "\\D+", ""),
         Age = as.numeric(Age),
         ID2 = str_replace(Cloudbuddy, "\\D+", ""),
         ID3 = str_replace(ID2, "0*(?=[1-9])", "00"),
         ID4 = str_replace(ID3, "0*(?=\\d{2})", "0")) %>% 
  view()

#Now add the "cbID-" part to the ID
#Use str_c() for this.
cb_mood %>% 
  mutate(mood = str_to_lower(mood),
         mood = str_replace(mood, "medium", "neutral"),
         mood = str_replace(mood, ".+happy", "very happy"),
         mood = str_replace(mood, ".+sad", "very sad"),
         Age = str_replace(Age, "\\D+", ""),
         Age = as.numeric(Age),
         ID2 = str_replace(Cloudbuddy, "\\D+", ""),
         ID3 = str_replace(ID2, "0*(?=[1-9])", "00"),
         ID4 = str_replace(ID3, "0*(?=\\d{2})", "0"),
         ID5 = str_c("cbID-", ID4)) %>% 
  view()

#Select, rename and save as object
cb_mood_clean <- cb_mood %>% 
  mutate(mood = str_to_lower(mood),
         mood = str_replace(mood, "medium", "neutral"),
         mood = str_replace(mood, ".+happy", "very happy"),
         mood = str_replace(mood, ".+sad", "very sad"),
         Age = str_replace(Age, "\\D+", ""),
         Age = as.numeric(Age),
         ID2 = str_replace(Cloudbuddy, "\\D+", ""),
         ID3 = str_replace(ID2, "0*(?=[1-9])", "00"),
         ID4 = str_replace(ID3, "0*(?=\\d{2})", "0"),
         ID5 = str_c("cbID-", ID4)) %>% 
  select(mood, Age, ID5) %>% 
  rename(Age_in_days = Age, 
         Cloudbuddy = ID5)

#What if we wanted to look at how Volume varies with mood?
#We need to combine (join) cb_data and cb_mood_clean
#Before we join we need to make sure we have a key for joining
#A key is a variable (variables) that uniquely identify all obs
cb_mood_clean %>% 
  count(Cloudbuddy)

cb_mood_clean %>% 
  count(Cloudbuddy, Age_in_days) %>% 
  filter(n > 1)

#inner_join() only keeps obs from x that have a matching key in y
#Join will by default use variable(s) found in both sets as key 
cb_data %>% 
  inner_join(cb_mood_clean) %>% 
  view()

#left_join keeps all observations in x (this is the go-to join)
cb_data %>% 
  left_join(cb_mood_clean) %>% 
  view()

#What does right_join do?
cb_data %>% 
  right_join(cb_mood_clean) %>% 
  view()
