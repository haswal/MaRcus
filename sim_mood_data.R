library(tidyverse)
library(babynames)
library(readxl)


cb_data <- read_xlsx(path = "CB_data_2023.xlsx",
                     skip = 11,
                     na = "-99")

vec <- c(rep("cbID-", 25),
         "cloudbuddyID-", 
         "clbud-Id-", 
         "cbIDnumber-",
         "cbIDnum-")

vec2 <- c("",
          "0",
          "0",
          "0",
          "0",
          "0",
          "00")

vec3 <- c(rep("very sad", 2),
          rep("Very sad", 2),
          rep("sad", 4),
          rep("Sad", 4),
          rep("neutral", 15),
          rep("Neutral", 15),
          rep("happy", 10), 
          rep("Happy", 10),
          rep("very happy", 5), 
          rep("Very Happy", 5),
          rep("medium", 2),
          rep("Very super happy", 2),
          rep("very very happy", 2),
          rep("v happy", 2),
          rep("V sad", 2),
          rep("oh so sad", 2))

vec4 <- c(rep("", 50),
          rep("-days", 5))

IDs <- cb_data %>% 
  filter(!Cloudbuddy %in% c("cbID-100")) %>% 
  group_by(Cloudbuddy) %>% 
  count() %>% 
  ungroup() %>% 
  slice_head(n = 13) %>% 
  pull(Cloudbuddy)

mood_data <- cb_data %>% 
  filter(Cloudbuddy %in% IDs) %>% 
  separate(Cloudbuddy, 
           into = c("chr", "number"), 
           remove = FALSE) %>% 
  mutate(number = str_replace_all(number, "0*(?=[1-9])", "")) %>% 
  group_by(Cloudbuddy) %>% 
  mutate(nn = str_c(sample(vec, size = n(), replace = TRUE), 
                    sample(vec2, size = n(), replace = TRUE),
                    number),
         mood = sample(vec3, size = n(), replace = TRUE),
         Age = str_c(Age_in_days, sample(vec4,
                                         size = n(),
                                         replace = TRUE))) %>% 
  ungroup() %>% 
  mutate(nn = str_replace(nn, "12", "11b"),
         nn = str_replace(nn, "13", "11c")) %>% 
  select(nn, mood, Age) %>% 
  rename(Cloudbuddy = nn)

write_csv(mood_data, "mood_data.csv")

mood_data %>% 
# str_to_lower changes all characters to lower case
# See also str_to_upper, str_to_title, str_to_sentence
  mutate(mood = str_to_lower(mood),
#str_replace (and str_replace_all) can be used as find-replace
         mood = str_replace(mood, "medium", "neutral"),
#str_replace uses regex when searching for parts of string
#regex is a language used for more advanced string search
#for example ".+" matches ANY character one or more times
         mood = str_replace(mood, ".+happy", "very happy"),
         mood = str_replace(mood, ".+sad", "very sad")) %>% 
  count(mood)


cb_data %>% 
  filter(Cloudbuddy %in% IDs) %>% 
  separate(Cloudbuddy, 
           into = c("chr", "number"), 
           remove = FALSE) %>% 
  mutate(number = str_replace_all(number, "0*(?=[1-9])", "")) %>% 
  group_by(Cloudbuddy) %>% 
  mutate(nn = str_c(sample(vec, size = n(), replace = TRUE), 
                    sample(vec2, size = n(), replace = TRUE),
                    number)) %>% 
  ungroup() %>% 
         mutate(ID_2 = str_to_upper(nn),
         ID_3 = str_replace(ID_2, "0*(?=[1-9]{1})", "00"),
         ID_4 = str_replace(ID_3, "0*(?=[0-9]{2})", "0"),
         ID_5 = str_replace(ID_4, ".*(?=ID)", ""),
         ID_6 = str_replace(ID_5, "(?<=ID)\\w+", ""),
         ID_7 = str_c("cb", ID_6)) %>%  
  view()


cb_data %>% 
  filter(Cloudbuddy %in% IDs) %>% 
  separate(Cloudbuddy, 
           into = c("chr", "number"), 
           remove = FALSE) %>% 
  mutate(number = str_replace_all(number, "0*(?=[1-9])", "")) %>% 
  group_by(Cloudbuddy) %>% 
  mutate(nn = str_c(sample(vec, size = n(), replace = TRUE), 
                    sample(vec2, size = n(), replace = TRUE),
                    number),
         ID2 = str_replace(nn, "\\D*(?=\\d+)", ""),
         ID3 = str_replace(ID2, "0*(?=[1-9]{1})", "00"),
         ID4 = str_replace(ID3, "0*(?=[0-9]{2})", "0"),
         ID5 = str_c("cbID-", ID4)) %>% 
  view()

str_to_upper("id-01")
str_view(c("v v happy"), "\\w*\\s\\wy$")


babynames %>% 
  group_by(year, sex) %>% 
  mutate(sname = sum(prop)) %>% 
  view()
  summarise(prop_x = mean(str_detect(name, "x|X"))) %>% 
  ggplot(aes(x = year, 
             y = prop_x, 
             color = sex)) +
  geom_line()

babynames %>% 
  filter(str_detect(name, "^Has")) %>% 
  view()

d <- tibble(ID = c(str_c("id-", c("01", "001", "1", "0001")),
                   "cloudbuddyID-001",
                   "clbud-Id-001",
                   "IDnumber-012",
                   "IDnum-001"))

d2 <- tibble(mood = c("very sad",
                      "very very sad",
                      "very happy"))

#The str_ function I use the most is str_replace(_all).
string <- "Python is my favorite programming language"
str_replace(string, "Python", "R")

# Filter on cb with "1" in ID using str_detect()

#Highlight code and replace

# 0* means zero or more zeros
# (?=[1-9]) means before numbers between 1 and nine
#




d %>% 
  mutate(ID_2 = str_to_upper(ID),
         ID_3 = str_replace(ID_2, "0*(?=[1-9]{1})", "00"),
         ID_4 = str_replace(ID_3, "0*(?=[1-9]{2})", "0"),
         ID_5 = str_replace(ID_4, ".*(?=ID)", ""),
         ID_6 = str_replace(ID_5, "(?<=ID)\\w+", ""),
         ID_7 = str_c("cb", ID_6)) %>% 
  view()

  

