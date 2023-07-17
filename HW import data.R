library(mgcv)
library(gratia)
library(tidyverse)


d1 <- tribble(
  ~x,  ~y,  ~z, 
  "Week 1", "1.21", "Control",
  "Week 2", "2.32", "Drug",
  "!!No data collection done", "", "",
  "Week 4", "3.14", "N/A",
  "Week 5", "N/A", "Control"
)

d2 <- tribble(
  ~x,  ~y,  ~z, 
  "Week 1", "1.21", "Control",
  "Week 2", "2.32", "Drug",
  "!!No data collection done", "", "",
  "Week 4", "3.14", "N/A",
  "Week 5", "N/A", "Control"
)

df <- tribble(
  ~x,  ~y,
  "1", "1.21",
  "2", "2.32",
  "!!Data collection resumed", "",
  "3", "-999"
)

dd <- tribble(
  ~values,  ~date,
  "1.34", "3 Februari 2012",
  "4.2521", "17 Mars 2013",
  "2.544", "Datum saknas",
  "8.22", "30 Augusti 2014"
)

write_csv(d1, "data1.csv")
write_delim(dd, "test.csv", delim = "|")

read_delim("test.csv", 
         comment = "!!",
         na = "Datum saknas",
         delim = "|",
         locale = locale("sv"),
         col_types = cols(
           values = col_double(),
           date = col_date("%d %B %Y")
         ))

read_csv("data1.csv", 
         na = "N/A", 
         comment = "!!")

ggplot(data = mpg) + 
  geom_point(mapping = aes(x = drv, y = class))
