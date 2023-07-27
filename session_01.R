#Load tidyverse packages so we can use ggplot
library(tidyverse)

#Load the readxl package (needs to be installed first)
library(readxl)

#Use the read_xlsx() function to import data from an excel document
#Always take look at the data before attempting to read into R
#Use "skip =" to avoid reading in metadata lines
#Use "na =" to specify how NAs are coded
read_xlsx(path = "CB_data_2023.xlsx",
          skip = 11,
          na = "-99")

#Run ?read_xlsx to get more information about the function
?read_xlsx

#Assign to an object with <- ("Alt" + "-")
#Object names must start with a letter, can include letters, numbers, periods, underscores 
cb_data <- read_xlsx(path = "CB_data_2023.xlsx",
                     skip = 11,
                     na = "-99")

#Use glimpse() or view() to take a look at the data object
#view() the same as clicking the object name in Environment tab
glimpse(cb_data)
view(cb_data) 

#Use the ggplot function to start building a plot
ggplot(data = cb_data)

#Tell ggplot what to plot using "mapping = aes()"
#aes() mappings should refer to a column (variable) in the data
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight))

#Complete the graph by adding one or more layers
#Geoms determine the type of plot created
#geom_point = scatter plot for example
#Layers are added with a "+"
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight)) +
  geom_point()

#Add "+" to the end of the previous line of code
#Not at the beginning (will give an error)
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight))
+geom_point()

#Add more Aesthetics to make plot more informative
#For example color, alpha, shape, size
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight,
                     color = Observer)) +
  geom_point()

#Assign shapes to different observers instead of color
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight,
                     shape = Observer)) +
  geom_point()

#We must add backticks when variable name includes space 
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight,
                     shape = `Phase (color)`)) +
  geom_point()

#Specify aesthetics outside of aes() to change ALL obs
#Here using "alpha = " to show overplotting
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight,
                     color = `Phase (color)`)) +
  geom_point(alpha = 0.3,
             shape = 18)

#Divide plot into subplots with facets
#Facet_wrap takes one variable 
#Use the tilde character "~" to specify variable
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight,
                     color = `Phase (color)`)) +
  geom_point() +
  facet_wrap(~`Phase (color)`)

#Free scales using "scales ="
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight,
                     color = `Phase (color)`)) +
  geom_point() +
  facet_wrap(~`Phase (color)`,
             scales = "free_x")

#Control layout with "ncol =" or "nrow ="
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight,
                     color = `Phase (color)`)) +
  geom_point() +
  facet_wrap(~`Phase (color)`,
             scales = "free_x",
             ncol = 6)

#facet_grid will take two variables
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight,
                     color = `Phase (color)`)) +
  geom_point() +
  facet_grid(Observer ~ `Phase (color)`,
             scales = "free_x")

#Multiple geoms can be added to the same plot
#Here adding geom_smooth
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight,
                     color = `Phase (color)`)) +
  geom_point() +
  geom_smooth()

#Global vs local aes()
#If aes() is specified in ggplot(): affects all geoms (global)
#aes() can be added to geoms instead (local)
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight,
                     )) +
  geom_point(aes(color = `Phase (color)`)) +
  geom_smooth()

#Change line color (outside of aes)
#Color names shown with color in RStudio
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight,
       )) +
  geom_point(aes(color = `Phase (color)`)) +
  geom_smooth(color = "black")

#Multiple geoms of the same type can be added
#First geom added in code: first to be "drawn"
ggplot(data = cb_data, 
       mapping = aes(x = Age_in_days,
                     y = Weight,
       )) +
  geom_point(color = "white",
             size = 3) +
  geom_point(aes(color = `Phase (color)`))
