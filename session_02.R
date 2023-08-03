#Loading libraries 
library(tidyverse)
library(readxl)

#Reading in data the same way as last time
cb_data <- read_xlsx(path = "CB_data_2023.xlsx",
                     skip = 11,
                     na = "-99")

#geom_histogram makes histograms
#A histogram shows the distribution of a continuous variable
#Divides data into bins and counts how many obs per bin
#Height of bar = count
#Only specify "x" aesthetic 
ggplot(data = cb_data,
       aes(x = Volume)) +
  geom_histogram()

#Adjust number of bins with binwidth
ggplot(data = cb_data,
       aes(x = Volume)) +
  geom_histogram(binwidth = 1)

#Making a bar plot using geom_bar
#With only "x" aesthetic; counts number of obs
#Use with categorical variable
ggplot(data = cb_data,
       aes(x = Observer)) +
  geom_bar()

#Use fct_infreq() to order based on count
ggplot(data = cb_data,
       aes(x = fct_infreq(Observer))) +
  geom_bar()

#Use the fill aesthetic for example to add another variable
#Color of bars controlled using the fill aesthetic
#The default setting creates stacked bars
ggplot(data = cb_data,
       aes(x = Observer, 
       fill = `Phase (color)`)) +
  geom_bar()

#Color aesthetic controls outline
ggplot(data = cb_data,
       aes(x = Observer, 
           fill = `Phase (color)`)) +
  geom_bar(color = "black")

#Use position = "fill" to get percentages 
ggplot(data = cb_data,
       aes(x = Observer, 
           fill = `Phase (color)`)) +
  geom_bar(position = "fill")

#geom_bar with both x and y aesthetics
#Throws an error because of the default counting
ggplot(data = cb_data) +
  geom_bar(aes(x = `Phase (color)`,
               y = Volume))

#We need to specify "stat" for it to work
#stat = "identity" uses data values for plotting
#If multiple values per group, they are summed. 
ggplot(data = cb_data) +
  geom_bar(aes(x = `Phase (color)`,
               y = Volume),
           stat = "identity")

#Often more useful to be specific about what to plot
ggplot(data = cb_data) +
  geom_bar(aes(x = `Phase (color)`,
               y = Volume),
           stat = "summary",
           fun = "mean")

#We can flip axes to give more room for axis labels
ggplot(data = cb_data) +
  geom_bar(aes(x = `Phase (color)`,
               y = Volume),
           stat = "summary",
           fun = "mean") +
  coord_flip()

#Adding more information to our plot
#Here using the fill aesthetic
ggplot(data = cb_data) +
  geom_bar(aes(x = `Phase (color)`,
               y = Volume, 
               fill = Observer),
           stat = "summary",
           fun = "mean") +
  coord_flip()

#Plot bars side-by-side using position = "dodge"
ggplot(data = cb_data,
       aes(x = `Phase (color)`,
           y = Volume, 
           fill = Observer)) +
  geom_bar(stat = "summary",
           fun = "mean", 
           position = "dodge")

#Add errorbars
#Error bars also need position = "dodge" to align with bars
ggplot(data = cb_data,
       aes(x = `Phase (color)`,
           y = Volume, 
           fill = Observer)) +
  geom_bar(stat = "summary",
           fun = "mean", 
           position = "dodge") +
  geom_errorbar(stat = "summary",
                fun.data = "mean_se",
                position = "dodge")

#Show similar info as above using geom_boxplot
ggplot(data = cb_data,
       aes(x = `Phase (color)`,
           y = Volume, 
           fill = Observer)) +
  geom_boxplot()

#Assigning plot to object
#Use brackets to both assign a plot to an object and show it
(p1 <- ggplot(data = cb_data,
       aes(x = `Phase (color)`,
           y = Volume, 
           fill = Observer)) +
  geom_bar(stat = "summary",
           fun = "mean", 
           position = "dodge") +
  geom_errorbar(stat = "summary",
                fun.data = "mean_se",
                position = "dodge"))

(p2 <- ggplot(data = cb_data,
              aes(x = `Phase (color)`,
                  y = Volume, 
                  fill = Observer)) +
    geom_boxplot())

#Use the plot_grid() function (cowplot) to combine plots
#Instead of loading package, :: can be used to find function 
cowplot::plot_grid(p1, p2)

#Add a label
cowplot::plot_grid(p1, p2,
          labels = "AUTO")

#Export plot using ggsave
ggsave(filename = "plot1.pdf",
       plot = p1)
