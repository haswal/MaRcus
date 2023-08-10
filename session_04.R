#Returning to the issue regarding the wonky weight obs
ggplot(cb_data, 
       aes(x = Age_in_days,
           y = Weight)) +
  geom_point(aes(color = Observer))

#Lets use the filter() from dplyr to subset on raID-07

filter(cb_data,
       Observer == "raID-07")

#Assign to object (lets you view data frame)
cb_data_07 <- filter(cb_data,
                     Observer == "raID-07")

#Plot only raID-07 obs
ggplot(cb_data_07,
       aes(x = Age_in_days,
           y = Weight)) +
  geom_point()

#Plot raID-07 and the full data set side-by-side
p1 <- ggplot(cb_data,
             aes(x = Age_in_days,
                 y = Weight)) +
  geom_point() 

p2 <- ggplot(cb_data_07,
             aes(x = Age_in_days,
                 y = Weight)) +
  geom_point()

cowplot::plot_grid(p1, p2)

#Use mutate() to create new variable
#Give new variable a name
cb_data_fixed <- mutate(cb_data,
                        Weight_fixed = ifelse(Observer == "raID-07",
                                              Weight * 1000, 
                                              Weight))

#plot fixed data
ggplot(cb_data_fixed,
       aes(x = Age_in_days,
           y = Weight_fixed)) +
  geom_point(aes(color = Observer))