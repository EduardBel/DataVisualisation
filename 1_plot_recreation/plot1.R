library(ggplot2)
library(forcats)

#Beginning of the code
theme_set(theme_minimal())  #minimal theme

df=mtcars #assign the dataframe to a variable
cars=rownames(df) #keep the car names (they are the row names)
cars=factor(cars) #transform to factor to reorder

#reorder by cylinder and mpg
cars=fct_reorder(cars, df$mpg, .desc = T) #reorder cars taking into account the 
  #mpg variable in a descending order
cars=fct_reorder(cars, df$cyl) #reorder cars taking into account the #cylinders

ggplot(df, aes(x=mpg, y=cars, fill=factor(cyl))) + geom_col(width = 0.75) + #change width of the bars
  scale_fill_manual(values=c("#A6CEE3", "#2078B4", "#B2DF8A")) + #manually input exact colors
  labs(x="Miles per gallon", y=NULL, fill="Number of\ncylinders")+  #labeling
  theme(legend.position = "top")  #changing legend position to top
ggsave("plot1.pdf", width = 13.3333, height = 20.8333, units = "cm")  #save as pdf