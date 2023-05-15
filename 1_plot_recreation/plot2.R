library(ggplot2)
library(forcats)

#Beginning of the code
theme_set(theme_minimal())  #minimal theme

df=mtcars #assign the dataframe to a variable
cars=rownames(df) #keep the car names (they are the row names)
cars=factor(cars) #transform to factor to reorder

#calculate population mean and SD
mean=mean(df$mpg)
std_dev=sd(df$mpg)

#calculate z-score and putting it into a new column
df$zscore=(df$mpg-mean)/std_dev
df$lowhigh=ifelse(df$zscore > 0,1,0)  #create column for low-high(color), 
  #if the z-score is lower than 0, store 0, else 1

#reorder by z-score
cars=fct_reorder(cars, df$zscore, .desc = T) #reorder cars taking into account the 

ggplot(df, aes(x=zscore, y=cars, fill=factor(lowhigh))) + #fill by the factor of lowhigh
  geom_col(width = 0.65, alpha=abs(df$zscore)) + #change width of the bars and set alpha depending on the value of zscore
  scale_fill_manual(values = c("#191970", "#ffd602"), labels=c('low', 'high')) +  #manual pure colors and labels
  labs(x="MPG z-score", y="Car name", fill="MPG\nGroup")  #labeling
  ggsave("plot2.pdf", width = 13.3333, height = 20.8333, units = "cm")  #save as pdf