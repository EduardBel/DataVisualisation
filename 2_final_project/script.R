library(ggplot2)
library(forcats)
library(dplyr)

theme_set(theme_minimal())  #minimal theme

df=read.csv("./astronauts.csv") #read the CSV file

#How many astronauts of each nation have been sent to space?
unique_astronauts = unique(df[, c("name", "nationality")]) #this leaves only one instance of each astronaut
nationality_counts = as.data.frame(table(unique_astronauts$nationality))  #table format
colnames(nationality_counts) = c("nationality", "count")  #rename the columns

ggplot(nationality_counts, aes(y = reorder(nationality, count), x = count)) + #reorder by count
  geom_col(fill="darkblue", alpha=0.5, width = 0.75) + 
  geom_text(aes(label = count), nudge_x = 10, nudge_y = 0.05, size = 3) +
  ggtitle("How many astronauts of each nation have been sent to space?") +
  xlab("Count") +
  ylab("Nationality")
  ggsave("results/dist_nationalities.pdf", width = 17, height = 20, units = "cm")  #save as pdf
  
  
  
  
#How many men/women have gone to space by years?
unique_astronauts = unique(df[, c("name", "nationality", "year_of_mission")]) #this leaves only one instance of each astronaut per year
gender_counts = summarise(group_by(df, sex, year_of_mission), count = n_distinct(name)) #count the total

ggplot(gender_counts, aes(x = year_of_mission, y = count, fill = sex)) +
  geom_bar(stat = "identity", position = position_dodge(preserve = "single"), width = 0.75) +
  labs(title = "Number of Astronauts by Sex and Year of Mission",
       x = "Year of Mission",
       y = "Number of Astronauts",
       fill = "Sex") +
  scale_fill_manual(values = c("purple", "springgreen3")) +
  scale_x_continuous(breaks =  seq(1960,2020, 10),
                     labels = seq(1960,2020, 10)) +
  scale_y_continuous(breaks =  seq(0,max(gender_counts$count), 10),
                     labels = seq(0,max(gender_counts$count), 10))
ggsave("results/dist_gender.pdf", width = 25, height = 15, units = "cm")  #save as pdf

  
  
#Which is the distribution of spaceflights by astronauts?
unique_astronauts = unique(df[, c("name", "nationality", "total_number_of_missions")]) #this leaves only one instance of each astronaut

ggplot(unique_astronauts, aes(x = total_number_of_missions)) +
  geom_bar(width = 0.8, fill = "darkblue", alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 7, by = 1)) +
  labs(title = "Distribution of Spaceflights by Astronauts",
       x = "Total Number of Spaceflights",
       y = "Frequency")+
  stat_bin(bins=7, aes(label = ..count..), geom = "text", size = 4, position = position_nudge(y=6)) 
  ggsave("results/dist_spaceflights.pdf", width = 25, height = 15, units = "cm")  #save as pdf

    

  
#Does the EVA(Extra-Vehicular Activity) time have some relation with the duration of the mission? 
#Has the length of mission varied throughout the years?
  
# Aggregate by mission_title, year_of_mission and hours_of_mission, summing eva_hrs_mission
#this way we can see if there has been some mission lasting more than one year
partial_df = aggregate(eva_hrs_mission ~ mission_title + year_of_mission + hours_mission, data = df, FUN = sum)
#partial_df = partial_df %>% arrange(desc(eva_hrs_mission))
#partial_df 
partial_df$eva_bins = cut(partial_df$eva_hrs_mission, breaks = c(-0.1, 0,10,20,40,60,80,114.4), 
                          labels=c("[0]", "(0,10]", "(10,20]","(20,40]", "(40,60]", "(60,80]", "[114.4]"))
            # it would be interesting to know how many missions didn't need EVA time, we "fool" the system
            # by adding the "-0.1" value to be able to get the [0] "range"
            # since abave 80h there is only one occurrence I manually created the labeling

ggplot(partial_df, aes(x = year_of_mission, y = hours_mission)) +
  geom_point(aes(color = eva_bins), size=3, alpha=0.7, position = position_jitter()) +
  scale_color_manual(values = c("lightblue", "royalblue", "forestgreen", "gold", "darkorange", "firebrick","black")) +
  labs(title = "Scatterplot of EVA Hours vs. Year and Mission Hours",
       x = "Year of Mission",
       y = "Mission Hours",
       color = "EVA Hours")
  ggsave("results/EVA-hours.pdf", width = 25, height = 25, units = "cm")  #save as pdf

