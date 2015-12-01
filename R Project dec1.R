##        Charles' Rodent Analysis Script
## Script for analyzing rodent data and uploading analyses to github
## includes three graphs for exploring correlations between data and one statistical ANOVA test.

#Install and load dplyr package
install.packages("dplyr")
library(dplyr)

#Install and load ggplot2 package
install.packages("ggplot2")
library(ggplot2)

#download Data file
download.file("http://files.figshare.com/2236372/combined.csv", "data/portal_data_joined.csv")

#load data file into environment
surveys <- read.csv("data/portal_data_joined.csv")

#parse data
surveysSample <- surveys %>% select(month, species, sex, hindfoot_length, weight) %>%
  filter(!sex == "P") %>%
  filter(!sex == "R") %>%
  filter(!sex == "Z") %>%
  filter(!sex == "")     #filter out unneeded data columns and unwanted categories within columns

# Rodent weight vs hindfoot length scatterplot
pdf("Figures/Rodent Weight vs Hindfoot Length.pdf") #save figure as pdf
p <- ggplot(data = surveysSample, aes(x=weight, y=hindfoot_length, color=species)) + geom_point(size=1.00) 
p + ggtitle("Hindfoot Length of Various Rodent Species\nas a Function of Weight") 
p + scale_x_continuous(limits=c(0, 300), breaks=c(0, 50, 100, 150, 200, 250, 300)) #scatter plot color coded by species
dev.off() #stop saving

# male vs female weight, by spp. bar graph
pdf("Figures/Rodent Mean Weight by Species and Sex.pdf")  #save figure as pdf
ggplot(data = surveysSample, aes(x=species, y=weight, fill=sex)) +
  geom_bar(position="dodge", stat="identity") + coord_flip() + 
  ggtitle("Average Weight of Various Rodent Species\nSeparated by Sex") + 
  theme(plot.title=element_text(size=rel(1.5)))  #bar graph with formatting
dev.off() #stop saving

#Rodent weight by month boxplot
pdf("Figures/Seasonal Rodent Weight Variation by Sex.pdf") #save figure as pdf
ggplot(data=surveysSample, aes(x=factor(month), y=weight, fill=sex)) + 
  geom_boxplot() + scale_x_discrete(breaks=c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
  labels=c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")) + 
  ggtitle("Rodent Weight by Month") + theme(plot.title=element_text(size=rel(1.5))) +
  theme(axis.text.x = element_text(angle=30, hjust=1, vjust=1)) +
  xlab("Month") + ylab("Weight in grams") + labs(fill="Legend")  #boxplot, with much axis formatting
dev.off() #stop saving

# ANOVA test: how does hindfoot length depend on weight and species?
fit <- aov(hindfoot_length ~ weight * species, data = surveysSample) # fit model
fit # look at fit
summary(fit) # summarize and show results

#"I think, therefore I R"
