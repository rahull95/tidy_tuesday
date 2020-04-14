library(tidyverse)
library(ggthemes)
library(wesanderson)
library(RColorBrewer)
library(ggdark)

polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

#Maximum points for a title for each year
x1 = rankings %>% select(year, title, points) %>% group_by(year, title) %>% 
    summarise(max_pts = max(points)) %>% arrange(year) %>%
    group_by(year) %>% slice(which.max(max_pts))


ggplot(x1, aes(x=year, y=max_pts, color = max_pts)) + geom_point() + 
    geom_text(aes(label=ifelse(max_pts>50,as.character(title),'')),hjust=-0.1,vjust=-0.2, size = 3.5) + 
    scale_color_gradient(low = "lightblue", high = "darkblue") +
    scale_x_continuous(breaks = round(seq(min(x1$year), max(x1$year), by = 5),1)) + 
    xlab("Year") + ylab("Maximum points") + theme_wsj() + theme(legend.position = "none") + 
    ggtitle("HIGHEST POINTS FOR A TITLE BY RELEASE YEAR") + 
    theme(plot.title = element_text(hjust = 0.5, vjust = 2, size = 15))




#Percentage of votes across gender by release years
data <- polls  %>% select(year, gender) %>% group_by(year, gender) %>% summarise(cnt = n())
df1 = data %>% group_by(year) %>% mutate(pct = cnt/sum(cnt))
df1$Gender <- factor(df1$gender, levels=c("mixed","female","male"))

ggplot(df1, aes(x=year, y=pct, fill=Gender)) + 
    geom_col(alpha = 0.9,size = 1, colour="black")  + 
    scale_x_continuous(breaks = seq(1985, 2015, by = 5), limits = c(1982, 2019)) +
    xlab("Year") + ylab("Percentage (%)") + ggtitle("Percentage of votes across genders by song release year") +
    dark_theme_classic() + scale_fill_brewer(palette="Spectral") +
    theme(plot.title = element_text(hjust = 0.5))