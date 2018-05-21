library(data.table)
library(dplyr)
library(ggplot2)

responses = fread('multipleChoiceResponses.csv')

df_country_age = responses %>%
  group_by(Country) %>%
  summarise(AgeMedian = median(Age, na.rm = T)) %>%
  arrange(desc(AgeMedian))

ggplot(head(df_country_age), aes(x = reorder(Country, AgeMedian), y = AgeMedian))+
  geom_bar(aes(fill=Country), stat = 'identity') + labs(x = 'Country', y = 'AgeMedian')+
  geom_text(aes(label = AgeMedian), hjust = 1.5, color = 'white') +coord_flip()+
  theme_minimal()+ theme(legend.position = 'none')

fun1 <- function(data, xlab, ylab, xname, yname){
  ggplot(data, aes(xlab, ylab))+
    geom_bar(aes(fill=xlab), stat = 'identity')+
    labs(x = xname, y = yname)+
    geom_text(aes(label = ylab), hjust = 1.5, colour = 'white')+
    coord_flip()+
    theme_minimal()+
    theme(legend.position = 'none')
}

df_CJT = responses %>%
  filter(CurrentJobTitleSelect != '') %>%
  group_by(CurrentJobTitleSelect) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count))
data = head(df_CJT, 10)
xname = 'Job'
yname = 'Count'
fun1(data, reorder(data$CurrentJobTitleSelect, data$Count), data$Count, xname, yname)
