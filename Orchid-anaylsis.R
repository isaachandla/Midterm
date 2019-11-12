library(readr)
library(readxl)
library(tidytext)
library(tidyverse)
library(dplyr)
library(stringr)

f <- file.choose()
d <- read_excel(f)
head(d)

#figure1
d$`Life form`<- tolower(d$`Life form`)
Specimen_breakdown <- group_by(d, `Life form`) %>% summarize(count = n()) %>% arrange(desc(count)) %>%
  mutate(percentage = count/sum(count)*100)

d$Species <- tolower(d$Species)
Species_breakdown <- select(d, Species, 'Life form') %>% unique()
group_by(Species_breakdown, `Life form`) %>% summarize(count =n()) %>% arrange(desc(count)) %>%
  mutate(percentage = count/sum(count)*100)

#figure2
d$Habitat <- tolower(d$Habitat)
group_by(d, Habitat) %>% summarize(count = n()) %>% arrange(desc(count)) %>%
  mutate(percentage = count/sum(count)*100)

d$Species <- tolower(d$Species)
select(d, Species, Habitat) %>% unique() %>%
group_by(Habitat) %>% summarize(count = n()) %>% arrange(desc(count)) %>%
  mutate(percentage =count/sum(count)*100)

##figure3
colnames(d) 
v <- select(d, Species, elevation)
  summary(v)
  v
breaks <- c(300, 500, 700, 900, 1100, 1300, 1500, 1700, 1900, 2100, 2300, 2500, 2700, 2900, 3100, 3300, 3500, 3700, 3900, 4100, 4300)
tags <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
group_tags <- cut(v$elevation,
                  breaks = breaks,
                  include.lowest = TRUE,
                  right = FALSE,
                  labels = tags)
summary(group_tags)
