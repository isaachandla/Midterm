library(readr)
library(readxl)
library(tidytext)
library(tidyverse)
library(dplyr)


f <- file.choose()
f <- "Midterm data.xlsx"
d <- read_excel(f)
head(d)

#figure1
d$`Life form`<- tolower(d$`Life form`)
Specimen_breakdown <- group_by(d, `Life form`) %>% summarize(count = n()) %>% arrange(desc(count)) %>%
  mutate(percentage = count/sum(count)*100)
Specimen_breakdown #gives statistical analysis of Life form by specimen


#run here
names <- Specimen_breakdown$`Life form`
counts <- Specimen_breakdown$count
barplot(counts, names.arg = names, main = "# of Specimens by Life form")


d$Species <- tolower(d$Species)
Species_breakdown <- select(d, Species, 'Life form') %>% unique() %>% 
  group_by(`Life form`) %>% summarize(count =n()) %>% arrange(desc(count)) %>%
  mutate(percentage = count/sum(count)*100) #give statistical analysis of Life form by species
Species_breakdown

#runhere
names <- Species_breakdown$`Life form`
counts <- Species_breakdown$count
barplot(counts, names.arg = names, main = "# of Species by Life form" )

#figure2
d$Habitat <- tolower(d$Habitat)
Habitat_specimen <- group_by(d, Habitat) %>% summarize(count = n()) %>% arrange(desc(count)) %>%
  mutate(percentage = count/sum(count)*100)
Habitat_specimen

names <- Habitat_specimen$Habitat
counts <- Habitat_specimen$count
barplot(counts, names.arg = names, las = 2) 

d$Species <- tolower(d$Species)
Habitat_species <- select(d, Species, Habitat) %>% unique() %>%
group_by(Habitat) %>% summarize(count = n()) %>% arrange(desc(count)) %>%
  mutate(percentage = count/sum(count)*100)

names <- Habitat_species$Habitat
counts <- Habitat_species$count
barplot(counts, names.arg = names, las = 2)


##figure3a
colnames(d)
v <- select(d, Species, elevation, 'Life form')
  summary(v)
v
head(v)

breaks <- c(0, 300, 500, 700, 900, 1100, 1300, 1500, 1700, 1900, 2100, 2300, 2500, 2700,
            2900, 3100, 3300, 3500, 3700, 3900, 4100, 4300, 4500)
tags <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)

group_tags <- cut(v$elevation,
                  breaks = breaks,
                  include.lowest = TRUE,
                  right = FALSE,
                  labels = tags)

v<- cbind(v,group_tags)

v <- cbind(v, genus <- sapply(str_split(v$Species," "), `[`, 1))
names(v)<- c("Species","Elevation","Life Form","Elevation Bin","Genus")

head(v)

fig3aT <- group_by(v, `Elevation Bin`) %>% summarize(count=n())
names(fig3aT) <- c("Bin", "Count")
fig3aS <- group_by(v, `Elevation Bin`,Species) %>% summarize() %>% group_by(`Elevation Bin`) %>% summarize(count=n())
names(fig3aS) <- c("Bin", "Count")
fig3aC <- group_by(v, `Elevation Bin`,Genus) %>% summarize() %>% group_by(`Elevation Bin`) %>% summarize(count=n())
names(fig3aC) <- c("Bin", "Count")

x <- fig3aT$Bin
yT <- fig3aT$Count
xT <- as.numeric(levels(x))[x]

x <- fig3aS$Bin
yS <- fig3aS$Count
xS <- as.numeric(levels(x))[x]

x <- fig3aC$Bin
yC <- fig3aC$Count
xC <- as.numeric(levels(x))[x]

fitT <- lm(yT ~ xT + I(xT^2) + I(xT^3))
fitS <- lm(yS ~ xS + I(xS^2) + I(xS^3))
fitC <- lm(yC ~ xC + I(xC^2) + I(xC^3))

plot(fig3aT, main = "Species Richness", )
points(xT, fitted(fitT), pch = 20, col = 'red')
quadraticT <- lines(sort(xT), fitted(fitT)[order(xT)], col='red', type='b')

points(xS, fitted(fitS), pch = 20, col = 'blue')
quadraticS <- lines(sort(xS), fitted(fitS)[order(xS)], col='blue', type='b')

points(xC, fitted(fitC), pch = 20, col = 'black')
quadraticC <- lines(sort(xC), fitted(fitC)[order(xC)], col='black', type='b')

summary(fitT)

summary(fitS) 

summary(fitC) 

#figure3b

fig3bT <- group_by(v, `Elevation Bin`,`Life Form`, Species) %>% summarize() %>% filter(`Life Form` == "epiphyte") %>%
  group_by(`Elevation Bin`) %>% summarize(count=n())
names(fig3bT) <- c("Bin", "Count")
fig3bS <- group_by(v, `Elevation Bin`,`Life Form`, Species) %>% summarize() %>% filter(`Life Form` == "terrestrial") %>%
  group_by(`Elevation Bin`) %>% summarize(count=n())
names(fig3bS) <- c("Bin", "Count")
fig3bC <- group_by(v, `Elevation Bin`,`Life Form`, Species) %>% summarize() %>% filter(`Life Form` == "saprophyte") %>%
  group_by(`Elevation Bin`) %>% summarize(count=n())
names(fig3bC) <- c("Bin", "Count")


x <- fig3bT$Bin
yT <- fig3bT$Count
xT <- as.numeric(levels(x))[x]

x <- fig3bS$Bin
yS <- fig3bS$Count
xS <- as.numeric(levels(x))[x]

x <- fig3bC$Bin
yC <- fig3bC$Count
xC <- as.numeric(levels(x))[x]

fitT <- lm(yT ~ xT + I(xT^2) + I(xT^3))
fitS <- lm(yS ~ xS + I(xS^2) + I(xS^3))
fitC <- lm(yC ~ xC + I(xC^2) + I(xC^3))


plot(fig3bT, main = "Species Richness - Life form")
quadraticT <- lines(sort(xT), fitted(fitT)[order(xT)], col='red', type='b')

points(xS, fitted(fitS), pch = 20, col = 'blue')
quadraticS <- lines(sort(xS), fitted(fitS)[order(xS)], col='blue', type='b')

points(xC, fitted(fitC), pch = 20, col = 'black')
quadraticC <- lines(sort(xC), fitted(fitC)[order(xC)], col='black', type='b')

summary(fitT)

summary(fitS) 

summary(fitC) 



