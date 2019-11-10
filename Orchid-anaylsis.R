library(readr)
library(readxl)
library(tidytext)
library(tidyverse)

f <- file.choose()
d <- read_excel(f)
head(d)