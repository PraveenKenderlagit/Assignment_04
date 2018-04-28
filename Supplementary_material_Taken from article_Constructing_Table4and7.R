
# Assignment_06

library(tidyverse)
library(ggplot2)

#  1)   12.6.1 : problems 3 and 4.
who1 <- who %>%  
  gather (new_sp_m014:newrel_f65, key = "key", value = "cases", na.rm = T)

who1 %>%  count(key)

who2 <- who1 %>%  mutate(key = stringr::str_replace(key, "newrel", "new_rel"))

who3 <- who2 %>%  
  separate(key, c("new", "type", "sexage"), sep = "_")

who3 %>%  count(new)

who4 <- who3 %>%  
  select(-new, -iso2, -iso3)

who5 <- who4 %>%  
  separate(sexage, c("sex", "age"), sep = 1)

AA <- who %>%
  gather(code, value, new_sp_m014:newrel_f65, na.rm = TRUE) %>% 
  mutate(code = stringr::str_replace(code, "newrel", "new_rel")) %>%
  separate(code, c("new", "var", "sexage")) %>% 
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1)



# 2)   10.5 : problem 5

?tibble::enframe()
# enframe(x, name = "name", value = "value")
# enframes coverts the vectors or lists into 2 column data frames
# it can also takes the input arguments name and value to change the name of columns

# It is useful to combine the vector values into a column matrix.


# Use dplyr and tidyr to reproduce

# 3)   table 4 -> table 6

library(foreign)
library(stringr)
library(plyr)
library(reshape2)
source("xtable.r")
library(tidyverse)

pew <- read.spss("pew.sav")
pew <- as.data.frame(pew)


religion <- pew[c("q16", "reltrad", "income")]
religion$reltrad <- as.character(religion$reltrad)
religion$reltrad <- str_replace(religion$reltrad, " Churches", "")
religion$reltrad <- str_replace(religion$reltrad, " Protestant", " Prot")
religion$reltrad[religion$q16 == " Atheist (do not believe in God) "] <- "Atheist"
religion$reltrad[religion$q16 == " Agnostic (not sure if there is a God) "] <- "Agnostic"
religion$reltrad <- str_trim(religion$reltrad)
religion$reltrad <- str_replace_all(religion$reltrad, " \\(.*?\\)", "")

religion$income <- c("Less than $10,000" = "<$10k", 
                     "10 to under $20,000" = "$10-20k", 
                     "20 to under $30,000" = "$20-30k", 
                     "30 to under $40,000" = "$30-40k", 
                     "40 to under $50,000" = "$40-50k", 
                     "50 to under $75,000" = "$50-75k",
                     "75 to under $100,000" = "$75-100k", 
                     "100 to under $150,000" = "$100-150k", 
                     "$150,000 or more" = ">150k", 
                     "Don't know/Refused (VOL)" = "Don't know/refused")[religion$income]



religion$income <- factor(religion$income, levels = c("<$10k", "$10-20k", "$20-30k", "$30-40k", "$40-50k", "$50-75k", 
                                                      "$75-100k", "$100-150k", ">150k", "Don't know/refused"))

counts <- count(religion, c("reltrad", "income"))
names(counts)[1] <- "religion"

xtable(counts[1:10, ], file = "pew-clean.tex")

# Convert into the form in which I originally saw it -------------------------

raw <- dcast(counts, religion ~ income)

Table4 = as.tibble(raw)

write.csv(Table4, file = "Table4.csv", header = TRUE)

Table6 <- Table4 %>%  gather (`<$10k`, `$10-20k`, `$20-30k`,`$30-40k`,`$40-50k`,`$50-75k`,`$75-100k`,`$100-150k`,`>150k`, `Don't know/refused`, key = "Income", value = "freq")

Table6 <- Table4 %>% gather (`<$10k`:`Don't know/refused`, key = "Income", value = "Freq", na.rm = T)

Table6


# Table7 to Table8; 


options(stringsAsFactors = FALSE)

raw <- read.csv("billboard.csv")
raw <- raw[, c("year", "artist.inverted", "track", "time", "date.entered", "x1st.week", "x2nd.week", "x3rd.week", "x4th.week", "x5th.week", "x6th.week", "x7th.week", "x8th.week", "x9th.week", "x10th.week", "x11th.week", "x12th.week", "x13th.week", "x14th.week", "x15th.week", "x16th.week", "x17th.week", "x18th.week", "x19th.week", "x20th.week", "x21st.week", "x22nd.week", "x23rd.week", "x24th.week", "x25th.week", "x26th.week", "x27th.week", "x28th.week", "x29th.week", "x30th.week", "x31st.week", "x32nd.week", "x33rd.week", "x34th.week", "x35th.week", "x36th.week", "x37th.week", "x38th.week", "x39th.week", "x40th.week", "x41st.week", "x42nd.week", "x43rd.week", "x44th.week", "x45th.week", "x46th.week", "x47th.week", "x48th.week", "x49th.week", "x50th.week", "x51st.week", "x52nd.week", "x53rd.week", "x54th.week", "x55th.week", "x56th.week", "x57th.week", "x58th.week", "x59th.week", "x60th.week", "x61st.week", "x62nd.week", "x63rd.week", "x64th.week", "x65th.week", "x66th.week", "x67th.week", "x68th.week", "x69th.week", "x70th.week", "x71st.week", "x72nd.week", "x73rd.week", "x74th.week", "x75th.week", "x76th.week")]
names(raw)[2] <- "artist"

raw$artist <- iconv(raw$artist, "MAC", "ASCII//translit")
raw$track <- str_replace(raw$track, " \\(.*?\\)", "")
names(raw)[-(1:5)] <- str_c("wk", 1:76)
raw <- arrange(raw, year, artist, track)

# Question 4: 

Table7 <- as.tibble(raw) 
write.csv(Table7, file = "Table7.csv")
Table7 <- Table7 %>%  gather(wk1:wk76, key = "Week", value = "Freq", na.rm = T)
Table8 <- Table7 %>%  arrange(artist)
names(Table8)[names(Table8) == "date.entered"] <- "date"
Table8
