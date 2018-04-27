
library(tidyverse)
library(readr)
library(stringr)

# import bib table and filter for only monographs

validation_bib <- read_delim("data/validation_rawdata.BIB.list", 
                                     "\t", escape_double = FALSE, col_names = FALSE, 
                                     trim_ws = TRUE)

validation_bib$X2 <- NULL
names(validation_bib)[1] <- "bib_id"
names(validation_bib)[2] <- "leader"

df_bib_monograph <- validation_bib %>% 
  mutate(c07 = str_sub(leader, 8, 8)) %>%
  filter(c07 == "m") %>%
  select(bib_id)


# import hld table and filter for only included locations


validation_hld <- read_delim("data/validation_holdings.MFHD.list", 
                                       "\t", escape_double = FALSE, col_names = FALSE, 
                                       trim_ws = TRUE)

names(validation_hld)[1] <- "hld_id"
names(validation_hld)[2] <- "bib_id"
names(validation_hld)[3] <- "f852"

location_list <- c("afr","afr,ref","hote","hote,ref","ilr","ilr,ref","jgsm","law","law,ref","mann","mann,ref","math","math,ref","phys","mus","mus,ref","asia","asia,ref","ech","ech,ref","olin","olin,ref","sasa","sasa,ref","was","was,ref","orni","uris","uris,ref","vet")

population <- validation_hld
population <- population %>%
  mutate(location = str_extract(f852, "\\$b(.+?)\\$") ) %>%
  mutate(location = str_sub(location, 3, -2))

df <- population %>%
  filter(location %in% location_list)

sum_df <- df %>%
  count(location, sort = TRUE) %>%
  mutate(population_total = nrow(df), pct_of_total_population = round(n / population_total,4) * 100   )

sample_6000 <- df %>%
  sample_n(6000, replace = FALSE)

sum_sample6000 <- sample_6000 %>%
  count(location, sort = TRUE) %>%
  mutate(total_sample = nrow(sample_6000), pct_of_total_sample = round(n / total_sample,4) * 100   )

df_joined <- left_join(sum_df, sum_sample6000, by = "location")

write_csv(df_joined, path = "output/comparison_pop_sample.csv")
