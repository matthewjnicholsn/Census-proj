library(tidyr)
library(dplyr)
library(ggplot2)



data <- c("datatable2011.csv", "datatable2021.csv")

for(i in seq_along(data)) {
  # get year from filename
  year <- substr(data[i], 10, 13)  
  
  # read file
  df <- read.csv(data[i])
  
  # rename vars
  if (year == "2011") {
    df <- df %>% 
      rename(gender = Sex..3.) %>% 
      rename(birthplace = Place.of.birth) %>% 
      rename(labour = Selected.charact) %>% 
      rename(counts = Canada) %>% 
      rename(education = Highest.certif) %>% 
      mutate(year = as.numeric(year)) %>% 
      mutate(counts = as.numeric(counts))
  } else if (year == "2021") {
    df <- df %>% 
      rename(gender = Gender..3.) %>%  # Different naming for 2021
      rename(birthplace = Place.of.Birth) %>% 
      rename(labour = Selected.charact) %>% 
      rename(counts = Canada.20000....4.3..) %>% 
      rename(education = Highest.certific) %>% 
      mutate(year = as.numeric(year)) %>% 
      mutate(counts = as.numeric(counts))
  }
  
  # give df a year
  assign(paste0("df_", year), df)
}

df_comb <- bind_rows(df_2011, df_2021)
