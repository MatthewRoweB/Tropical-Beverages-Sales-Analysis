## Sales Prediction Based on Units Types (First Part)

rm(list=ls())
graphics.off()

# Units_Types for 2024 Data Cleaning --------------------------------------------------------------------------------------------------------
# Its impossible to come up with a function that modifies every year to a neat representative table as every table has its own adjustments
# So we will clean it for every year 

# This code will generalize with a few modifications for each year (2017-2024)

library(tidyverse)
library(dplyr)
library(readxl)
library(magrittr)
Units_Types_2024 <- read_excel("C:/Users/mattr/Downloads/Unidades2017-2024.xlsx", sheet=2) 


# Produce Inverted Table with no gaps

Units_Types_2024 <- Units_Types_2024 %>% 
  as_tibble() %>%
  na.omit() %>%
  rename(c('Unit_Type' = 'TROPISABOR HONDURAS S DE R L', 'January' = '...2', 'February' = '...3', 'March' = '...4', 'April' = '...5',
           'May' = '...6', 'June' = '...7', 'July' = '...8', 'August' = '...9', 'September' = '...10', 'October' = '...11', 
           'November' = '...12', 'December' = '...13')) %>%
  select(-tail(names(.), 1)) %>%
  filter(Unit_Type != 'subcategoria') %>%
  t() 
  
colnames(Units_Types_2024) <- as.character(Units_Types_2024[1,])  # Place Beverage Drinks Names to header 
Units_Types_2024 <- Units_Types_2024[c(-1),]

Units_Types_2024[Units_Types_2024 == "0"] <- NA

# Check structure of data
glimpse(Units_Types_2024)
str(Units_Types_2024)
head(Units_Types_2024, n=Inf) # Notice how columns of the tibble are characters instead of numeric




Units_Types_2024 <- data.frame(Units_Types_2024, check.names=FALSE) %>% 
  mutate_if(is.character, as.numeric)  # Transforms columns variables from character to numeric

Units_Types_2024 <- tibble::rownames_to_column(as_tibble(Units_Types_2024), "Months") # Add extra column to indicate row names

grep("½", colnames(Units_Types_2024)) # Check for half Gallons and pints (The standard is 1 Gallon)
duplicated(t(Units_Types_2024))       # Check for duplicates

# Organize Column Names by: Translating them to English
# Notice: We will not convert numerical values to a standard measurement but we will change the column measurement's title. For example
# "HORCHATA ½ GALON" will be replaced as "HORCHATA 1/2 GALON" but their 
# values remain the same. Horchata is the same in English as Spanish and 1/2 is now the text notation. 



# Function to translate from Spanish to English for all tables 2017-2024.
# Input your year Unit_Type table and return English Translation for all columns
# Notice this function is not perfect and there will be some extra columns that will have to come up with an English translation on its own.


# Translates "GALON" to English "GALLON", "PINTAS" to "PINTS", removes round brackets, and concatenates with _ underscore
# This function will apply to any sales data year

columns_translate <- function(data){
  colnames(data) <- str_replace_all(colnames(data), pattern = "GALON", replacement = "GALLON")
  colnames(data) <- str_remove_all(colnames(data), "\\(|\\)") 
  colnames(data) <- str_replace_all(colnames(data), pattern = " ", replacement= "_")
  colnames(data) <- str_replace_all(colnames(data), pattern = "-", replacement= "_")
  colnames(data) <- str_replace_all(colnames(data), pattern = "PINTAS", replacement = "PINTS")
  colnames(data) <- str_replace_all(colnames(data), pattern = "½", replacement = "1/2")
  return(data)
}

Units_Types_2024 <- columns_translate(Units_Types_2024) # Final cleaned data for 2024

# This data is finally cleaned on R, however there are still a few simple modifications and joining tables we will do on SQL. 
# Notice this is only for 2024 data. We will have to clean the data separately for each year as there a few inconsistencies




# Units_Types for 2023 Data Cleaning --------------------------------------------------------------------------------------------------------

# Almost the same code will be used for the one in 2024, however, some modifications will be performed. For example, filter out total
# for the last few rows in the data as is entered differently

# Every comment from now on will indicate only the modifications made


Units_Types_2023 <- read_excel("C:/Users/mattr/Downloads/Unidades2017-2024.xlsx", sheet=3) 

# Produce Inverted Table with no gaps

Units_Types_2023 <- Units_Types_2023 %>% 
  as_tibble() %>%
  na.omit() %>%
  rename(c('Unit_Type' = 'TROPISABOR HONDURAS S DE R L', 'January' = '...2', 'February' = '...3', 'March' = '...4', 'April' = '...5',
           'May' = '...6', 'June' = '...7', 'July' = '...8', 'August' = '...9', 'September' = '...10', 'October' = '...11', 
           'November' = '...12', 'December' = '...13')) %>%
  select(-tail(names(.), 1)) %>%
  filter(Unit_Type != 'subcategoria' & Unit_Type != 'TOTAL') %>%    # <- Modification: Filters also for Total records as we do not need them
  t() 

colnames(Units_Types_2023) <- as.character(Units_Types_2023[1,]) 
Units_Types_2023 <- Units_Types_2023[c(-1),]
Units_Types_2023[Units_Types_2023 == "0"] <- NA

glimpse(Units_Types_2023)
str(Units_Types_2023)
head(Units_Types_2023, n=Inf) 

Units_Types_2023 <- data.frame(Units_Types_2023, check.names=FALSE) %>% mutate_if(is.character, as.numeric)  

Units_Types_2023 <- tibble::rownames_to_column(as_tibble(Units_Types_2023), "Months") 

Units_Types_2023 <- columns_translate(Units_Types_2023) # Final cleaned data for 2023





# Units_Types for 2022 Data Cleaning --------------------------------------------------------------------------------------------------------

Units_Types_2022 <- read_excel("C:/Users/mattr/Downloads/Unidades2017-2024.xlsx", sheet=4) 

Units_Types_2022 <- Units_Types_2022 %>% 
  as_tibble() %>%
  na.omit() %>%
  rename(c('Unit_Type' = 'TROPISABOR HONDURAS S DE R L', 'January' = '...2', 'February' = '...3', 'March' = '...4', 'April' = '...5',
           'May' = '...6', 'June' = '...7', 'July' = '...8', 'August' = '...9', 'September' = '...10', 'October' = '...11', 
           'November' = '...12', 'December' = '...13')) %>%
  select(-tail(names(.), 1)) %>%
  filter(Unit_Type != 'subcategoria' & Unit_Type != 'TOTAL') %>%
  t() 

colnames(Units_Types_2022) <- as.character(Units_Types_2022[1,])  
Units_Types_2022 <- Units_Types_2022[c(-1),]
Units_Types_2022[Units_Types_2022 == "0"] <- NA

# Check structure of data
str(Units_Types_2022)
head(Units_Types_2022, n=Inf) 

Units_Types_2022 <- data.frame(Units_Types_2022, check.names=FALSE) %>% mutate_if(is.character, as.numeric)  

Units_Types_2022 <- tibble::rownames_to_column(as_tibble(Units_Types_2022), "Months") 

Units_Types_2022 <- columns_translate(Units_Types_2023) # Final cleaned data for 2022




# Units_Types for 2021 Data Cleaning --------------------------------------------------------------------------------------------------------

Units_Types_2021 <- read_excel("C:/Users/mattr/Downloads/Unidades2017-2024.xlsx", sheet=5) 

Units_Types_2021 <- Units_Types_2021 %>% 
  as_tibble() %>%
  na.omit() %>%
  rename(c('Unit_Type' = 'TROPISABOR HONDURAS S DE R L', 'January' = '...2', 'February' = '...3', 'March' = '...4', 'April' = '...5',
           'May' = '...6', 'June' = '...7', 'July' = '...8', 'August' = '...9', 'September' = '...10', 'October' = '...11', 
           'November' = '...12', 'December' = '...13')) %>%
  select(-tail(names(.), 1)) %>%
  filter(Unit_Type != 'subcategoria' & Unit_Type != 'TOTAL') %>%
  t() 

colnames(Units_Types_2021) <- as.character(Units_Types_2021[1,])  
Units_Types_2021 <- Units_Types_2021[c(-1),]
Units_Types_2021[Units_Types_2021 == "0"] <- NA

# Check structure of data
str(Units_Types_2021)
head(Units_Types_2021, n=Inf) 

Units_Types_2021 <- data.frame(Units_Types_2021, check.names=FALSE) %>% mutate_if(is.character, as.numeric)  

Units_Types_2021 <- tibble::rownames_to_column(as_tibble(Units_Types_2021), "Months") 

Units_Types_2021 <- columns_translate(Units_Types_2021) # Final cleaned data for 2021


# Ignore Table for 2020


# Units_Types for 2019 Data Cleaning ---------------------------------------------------------------------------------------------------------

Units_Types_2019 <- read_excel("C:/Users/mattr/Downloads/Unidades2017-2024.xlsx", sheet=7) 

Units_Types_2019 <- Units_Types_2019 %>% 
  as_tibble() %>%
  na.omit() %>%
  rename(c('Unit_Type' = 'TROPISABOR HONDURAS S. DE R.L. DE C.V',     # Modification: Add periods and "DE C.V" for renaming columns
           
           'January' = '...2', 'February' = '...3', 'March' = '...4', 'April' = '...5',
           'May' = '...6', 'June' = '...7', 'July' = '...8', 'August' = '...9', 'September' = '...10', 'October' = '...11', 
           'November' = '...12', 'December' = '...13')) %>%
  select(-tail(names(.), 1)) %>%
  filter(Unit_Type != 'subcategoria' & Unit_Type != 'TOTAL') %>%
  t() 

colnames(Units_Types_2019) <- as.character(Units_Types_2019[1,])  
Units_Types_2019 <- Units_Types_2019[c(-1),]
Units_Types_2019[Units_Types_2019 == "0"] <- NA

# Check structure of data
str(Units_Types_2019)
head(Units_Types_2019, n=Inf) 

Units_Types_2019 <- data.frame(Units_Types_2019, check.names=FALSE) %>% mutate_if(is.character, as.numeric)  

Units_Types_2019 <- tibble::rownames_to_column(as_tibble(Units_Types_2019), "Months") 

Units_Types_2019 <- columns_translate(Units_Types_2019) # Final cleaned data for 2019




# Units_Types for 2018 Data Cleaning ---------------------------------------------------------------------------------------------------------


Units_Types_2018 <- read_excel("C:/Users/mattr/Downloads/Unidades2017-2024.xlsx", sheet=8) 

Units_Types_2018 <- Units_Types_2018 %>% 
  as_tibble() %>%
  na.omit() %>%
  rename(c('Unit_Type' = 'TROPISABOR HONDURAS S. DE R.L. DE C.V',   # Modification: Add periods and "DE C.V" for renaming columns
           
           'January' = '...2', 'February' = '...3', 'March' = '...4', 'April' = '...5',
           'May' = '...6', 'June' = '...7', 'July' = '...8', 'August' = '...9', 'September' = '...10', 'October' = '...11', 
           'November' = '...12', 'December' = '...13')) %>%
  select(-tail(names(.), 1)) %>%
  filter(Unit_Type != 'subcategoria' & Unit_Type != 'TOTAL') %>%
  t() 

colnames(Units_Types_2018) <- as.character(Units_Types_2018[1,])  
Units_Types_2018 <- Units_Types_2018[c(-1),]
Units_Types_2018[Units_Types_2018 == "0"] <- NA

# Check structure of data
str(Units_Types_2018)
head(Units_Types_2018, n=Inf) 

Units_Types_2018 <- data.frame(Units_Types_2018, check.names=FALSE) %>% mutate_if(is.character, as.numeric)  

Units_Types_2018 <- tibble::rownames_to_column(as_tibble(Units_Types_2018), "Months") 

Units_Types_2018 <- columns_translate(Units_Types_2018) # Final cleaned data for 2018




# Units_Types for 2017 Data Cleaning ---------------------------------------------------------------------------------------------------------

Units_Types_2017 <- read_excel("C:/Users/mattr/Downloads/Unidades2017-2024.xlsx", sheet=9) 

Units_Types_2017 <- Units_Types_2017 %>% 
  as_tibble() %>%
  na.omit() %>%
  rename(c('Unit_Type' = 'TROPISABOR HONDURAS S. DE R.L. DE C.V',   # Modification: Add periods and "DE C.V" for renaming columns
           
           'January' = '...2', 'February' = '...3', 'March' = '...4', 'April' = '...5',
           'May' = '...6', 'June' = '...7', 'July' = '...8', 'August' = '...9', 'September' = '...10', 'October' = '...11', 
           'November' = '...12', 'December' = '...13')) %>%
  select(-tail(names(.), 1)) %>%
  filter(Unit_Type != 'subcategoria' & Unit_Type != 'TOTAL') %>%
  t() 

colnames(Units_Types_2017) <- as.character(Units_Types_2017[1,])  
Units_Types_2017 <- Units_Types_2017[c(-1),]
Units_Types_2017[Units_Types_2017 == "0"] <- NA

# Check structure of data
str(Units_Types_2017)
head(Units_Types_2017, n=Inf) 

Units_Types_2017 <- data.frame(Units_Types_2017, check.names=FALSE) %>% mutate_if(is.character, as.numeric)  

Units_Types_2017 <- tibble::rownames_to_column(as_tibble(Units_Types_2017), "Months") 

Units_Types_2017 <- columns_translate(Units_Types_2017) # Final cleaned data for 2018

# We will now get rid of variables that have a great percentage of missing data. Notice that a variable can have a lot of missing data
# in one year table but not in another






