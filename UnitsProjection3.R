## Sales Prediction Based on Unit Types (Second Part)

# We will perform extra cleaning: Final renaming (translation) of column names
# Function: renames columns for all tables (2017 - 2024)

list_tables <- list(Units_Types_2017, Units_Types_2018, Units_Types_2019, Units_Types_2021, Units_Types_2022, Units_Types_2023,
                    Units_Types_2024)

replace_string <- function(data){
  colnames(data) <- str_replace_all(colnames(data), pattern = "SIROPE_DE_MANZANA", replacement = "APPLE_SYRUP")
  colnames(data) <- str_replace_all(colnames(data), pattern = "SIROPE_DE_FRAMBUESA", replacement = "RASPBERRY_SYRUP")
  colnames(data) <- str_replace_all(colnames(data), pattern = "SIROPE_DE_CEREZA", replacement = "CHERRY_SYRUP")
  colnames(data) <- str_replace_all(colnames(data), pattern = "MARAÑON", replacement = "CASHEW")
  colnames(data) <- str_replace_all(colnames(data), pattern = "LIMÓN_FRESA", replacement = "STRAWBERRY_LEMONADE")
  colnames(data) <- str_replace_all(colnames(data), pattern = "GRANITA_MORA", replacement = "BLACKBERRY_SLUSH")
  colnames(data) <- str_replace_all(colnames(data), pattern = "SIROPE_DE_PIÑA", replacement = "PINEAPPLE_SYRUP")
  colnames(data) <- str_replace_all(colnames(data), pattern = "SIROPE_DE_MORA", replacement = "BLACKBERRY_SYRUP")
  colnames(data) <- str_replace_all(colnames(data), pattern = "NARANJA", replacement = "ORANGE")
  colnames(data) <- str_replace_all(colnames(data), pattern = "MANGO", replacement = "MANGO")                   # same in both languages
  colnames(data) <- str_replace_all(colnames(data), pattern = "HORCHATA_MORRO", replacement = "HORCHATA_MORRO") # Again
  colnames(data) <- str_replace_all(colnames(data), pattern = "HORCHATA_MANI", replacement = "HORCHATA_PEANUTS")
  colnames(data) <- str_replace_all(colnames(data), pattern = "TÉ_NEGRO", replacement = "BLACK_TEA")
  colnames(data) <- str_replace_all(colnames(data), pattern = "TAMARINDO", replacement = "TAMARIND")
  colnames(data) <- str_replace_all(colnames(data), pattern = "ROSA_DE_JAMAICA_PIZZA", replacement = "JAMAICAN_ROSE_PIZZA")
  colnames(data) <- str_replace_all(colnames(data), pattern = "ROSA_DE_JAMAICA", replacement = "JAMAICAN_ROSE")
  colnames(data) <- str_replace_all(colnames(data), pattern = "PONCHE_DE_FRUTAS", replacement = "FRUIT_PUNCH")
  colnames(data) <- str_replace_all(colnames(data), pattern = "PIÑA", replacement = "PINEAPPLE")
  colnames(data) <- str_replace_all(colnames(data), pattern = "NARANJA_ZANAHORIA", replacement = "ORANGE_CARROT")
  colnames(data) <- str_replace_all(colnames(data), pattern = "NANCE", replacement = "NANCE")                   # Same 
  colnames(data) <- str_replace_all(colnames(data), pattern = "MORA", replacement = "BLACKBERRY") 
  colnames(data) <- str_replace_all(colnames(data), pattern = "MARACUYA", replacement = "PASSION_FRUIT") 
  colnames(data) <- str_replace_all(colnames(data), pattern = "LIMON_SIN_AZUCAR", replacement = "LEMONADE_NO_SUGAR")
  colnames(data) <- str_replace_all(colnames(data), pattern = "LIMON_CON_AZUCAR", replacement = "LEMONADE_WITH_SUGAR") 
  colnames(data) <- str_replace_all(colnames(data), pattern = "LIMON", replacement = "LEMONADE") 
  colnames(data) <- str_replace_all(colnames(data), pattern = "NANCE", replacement = "NANCE") 
  colnames(data) <- str_replace_all(colnames(data), pattern = "GUANABANA", replacement = "SOURSOP") 
  colnames(data) <- str_replace_all(colnames(data), pattern = "FRESA", replacement = "STRAWBERRY") 
  return(data)
}

mod_list_tables <- list_tables %>% sapply(replace_string) %>% lapply(data.frame, check.names=FALSE)

Units_Types_2017 <- mod_list_tables[[1]]
Units_Types_2018 <- mod_list_tables[[2]]
Units_Types_2019 <- mod_list_tables[[3]]
Units_Types_2021 <- mod_list_tables[[4]]
Units_Types_2022 <- mod_list_tables[[5]]
Units_Types_2023 <- mod_list_tables[[6]]
Units_Types_2024 <- mod_list_tables[[7]]



# Check for all the columns that are common across all tables

library(magrittr)
df_test <- as.data.frame(cbind(Units_Types_2024, Units_Types_2023, Units_Types_2022, Units_Types_2021, Units_Types_2019,
                               Units_Types_2018, Units_Types_2017)) %$% !duplicated(t(.))     
print(df_test)

# Noticed how I used the exposition pipe operator (%$%) instead of the normal one (%>%)
# Helps to use a function to which only applies to the columns of a dataframe and not the dataframe itself

# Brings column names which are not common across all tables (I used for loop to go over all the df_test)

for (i in 1:nrow(df_test)){
  if(df_test[[i]]==FALSE){
    print(df_test[i])
  }
  else{
    print("nothing")
  }
}


# Column names that are not in all tables:

# APPLE_SYRUP_GALLON, HORCHATA__1/2_GALLON, BLACK_TEA_1/2_GALLON, LEMONADE__1/2_GALLON, BLACKBERRY_1/2_GALLON, 
# JAMAICAN_ROSE_1/2_GALLON, NANCE_1/2_GALLON, TAMARIND_1/2_GALLON, FRUIT_PUNCH_1/2_GALLON, STRAWBERRY_LEMONADE_1/2_GALLON,
# PINTS_DE_LEMONADE__UNIDAD_1/2_WENDYS, ORANGE_GALLON, TAMARIND_GALLON, PINTS_DE_LEMONADE__UNIDAD_1/2_INTUR, PINEAPPLE_1/2_GALLON,
# PASSION_FRUIT_GALLON, LEMONADE_H.M._GALLON, BLACKBERRY_GALLON, JAMAICAN_ROSE_GALLON, PINEAPPLE_GALLON, ORANGE_ZANAHORIA_1/2_GALLON,
# SOURSOP_1/2_GALLON, FRUIT_PUNCH_GALLON, MANGO_1/2_GALLON, MANGO_GALLON, ORANGE_ZANAHORIA_GALLON, PINEAPPLE_COLADA_1/2_GALLON,
# SOURSOP_GALLON, STRAWBERRY_GALLON, ORANGE_1/2_GALLON, HORCHATA_PEANUTS_GALLON, NANCE_GALLON, CASHEW_1/2_GALLON, 
# PINEAPPLE_SYRUP_1/2_GALLON, BLACKBERRY_SYRUP_1/2_GALLON, CASHEW_GALLON, PINTS_LEMONADE_INTUR, PINTS_LEMONADE_WENDYS


# We will only visualize missing values in the dataset across tables to see which columns to remove, the list above will give a
# hint to which products to look for (to remove) but its not set in stone. 

# Non-duplicates column across all tables represented in a table:

df_t <- as.data.frame(cbind(Units_Types_2024, Units_Types_2023, Units_Types_2022, Units_Types_2021, Units_Types_2019,
                            Units_Types_2018, Units_Types_2017))
df_t <- df_t[, !duplicated(colnames(df_t))]
 
visdat::vis_miss(df_t) # Normally unique columns are the ones filled with missing values as they do not use
                       # their products for next year

# Target Products to remove on all tables: PINEAPPLE_SYRUP_1/2_GALLON, PINEAPPLE_COLADA_1/2_GALLON, CASHEW_1/2_GALLON,
# RASPBERRY_SYRUP_GALLON, APPLE_SYRUP_GALLON, CHERRY_SYRUP_GALLON, STRAWBERRY_LEMONADE_1/2_GALLON, BLACKBERRY_SLUSH_GALLON,
# MAIZ_BLACKBERRYDO_GALLON (Mispronounced but will removed anyway), SIROPE_DE_MANGO_1/2_GALLON, ARRAYAN_/_CAS_1/2_GALLON, 
# and GRANITA_MANGO_GALLON





# Now we will go table-by-table to drop columns in which they have a lot of missing values

# Take a perspective

visdat::vis_miss(Units_Types_2024) # Only for 2024


Unit_Types_Data <- dplyr::bind_rows(Units_Types_2024, Units_Types_2023, Units_Types_2022, Units_Types_2021, Units_Types_2019,
                 Units_Types_2018, Units_Types_2017)  # bind_rows bind datasets of different dimensions, a smarter counterpart
                                                      # to rbind (There are 64 products in total across all tables)

# Final Rote Translations
Unit_Types_Data <- Unit_Types_Data %>% select(-c(Months)) # Remove months (will help with calculations)
colnames(Unit_Types_Data) <- str_replace_all(colnames(Unit_Types_Data), 
                                             pattern = "PINTS_DE_LEMONADE__UNIDAD_1/2_INTUR", replacement = "LEMONADE_1/2_PINTS_INTUR")
colnames(Unit_Types_Data) <- str_replace_all(colnames(Unit_Types_Data), 
                                             pattern = "PINTS_DE_LEMONADE__UNIDAD_1/2_WENDYS", replacement = "LEMONADE_1/2_PINTS_WENDYS")
colnames(Unit_Types_Data) <- str_replace_all(colnames(Unit_Types_Data), 
                                             pattern = "ORANGE_ZANAHORIA_1/2_GALLON", replacement = "ORANGE_CARROT_1/2_GALLON")
colnames(Unit_Types_Data) <- str_replace_all(colnames(Unit_Types_Data), 
                                             pattern = "ORANGE_ZANAHORIA_GALLON", replacement = "ORANGE_CARROT_GALLON")



visdat::vis_miss(Unit_Types_Data) # For all datasets 

# Rule of thumb: If one column is missing 80% or more of the data, is better to drop it
# As long as there is overall 10% or less of missing data in the dataset is good (right now is 44.5%)

# We will drop all the necessary columns: 

Unit_Types_Data <- Unit_Types_Data %>% select_if(colSums(is.na(.))/nrow(.) < .60) # Returns dataset missing maps once
                                                                                  # columns (more than 60% missing) are removed

# I could have chosen the threshold around 80% however it would give me a data missingness of 30% which is too high
# Also, there are variables in which even though they have "sufficient" values (less than 80% missingness), they
# still are irrelevant to the whole dataset, they just have very low values to be of any significance. 


Unit_Types_Data %>% visdat::vis_miss()

Unit_Types_Data %>% naniar::gg_miss_var()                                                                         



# Apply MICE imputation: Data is mostly MAR (Missing at random) with some MNAR

# There is not a percentage value to accept or discard your variables. 
# The variance of your variable is what is important to watch before imputation of data.

skimr::skim(Unit_Types_Data) # Variables are mostly normally or right-skewed distributed
                             # Notice that the most sold over a good amount of years approximate a normal distribution
                             # by Central Limit Theorem (CLT), especially as each sample size is above 30 for non-NA values
                             # For example: JAMAICAN_ROSE_1/2_GALLON, FRUIT_PUNCH_1/2_GALLON, and TAMARIND_GALLON 

                             # By consequence, mean tends to get closer to its true value by Law of Large Numbers (LLN)

# We will first graph a scatter plot to see if there is a correlation between variables
# However, some (in fact, most), have NA values, so we will ignore them and assume there is a correlation between them.
# After applying MICE imputation, we will confirm that there is for once missing values columns solidifying the assumption


# Correlation Heat map Glimpse with Hierarchical Clustering

library(RColorBrewer)
Unit_Types_Data %>% select_if(colSums(is.na(.)) == 0) %$% heatmap(cor(.), col = colorRampPalette(brewer.pal(8, "Blues"))(25),
              scale = "column", main = "Correlation Map", cexRow=0.8, cexCol=0.5)  



# We use Predictive Mean Matching (PMM) as it is for robust semi-parametric data
# This method is very useful if you have a variable which needs imputing which has any of the following characteristics:

#Multimodal
#Integer
#Skewed

# Which when you plot Unit_Types_Data using skimr::skim you will see there are a lot of right-skewed and some multimodal 
# distribution


# Data Imputation

getwd() 
setwd("C:/Users/mattr/Downloads") # Change Working Directory to Downloads Path

write.csv(Unit_Types_Data, file="C:/Users/mattr/Downloads/Units_Types_Data.csv", row.names=FALSE)

# Perform MICE imputation in Python using sklearn using IterativeImputer 

Units_Types_Data_imp <- read_csv("C:/Users/mattr/Downloads/Units_Types_Data_imp.csv")
head(Units_Types_Data_imp, 20)

sign(as.data.frame(Units_Types_Data_imp))  # Check for negative values (There are not!)

# We have finally completed our data cleaning and preprocessing! Now lets jump to the statistical analysis

glimpse(Units_Types_Data_imp) # Remember: use the the imputed data with data type: dataframe

# Notice there are 3 extra columns that were added (probably I did not manage well the indexes in python), we will
# delete them

Units_Types_Data_imp <- Units_Types_Data_imp[, -c(1,2,3)] # Finally dataset is tidy! 

# Go to next file 
                                  