### 1. Clearing the environment in R
rm(list=ls(all=TRUE))

##2. Loading the dataset 
library(rio)
inequality_data = import("inequality.xlsx")

### 3. checking for panel vs cross sectional
head(inequality_data)

### 4. Providing the subset command for denmark and sweeden
subset(inequality_data, country == "Denmark"| country == "Sweden")

#### 5. Providing the score for brazil
subset(inequality_data, country == "Brazil")

### 7. Quick peek at the data frame
head(inequality_data)

# 8. writing a command to remove the accent
accent.remove <- function(x){
  ##for 1 character substitutions
  old1 <- "ú"
  new1 <- "u"
  ###use chartr to make the replacements
  s1 <- chartr(old1,new1,x)
}
##Running the command on the country column 
inequality_data$country=accent.remove(inequality_data$country)
###Checking the data 
head(inequality_data)

####9. sort the gini score
inequality_data=inequality_data[order(inequality_data$inequality_gini),]
      ## Check the head to see the top 5 countries
      head(inequality_data)

### 10. Mean gini  score
      mean(inequality_data$inequality_gini, na.rm=TRUE) ##Noticed there were NA's in the dataset

# 11. using if else to create dummy varables
##Creating the variable high inequality
inequality_data$high_inequality = "NA"
inequality_data$high_inequality =ifelse(test = inequality_data$inequality_gini > 36.81375, yes = 1, no = 0)

#Creating the variable low inequality

inequality_data$low_inequality = "NA"
inequality_data$low_inequality =ifelse(test = inequality_data$inequality_gini > 36.81375, yes = 0, no = 1)

# 12. run a crosstab
  # Load doby
  library(doBy)
  #Run the cross tab
  summaryBy(inequality_gini ~ high_inequality, data=inequality_data, FUN=c(mean,length))

# 13. run a for loop that prints names
  #Creating an organization vector 
  orgs <- c('World Bank','African Development Bank','Bill and Melinda Gates foundation')
 #Creating the loop to print the orgs
   for (i in orgs){
    print(i)
   }
# 14
# 15 import the variable
library(WDI)
poverty_data = WDI(country= "all",
                   indicator = c("SI.POV.DDAY"),
                   start = 2015, end = 2015, extra = FALSE, cache = NULL)

# 16 rename the variable
library(data.table)
setnames(poverty_data,"SI.POV.DDAY","Poverty_headcount_ratio")  
head(poverty_data)

##17. Merge 
## want to do a left join so X can keep all of its rows
library(tidyverse)
merged_df = left_join(x= inequality_data,
                      y= poverty_data,
                      by = c("iso2c","year") #used iso2c since both datasets had them
                      )
##Creating a variable that sees when names match and when they dont
merged_df <-
  merged_df %>%
  mutate(countries_match = ifelse(country.x == country.y,
                                  "yes",
                                  "no"))
#Check for countries that dont match
subset(merged_df, countries_match =="no")
### All countries match, so I can get rid of either column, choose to get rid of Y
merged_df <-
  merged_df %>%
  select(-c("country.y")) %>% # drop country.y
  rename("country" = "country.x")
## drop the countries match variable
merged_df$countries_match = NULL
## take a peak at the data to make sure its correct
head(merged_df)

###18. Remove NA's 
merged_df <- na.omit(merged_df, select = c("inequality_gini", "Poverty_headcount_ratio"))
### check for NA
is.na(merged_df$inequality_gini)
is.na(merged_df$Poverty_headcount_ratio)

### 19. filter and piping 
library(tidyverse)
data_greater_30 <-
  merged_df %>% 
  dplyr::filter((inequality_gini > 30)) ###Filter out any of these that have a gini score lower than 30
  # quick check to make sure they are greater than 30
  head(data_greater_30)
  
  
### 20. counting the number of "ai" in countries
length(grep("ai",data_greater_30$country))

### 21. use any command from the apply to take the sum of inequality_gini
sum(sapply(data_greater_30$inequality_gini, sum))
### 22. rename the variables
library(labelled)
var_label(merged_df) <- list(`country` = "Country",
                             `year` = "year",
                             `inequality_gini` = "Gini inequality score",
                             `high_inequality` = "Dummy Variable if the Gini score is higher than the mean",
                             `low_inequality` = "Dummy Variable if the Gini score is lower than the mean",
                             `iso2c`= "ISO-2 Country Code",
                             `Poverty_headcount_ratio` = "Poverty Headcount Ratio")

### 23, save merged df as a stata fle 
library(rio)
export(merged_df, file = "final_data.dta")
                             
