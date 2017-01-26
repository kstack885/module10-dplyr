#Use "dplyr"
#Install.packages("dplyr")



#Load in SwissData from data set from data folder and view it to understand what is in it.
swiss <- read.csv("data/SwissData.csv", stringsAsFactors = FALSE)
View(swiss)
#Add a column (using dpylr) that is the absolute difference between Education and Examination and call it
# Educated.Score
swiss <- mutate(swiss, Educated.Score = abs(Education - Examination))
View(swiss)

#Which area(s) had the largest difference
largest.diff <- filter(swiss, max(Educated.Score) == Educated.Score) %>% 
  select(Region)
print(largest.diff)

#Find which region has the highest percent of men in agriculture and retunr only the
#percent and region name.  Use pipe operators to accomplish this.

highest.perc.men <- filter(swiss, max(Agriculture) == Agriculture) %>% 
  select(Region, Agriculture)
print(highest.perc.men)

#Find the average of all infant.mortality rates and create a column (Mortality.Difference)
# showing the difference between a regions mortality rate and the mean. Arrange the dataframe in
# Descending order based on this new column. Use pipe operators.
swiss <- mutate(swiss, Mortality.Difference = Infant.Mortality - mean(Infant.Mortality)) %>% 
  arrange(-Mortality.Difference)
View(swiss)

# Create a new data frame that only is that of regions that have a Infant mortality rate less than the
# mean.  Have this data frame only have the regions name, education and mortality rate.
best.mortality <- filter(swiss, Infant.Mortality < mean(Infant.Mortality)) %>% 
  select(Region, Education, Infant.Mortality)
print(best.mortality)
#Filter one of the columns based on a question that you may have (which regions have a higher
#education rate, etc.) and write that to a csv file
# Question: Which regions have highest education rate? Which regions have the worst education rate
worst.education <- filter(best.mortality, Education == min(Education)) %>% 
  select(Region, Education, Infant.Mortality)
print(worst.education)
write.csv(worst.education, file = "data/WorstEducation.csv")

# education of top 5 best mortality rates, df of top 5

ed.of.top.mort <- arrange(best.mortality, -Infant.Mortality) %>% 
   head(5)
   
print(ed.of.top.mort)
# Create a function that can take in two different region names and compare them based on a statistic
# Of your choice (education, Examination, ect.)  print out a statment describing which one is greater
# and return a data frame that holds the selected region and the compared variable.  If your feeling adventurous
# also have your function write to a csv file.
CompareCatholics <- function(region.1, region.2) {
  # Which is more Catholic
  # select --> select(swiss, Region, Catholic)
  comparing <- select(swiss, Region, Catholic) %>% 
    filter(Region == region.1 | Region == region.2) %>% 
    filter(max(Catholic) == Catholic)
  write.csv(comparing, file = "data/CompareCatholics.csv")
  print(paste(select(comparing, Region), "has a greater proportion of Catholics."))
  return(comparing)
      
}
CompareCatholics("Sarine", "Broye")
