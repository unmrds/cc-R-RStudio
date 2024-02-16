zip_url <- "https://github.com/unmrds/cc-R-RStudio/raw/master/cc-R-Names/data.zip"
getwd()
download.file(zip_url, 'data.zip')
unzip('data.zip', exdir = 'cc-R-Names')

library(readr)
library(dplyr)
library(ggplot2)

# read in the 2010 and 2011 datasets 
names2010 <- read_csv("./data/2010")
names2011 <- read_csv("./data/2011")

# Add a column for the year before merging
names2010$year <- '2010'
names2011$year <- '2011'

# Combine the two enhanced dataframes into a single df
long_names <- rbind(names2010, names2011)

# read in additional years of data and combine them with the original long dataset
names2012 = read_csv("./data/2012")
names2012$year <- '2012'

names2013 = read_csv("./data/2013") 
names2013$year <- '2013'

names2014 = read_csv("./data/2014")
names2014$year <-  '2014'

names2015 = read_csv("./data/2015")
names2015$year <- '2015'

six_years = rbind(long_names, names2012, names2013, names2014, names2015)
str(six_years)

# most popular boy and girl names from 2010-2015
most_popular <- six_years %>% 
  group_by(name, sex) %>% 
  mutate(total = sum(count)) %>% 
  select(name, sex, total) %>% 
  # We can regroup our subset!
  group_by(sex) %>% 
  summarise(popular_name = name[which.max(total)], t = max(total))
most_popular

# how many babies have your name?
my_name <- six_years %>% 
  group_by(name) %>% 
  filter(name == "Jonathan" & sex == 'M') %>% 
  summarise(num_babies = sum(count))
my_name

# rank and proportion of your name?
boys <- six_years %>% 
  filter(sex == "M")
gt <- sum(boys$count)
rank_name <- boys %>%
  group_by(name) %>% 
  summarise(total = sum(count)) %>% 
  arrange(desc(total)) %>% 
  mutate(rank = 1:n(), proportion = round((total/gt)*100, 3)) %>% # from 1 to n()
  filter(name == "Jonathan")
rank_name


# plotting

# First, create a subset
sophia <- six_years %>% 
  filter(sex == "F" & name == "Sophia")
sophia

# now plot
sophia_plot <- ggplot(sophia, aes(year,count))
sophia_plot + geom_point()

# First, create a subset
names_compare <- six_years %>% 
  filter(sex == "F" & (name == "Sophia" | name == "Isabella"))
names_compare

# now plot
names_plot <- ggplot(names_compare, aes(year,count))
names_plot + geom_point(aes(color=name))

# First, create a subset
names_compare <- six_years %>% 
  filter(sex == "F" & (name == "Sophia" | name == "Isabella"))
names_compare

# now plot
names_plot <- ggplot(names_compare, aes(year, count, group=name))
names_plot + geom_line(aes(color=name)) + geom_point(aes(color=name))

# First, create a subset
names_compare <- six_years %>% 
  filter(sex == "F" & (name == "Sophia" | name == "Isabella"))
names_compare

# now plot
names_plot <- ggplot(names_compare, aes(year, count, group=name))
names_plot + geom_line(aes(color=name)) + geom_point(aes(color=name)) +
  labs(x = "Year", y = "Count", title = "Name Comparison: Sophia and Isabella")

# First, create a subset
names_compare <- six_years %>% 
  filter(sex == "F" & (name == "Sophia" | name == "Isabella"))
names_compare

# now plot
# other themes: light, minimal, classic
names_plot <- ggplot(names_compare, aes(year, count, group=name))
names_plot + geom_line(aes(color=name)) + geom_point(aes(color=name)) +
  labs(x = "Year", y = "Count", title = "Name Comparison: Sophia and Isabella") +
  theme_light()


# First, create a subset
names_compare <- six_years %>% 
  filter(sex == "M" & (name == "Jacob" | name == "Jonathan" | name == "Karl"))
names_compare

# now plot
# other themes: light, minimal, classic
names_plot <- ggplot(names_compare, aes(year, count, group=name))
names_plot + geom_line(aes(color=name)) + geom_point(aes(color=name)) +
  labs(x = "Year", y = "Count", title = "Name Comparison") +
  theme_light()