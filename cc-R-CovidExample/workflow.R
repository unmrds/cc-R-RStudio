## -----------------------------------------------------------------------------
knitr::opts_chunk$set(message = FALSE)
options("scipen"=100, "digits"=4) # tune up when numbers will be displayed in fixed vs. scientific notation
library(tidyverse)    # core meta-package for a bunch of the tidyverse packages
library(readxl)       # read XLS files into a datafram
library(lubridate)    # convenience functions for processing dates
library(ggthemes)     # add some great themes to use in our plots
library(RColorBrewer) # add the color brewer color palette
library(knitr)        # combined with kableExtra output tibbles as nicely formatted tables  
library(kableExtra)   # combined with kableExtra output tibbles as nicely formatted tables


## -----------------------------------------------------------------------------
# relative path and filename for the csv file to be imported
acs5_filepath <- "data/ACS2018/ACSST5Y2018.S0101_data_with_overlays_2020-04-06T234438.csv"
acs5_raw <- read_csv(acs5_filepath, col_names = TRUE, skip = 1)


## -----------------------------------------------------------------------------
glimpse(acs5_raw)


## -----------------------------------------------------------------------------
problems(acs5_raw)


## -----------------------------------------------------------------------------
# relative path and filename for the xls file to be imported
lad_filepath <- "data/LandArea/LND01.xls"
lad_raw <- read_excel(lad_filepath)


## -----------------------------------------------------------------------------
glimpse(lad_raw)


## -----------------------------------------------------------------------------
problems(lad_raw)


## -----------------------------------------------------------------------------
# relative path and filename for the xls file to be imported
c19_filepath <- "https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
c19_raw <- read_csv(c19_filepath)


## -----------------------------------------------------------------------------
glimpse(c19_raw)


## -----------------------------------------------------------------------------
problems(c19_raw)


## -----------------------------------------------------------------------------
acs5_working <- acs5_raw %>% 
  mutate(
    st_county = str_sub(id, -5)
  ) %>% 
  select(
    id = id,
    st_county,
    area_name = `Geographic Area Name`,
    pop_total = `Estimate!!Total!!Total population`,
    pop_lt5 =   `Estimate!!Total!!Total population!!AGE!!Under 5 years`,
    pop_5_9 =   `Estimate!!Total!!Total population!!AGE!!5 to 9 years`,
    pop_10_14 = `Estimate!!Total!!Total population!!AGE!!10 to 14 years`,
    pop_15_19 = `Estimate!!Total!!Total population!!AGE!!15 to 19 years`,
    pop_20_24 = `Estimate!!Total!!Total population!!AGE!!20 to 24 years`,
    pop_25_29 = `Estimate!!Total!!Total population!!AGE!!25 to 29 years`,
    pop_30_34 = `Estimate!!Total!!Total population!!AGE!!30 to 34 years`,
    pop_35_39 = `Estimate!!Total!!Total population!!AGE!!35 to 39 years`,
    pop_40_44 = `Estimate!!Total!!Total population!!AGE!!40 to 44 years`,
    pop_45_49 = `Estimate!!Total!!Total population!!AGE!!45 to 49 years`,
    pop_50_54 = `Estimate!!Total!!Total population!!AGE!!50 to 54 years`,
    pop_55_59 = `Estimate!!Total!!Total population!!AGE!!55 to 59 years`,
    pop_60_64 = `Estimate!!Total!!Total population!!AGE!!60 to 64 years`,
    pop_65_69 = `Estimate!!Total!!Total population!!AGE!!65 to 69 years`,
    pop_70_74 = `Estimate!!Total!!Total population!!AGE!!70 to 74 years`,
    pop_75_79 = `Estimate!!Total!!Total population!!AGE!!75 to 79 years`,
    pop_80_84 = `Estimate!!Total!!Total population!!AGE!!80 to 84 years`,
    pop_gt84 =  `Estimate!!Total!!Total population!!AGE!!85 years and over`
  ) %>% 
  mutate(
    pop_lt20 = pop_lt5 + pop_5_9 + pop_10_14 + pop_15_19,
    pop_gte65 = pop_65_69 + pop_70_74 + pop_75_79 + pop_80_84 + pop_gt84,
    pct_lt20 = pop_lt20/pop_total,
    pct_gte65 = pop_gte65/pop_total
  )
glimpse(acs5_working)


## -----------------------------------------------------------------------------
lad_working <- lad_raw %>% 
  select(
    st_county = STCOU,
    area_name = Areaname,
    land_area_sqmi = LND110210D
  )
glimpse(lad_working)


## -----------------------------------------------------------------------------
state_fips <- lad_raw %>% 
  filter(str_sub(STCOU, -3) == "000") %>% 
  mutate(
    st_fips = str_sub(STCOU, 1, 2)
  ) %>% 
  select(
    st_fips,
    Areaname
  )
glimpse(state_fips)


## -----------------------------------------------------------------------------
c19_working_wide <- c19_raw %>% 
  mutate(
    st_county = str_sub(UID, -5)
  ) %>% 
  select(
    -c(
      iso2, 
      iso3, 
      code3, 
      FIPS, 
      Admin2, 
      Province_State, 
      Country_Region, 
      Lat, 
      Long_
      ),
    st_county
  )
glimpse(c19_working_wide)


## -----------------------------------------------------------------------------
c19_working_tall <- c19_working_wide %>% 
  pivot_longer(
    -c(UID, st_county, Combined_Key),
    names_to = "caldate",
    values_to = "count"
  )
glimpse(c19_working_tall)


## -----------------------------------------------------------------------------
pop_density <- lad_working %>% 
  inner_join(acs5_working, by = "st_county") %>% 
  mutate(
    den_total = pop_total/land_area_sqmi,
    den_lt20 = pop_lt20/land_area_sqmi,
    den_gte65 = pop_gte65/land_area_sqmi
  ) %>% 
  select(
    -starts_with("pop"),
    -starts_with("pct"),
    -starts_with("area"), 
    -id,
    pop_total,
    pop_lt20,
    pop_gte65
  )
glimpse(pop_density)


## -----------------------------------------------------------------------------
master_ts <- c19_working_tall %>% 
  mutate(
    caldate = mdy(caldate),
    day_num = as.numeric(caldate - min(caldate))
  ) %>% 
  group_by(caldate, day_num) %>% 
  summarize(
    ct = sum(count)
  ) %>% 
  mutate(
    ln_ct = log1p(ct)
  ) %>% 
  filter(ct > 0)

glimpse(master_ts)

master_plot <- ggplot(master_ts, mapping = aes(x = day_num, y = ct)) + 
  geom_line() +
  xlab("Number of Days Since First Confirmed Infection") +
  ylab("Confirmed Infection Count")

master_plot + ggtitle("US - Number of confirmed cases")

master_plot + ggtitle("US - Number of confirmed cases") + geom_smooth(method="lm")

master_plot + ggtitle("US - Number of confirmed cases (log scale)") + scale_y_continuous(trans='log2')

ln_plot <- ggplot(master_ts, mapping = aes(x = day_num, y = ln_ct)) + 
  geom_line() +
  xlab("Number of Days") +
  ylab("Confirmed Infection Count (ln)")

ln_plot + ggtitle("US - Number of confirmed cases (ln)")
ln_plot + ggtitle("US - Number of confirmed cases (ln)") + geom_smooth(method="lm")




## -----------------------------------------------------------------------------
# calculate the start date for confirmed infections within the state (based on first confirmed value within a county within the state)
state_start <- c19_working_tall %>% 
  filter(str_length(st_county) == 5 & count > 0) %>% 
  mutate(
    st_fips = str_sub(st_county, 1, 2),
    caldate = mdy(caldate)
  ) %>% 
  inner_join(state_fips) %>% 
  group_by(Areaname) %>% 
  summarize(
    start_date = min(caldate)
  )
glimpse(state_start)

# build the datafram from which plots will be generated
state_ts <- c19_working_tall %>% 
  filter(str_length(st_county) == 5 & count > 0) %>% 
  mutate(
    st_fips = str_sub(st_county, 1, 2),
    caldate = mdy(caldate)
  ) %>% 
  inner_join(state_fips) %>%
  inner_join(pop_density) %>% 
  group_by(Areaname, caldate) %>% 
  summarize(
    ct = sum(count),
    pop_total = sum(pop_total),
    land_area_sqmi = sum(land_area_sqmi),
    den_total = pop_total/land_area_sqmi,
    rate_infection = (ct/pop_total) * 100000
  )  %>% 
 left_join(state_start, by = "Areaname") %>% 
 mutate(
    ln_ct = log1p(ct),
    day_num = as.numeric(caldate - start_date)
  )

glimpse(state_ts)

state_plot <- ggplot(state_ts, mapping = aes(x = day_num, y = ct, color = Areaname)) + 
  #geom_hex()+
  geom_point(size = .3) + 
  xlab("Number of Days Since First Confirmed Infection") +
  ylab("Confirmed Infection Count") +
  theme(legend.position="none") +
  theme(legend.text=element_text(size=6))

state_plot + ggtitle("Number of confirmed cases - by state")
state_plot + ggtitle("Number of confirmed cases - by state") + theme_tufte() + theme(legend.position="none")

# note use of geom_jitter() instead of geom_point
ln_plot <- ggplot(state_ts, mapping = aes(x = day_num, y = ln_ct, color = Areaname)) + 
  #geom_hex()+
  geom_jitter(size = .3) +
  xlab("Number of Days Since First Confirmed Infection (ln)") +
  ylab("Confirmed Infection Count (ln)") +
  ggtitle("Number of confirmed cases by date") +
  theme(legend.position="none") +
  theme(legend.text=element_text(size=6))

ln_plot + ggtitle("Number of confirmed cases (ln) - by state")
ln_plot + ggtitle("Number of confirmed cases (ln) - by state") + theme_tufte() + theme(legend.position="none")




## -----------------------------------------------------------------------------
# what are the states that have the long (and early slow) growth curves?
my_colors <- brewer.pal(8, "Dark2")
state_density_data_10d <- state_ts %>% 
  filter(day_num == 10) %>% 
  group_by(Areaname, start_date) %>% 
  summarize(
    max_days = max(day_num),
    max_infection = max(ct),
    max_rate_infection = max(rate_infection),
    avg_density = mean(den_total)
  ) %>% 
  arrange(desc(max_rate_infection))

state_density_data_20d <- state_ts %>% 
  filter(day_num == 20) %>% 
  group_by(Areaname, start_date) %>% 
  summarize(
    max_days = max(day_num),
    max_infection = max(ct),
    max_rate_infection = max(rate_infection),
    avg_density = mean(den_total)
  ) %>% 
  arrange(desc(max_rate_infection))

state_density_data_30d <- state_ts %>% 
  filter(day_num == 30) %>% 
  group_by(Areaname, start_date) %>% 
  summarize(
    max_days = max(day_num),
    max_infection = max(ct),
    max_rate_infection = max(rate_infection),
    avg_density = mean(den_total)
  ) %>% 
  arrange(desc(max_rate_infection))

state_density_data_40d <- state_ts %>% 
  filter(day_num == 40) %>% 
  group_by(Areaname, start_date) %>% 
  summarize(
    max_days = max(day_num),
    max_infection = max(ct),
    max_rate_infection = max(rate_infection),
    avg_density = mean(den_total)
  ) %>% 
  arrange(desc(max_rate_infection))

state_density_data_50d <- state_ts %>% 
  filter(day_num == 50) %>% 
  group_by(Areaname, start_date) %>% 
  summarize(
    max_days = max(day_num),
    max_infection = max(ct),
    max_rate_infection = max(rate_infection),
    avg_density = mean(den_total)
  ) %>% 
  arrange(desc(max_rate_infection))

state_density_data_60d <- state_ts %>% 
  filter(day_num == 60) %>% 
  group_by(Areaname, start_date) %>% 
  summarize(
    max_days = max(day_num),
    max_infection = max(ct),
    max_rate_infection = max(rate_infection),
    avg_density = mean(den_total)
  ) %>% 
  arrange(desc(max_rate_infection))

state_density_data_70d <- state_ts %>% 
  filter(day_num == 70) %>% 
  group_by(Areaname, start_date) %>% 
  summarize(
    max_days = max(day_num),
    max_infection = max(ct),
    max_rate_infection = max(rate_infection),
    avg_density = mean(den_total)
  ) %>% 
  arrange(desc(max_rate_infection))

state_density_data_80d <- state_ts %>% 
  filter(day_num == 80) %>% 
  group_by(Areaname, start_date) %>% 
  summarize(
    max_days = max(day_num),
    max_infection = max(ct),
    max_rate_infection = max(rate_infection),
    avg_density = mean(den_total)
  ) %>% 
  arrange(desc(max_rate_infection))

base_plot <- ggplot() +
  geom_jitter(data = state_density_data_10d, mapping = aes(x = max_rate_infection, y = avg_density), color = my_colors[1], shape = 1) +
  geom_jitter(data = state_density_data_20d, mapping = aes(x = max_rate_infection, y = avg_density), color = my_colors[2], shape = 2) +
  geom_jitter(data = state_density_data_30d, mapping = aes(x = max_rate_infection, y = avg_density), color = my_colors[3], shape = 3) +
  geom_jitter(data = state_density_data_40d, mapping = aes(x = max_rate_infection, y = avg_density), color = my_colors[4], shape = 4) +
  geom_jitter(data = state_density_data_50d, mapping = aes(x = max_rate_infection, y = avg_density), color = my_colors[4], shape = 5) +
  geom_jitter(data = state_density_data_60d, mapping = aes(x = max_rate_infection, y = avg_density), color = my_colors[4], shape = 6) +
  geom_jitter(data = state_density_data_70d, mapping = aes(x = max_rate_infection, y = avg_density), color = my_colors[4], shape = 7) +
  geom_jitter(data = state_density_data_80d, mapping = aes(x = max_rate_infection, y = avg_density), color = my_colors[4], shape = 8) +
  theme_tufte() +
  xlab("Infection rate (cases/100,000) ") +
  ylab("Average population density (persons/mi^2)") +
  ggtitle("Infection Rate vs. Population Density Day 10, 20, 30, 40, 50, 60, 70")
base_plot

base_plot + 
  scale_y_continuous(trans='log2')+
  scale_x_continuous(trans="log2")

print(state_density_data_10d)

state_density_data_10d %>% 
  kable() %>% 
  kable_styling()

# Another way to do this - by combining out three dataframes into 1 with a new column that identifies the day number for the values

plot_data <-  bind_rows(
  mutate(state_density_data_10d, day_num = "d10"),
  mutate(state_density_data_20d, day_num = "d20"), 
  mutate(state_density_data_30d, day_num = "d30"), 
  mutate(state_density_data_40d, day_num = "d40"),
  mutate(state_density_data_50d, day_num = "d50"),
  mutate(state_density_data_60d, day_num = "d60"), 
  mutate(state_density_data_70d, day_num = "d70"), 
  mutate(state_density_data_80d, day_num = "d80")
  )
base_plot <- ggplot(plot_data, mapping = aes(x = max_rate_infection, y = avg_density, color = day_num, shape = day_num)) +
  geom_jitter() +
  scale_color_brewer(palette = "Dark2") +
  theme_tufte() +
  xlab("Infection rate (cases/100,000) ") +
  ylab("Average population density (persons/mi^2)") +
  ggtitle("Infection Rate vs. Population Density Day 10, 20, 30, 40, 50, 60, 70")

base_plot

base_plot + scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7, 8))

base_plot + 
  scale_shape_manual(values = c(1, 2, 3, 4, 5, 6, 7, 8)) +
  scale_y_continuous(trans='log2') +
  scale_x_continuous(trans="log2")

print(plot_data)

plot_data %>% 
  kable() %>% 
  kable_styling()



## -----------------------------------------------------------------------------
purl('workflow.rmd', output = "workflow.R")

