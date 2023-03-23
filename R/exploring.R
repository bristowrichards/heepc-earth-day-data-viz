library(tidyverse)
library(janitor)


# get disease burden data by country
gbd_risk <- read_csv('data/IHME-GBD_2019_DATA-f9dbd439-1.csv')

# define categories
world_bank_categories <- c(
  'World Bank High Income',
  'World Bank Low Income',
  'World Bank Lower Middle Income',
  'World Bank Upper Middle Income'
)

# clean data
viz_data <- gbd_risk |> 
  rename(
    income_level = location_name,
    percent_risk = val,
    sex = sex_name
  ) |> 
  mutate(
    income_level = str_replace(income_level, 'World Bank ', ''),
    income_level = factor(income_level,
                          levels = c('Low Income',
                                     'Lower Middle Income',
                                     'Upper Middle Income',
                                     'High Income'))
  ) |> 
  select(income_level, percent_risk, sex, year)

# grid by income, line type by sex
viz_data |> 
  ggplot(aes(x = year, 
             y = percent_risk, 
             group = interaction(income_level, sex), 
             color = income_level, 
             linetype = sex)
       ) +
  geom_line(size = 1) +
  facet_wrap(~income_level) +
  labs(title = 'Mortality Rate by Country and Gender',
       x = 'Year',
       y = 'Percent Mortality',
       linetype = 'Gender') +
  guides(color = 'none') +
  theme_minimal()

# no grid, color by income, line type by sex
viz_data |> 
  ggplot(aes(x = year, 
             y = percent_risk, 
             group = interaction(income_level, sex), 
             color = income_level, 
             linetype = sex)
  ) +
  geom_line(size = 1) +
  labs(title = 'Mortality Rate by Country and Gender',
       x = 'Year',
       y = 'Percent Mortality',
       linetype = 'Gender') +
  theme_minimal()

# no grid, color by income, no sex disaggregation
viz_data |> 
  filter(sex == 'Both') |> 
  ggplot(aes(x = year, 
             y = percent_risk, 
             group = interaction(income_level, sex), 
             color = income_level, 
             )
  ) +
  geom_line(size = 1) +
  labs(title = 'Percentage of Mortality with Risk Rate to Indoor Air Pollution',
       x = 'Year',
       y = 'Percent Mortality'
       ) +
  theme_minimal()

