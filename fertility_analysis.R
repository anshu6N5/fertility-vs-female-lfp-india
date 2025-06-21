install.packages("MASS")
# loading of necessary libraries 
library(tidyverse)
library(readr)
library(janitor)
library(WDI)
library(ggplot2)

## project1 =relationship between fertility rate (births per woman) 
## and female labor force participation rate (FLFP) using World Bank data.

#search the indicators for the project.
WDIsearch("fertility rate")
WDIsearch("% of females ages 15+ in the labor force")


# Get the data from World bank 
data <- WDI(country = "IN", indicator = c("fertility" = "SP.DYN.TFRT.IN", "female_lfp" = "SL.TLF.CACT.FE.ZS"), start = 1990, end = 2022)
view(data)

#cleaning of data 
data <- data %>%
  clean_names() %>% 
  drop_na(fertility, female_lfp)
names(data)

#model 
model <- lm(fertility~female_lfp, data = data)
summary(model)

# the relationship between them on a graph 
ggplot(data, aes(x = female_lfp, y = fertility))+
  geom_point(color = "black", size = 3)+
  geom_smooth(method = "lm", se = TRUE, color = "red")+
  labs(
    title = "Does female Labour Participation Reduce the Fertility Rate In India ??",
    subtitle = "Relationship Between the Female Labour participation and Fertility",
    x = "Female Labour Participation Rate(%)",
    y = "Fertility Rate(%)",
    caption = "Data source : World Bank Via WDI"
    
  ) + 
  theme_minimal()