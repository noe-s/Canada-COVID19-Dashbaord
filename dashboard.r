#COVID-19 R Dashboard to show stats on Quebec
#Uses updated dataset from StatsCanada

# Load packages -----------------------------------------------------------

#install.packages(c("tidyverse", "rvest", "lubridate"))
library(tidyverse)
library(rvest)
library(lubridate)

# Read files --------------------------------------------------------------

# set url
url <- "https://health-infobase.canada.ca/src/data/covidLive/covid19.csv"

PHAC <- 
read_csv(url)

# View dataset
#glimpse(PHAC)

PHAC <- 
PHAC %>% 
  #Alter date format for from text -> date
  mutate(date = dmy(date))


# Create first graph ------------------------------------------------------

ca <-
  PHAC %>% 
  #Canada Only
  filter(prname == "Canada")

ca %>% 
  ggplot(mapping = aes(x = date, y = numconf)) +
  geom_col()

ca %>% 
  ggplot(mapping = aes(x = date, y = numconf)) +
  geom_line() +
  # change y axis to log scale
  scale_y_log10() +
  # change theme of map to minimal
  theme_minimal() +
  # add labels for graph
  labs(title = "Development of confirmed cases in Canada",
       y = "Confirmed cases (logarithmic scale)",
       x = "",
       caption = "Source: PHAC, Graph: Yourself")


# Get population data -----------------------------------------------------

url <- "https://www150.statcan.gc.ca/t1/tbl1/en/dtl!downloadDbLoadingData-nonTraduit.action?pid=1710000901&latestN=5&startDate=&endDate=&csvLocale=en&selectedMembers=%5B%5B2%2C4%2C10%2C3%2C5%2C6%2C7%2C12%2C9%2C14%2C8%2C11%2C15%2C1%5D%5D"

pop_ca <- 
  read_csv(url)

glimpse(pop_ca)

pop_ca %>% 
  # distinct() lists all unique values in a variable of a dataset
  distinct(REF_DATE)

pop_ca <- 
pop_ca %>% 
  filter(REF_DATE == "2020-01") %>% 
  select(GEO, VALUE)


# Relative confirmed cases graph in French -----------------------------------------

PHAC %>% distinct(prnameFR)

# sets the locale information of your R environment to French/Canada
Sys.setlocale(locale = "French_Canada")

PHAC %>% 
 # this filters out observations/rows that is equal to 'Canada' or contains
 # the word 'Voyageurs' to display provinces and territories
 filter(prnameFR != "Canada" & str_detect(prnameFR, "Voyageurs", negate = T)) %>% 

  left_join(pop_ca, by = c("prname" = "GEO")) %>% 
  # Calculate a new variable for proportional confirmed cases per 100,000 people
  mutate(propconf = numconf / VALUE * 1e5) %>% 
  ggplot(mapping = aes(x = date, y = propconf, 
                       color = prnameFR)) +
  geom_line() +
  theme_classic() +
  # Change Format of the dates shown (france goes dd-mm-yyyy)
  scale_x_date(date_labels = "%d %b") +
  labs(x = "", y = "Cas confirmÃ©s pour 100 000 habitants",
       title = "Ãvolution relatif du Covid-19 au Canada") +
  guides(color = guide_legend(title = "Provinces/Territoires"))


# Break -------------------------------------------------------------------


# Webscraping -------------------------------------------------------------

url <- "https://www.quebec.ca/sante/problemes-de-sante/a-z/coronavirus-2019/situation-coronavirus-quebec/"

MSSS <- 
url %>% 
  read_html() %>% 
  html_nodes(".tableauOverflow") %>%
  html_nodes("tbody td") %>% 
  # html_text() will then turn the html code into actual text
  html_text() %>% 
  split(rep(1:2, times = length(.) / 2)) %>% 
  as_tibble() %>% 
  rename(Regions = 1, numconf = 2) %>% 
  mutate(Regions = if_else(str_detect(Regions, "Total"), Regions, str_sub(Regions, 6)),
         numconf = numconf %>% str_replace("\\s", "") %>% as.integer())


# Create Quebec graph -----------------------------------------------------

# set system back to English
Sys.setlocale(locale = "English_Canada")

qc %>% 
  # filter all regions except the 'Total'
  filter(Regions != "Total") %>% 
  ggplot() +
  # create a column graph; Reorder the x axis values by the numconf variable
  geom_col(mapping = aes(x = Regions %>% fct_reorder(numconf),
                         y = numconf,
                         # we use the Regions to fill the columns with colour
                         fill = Regions)) +
  theme_light() +
  labs(x = "",
       y = "Confirmed cases",
       #Set title to today's date
       title = paste("situtation of Covid-19 in Quebec as of", today() %>% format("%d %b %Y")))

qc %>% 
  filter(Regions != "Total") %>% 
  ggplot(mapping = aes(x = Regions %>% fct_reorder(numconf),
                       y = numconf,
                       fill = Regions)) +
  geom_col() +
  geom_text(mapping = aes(label = numconf)) +
  theme(axis.text.x = element_text(angle = 90))
