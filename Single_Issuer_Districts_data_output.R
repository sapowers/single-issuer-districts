
library(tidyverse)


state_regions <- read_csv("State_Regions.csv")

county_rating <- read_csv("individual_county_rating_area_crosswalk_2019_2019_04_01.csv") %>%
  mutate(county_fips = paste0("0",fips_code)) %>%
  separate(county_fips, c(NA, "county_fips"), -5)

issuers_counties   <- read_csv("individual_issuer_county_report_2019_2019_08_29.csv") %>%
  mutate(county_fips = paste0("0",fips_code)) %>%
  separate(county_fips, c(NA, "county_fips"), -5)


number_issuers_counties <- issuers_counties %>%
  filter(market == "on_market") %>%
  group_by(state, county_name, county_fips) %>%
  summarize(count_issuers = n(), count_plans = sum(plan_count)) %>% ungroup()

write.csv(number_issuers_counties, file = "plans_per_county.csv")


us_counties_graph <- urbnmapr::counties %>% select(-county_name) %>% inner_join(number_issuers_counties, by = "county_fips") %>%
  select(county_name, county_fips, everything())

us_states_regions <-  state_regions %>% inner_join( urbnmapr::states , by = c("ST"= "state_abbv"))



issuers_rating_area <-
  number_issuers_counties %>%
  inner_join(county_rating) %>%
  select(AREA = rating_area_id, count_issuers, count_plans, county_name) %>%
  mutate(SINGLEISSUER = ifelse(count_issuers == 1, 1, 0))




plans_regular <- read_csv("plan_data.csv")

nrow(plans_regular)

issuers_by_area  <-
  plans_regular %>%
  select(ST, CARRIER, AREA) %>%
  inner_join(state_regions) %>%
  distinct() %>%
  group_by(Region, ST, AREA) %>%
  summarize(ISSUERS = n(), SINGLEISSUER = ifelse(ISSUERS == 1, 1, 0 ))

single_issuer_descriptors <-
  plans_regular %>%
  inner_join(issuers_by_area) %>%
  filter( METAL == "Silver" , actively_marketed == TRUE) %>%
  select(SINGLEISSUER, everything())




plans_per_county <- 
  us_counties_graph %>%
  mutate(tooltip = gsub( "'","", paste0( county_name, ", ", state_name, ": <br> ", count_plans, " plans"))) %>%
  
  ggplot(aes(long, lat, group = group, fill = count_plans)) +
  geom_polygon(color = "white", size = .1) +
  scale_fill_gradientn(colors = c("white", "dodgerblue2", "blue4"), 
                       limits = c(-10,  300),
                       breaks = c(0,50,100, 150,200, 250, 300),
                       guide = guide_colorbar(
                         #labesl = scales::dollar,
                         direction = "horizontal",
                         title.position = "top", nbin = 10)
  ) +
  geom_polygon(data = states, mapping = aes(long, lat, group = group ),
               fill = NA, color = "#ffffff") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.title = element_text(),
        legend.key.width = unit(.5, "in")
  ) +
  labs(fill = "")+    
  theme(panel.grid.major = element_blank(), 
        panel.background = element_rect(fill = "white"),
        legend.key.width = unit(2.5, "line"),
        legend.key.height = unit(1, "line"),
        legend.position = "top",
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(size=22, hjust = .5 )
        
  ) +
  ggtitle("Number of Plans Operating Per County")


plans_per_county
