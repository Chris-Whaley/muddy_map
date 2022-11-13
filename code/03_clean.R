
# load data and functions
source(here("code", "01_load.R"))
source(here("code", "02_functions.R"))

# create party win, margin, and county color columns
county <- county_votes %>% 
  rowwise() %>% 
  mutate(dem_pct = votes_dem_2016/(votes_dem_2016 + votes_gop_2016),
         rep_pct = votes_gop_2016/(votes_dem_2016 + votes_gop_2016),
         party = if_else(votes_dem_2016 > votes_gop_2016, "democrat", "republican"),
         
         # hue = party
         hue = hue(party),
         
         # saturation = party vote margin
         sat = saturation(votes_dem_2016, votes_gop_2016),
         
         # lightness = vote counts
         light = lightness(sum(votes_dem_2016, votes_gop_2016)),
         
         # map color
         county_color = hsl_to_rgb(hue, sat, light),
         
         # map county boundaries. constant lightness of 50%
         county_boundary = hsl_to_rgb(hue, sat, light=0.5)
         )


# adding spatial component
county_df <- left_join(county, counties, by = c("FIPS" = "county_fips"))
