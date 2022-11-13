
# load data, functions, and clean
source(here("code", "03_clean.R"))


# plot
county_df %>% 
  ggplot(aes(long, lat, group=group)) +
  geom_polygon(aes(color = county_boundary, fill=county_color)) +
  coord_map("conic", lat0 = 30) +
  scale_color_identity() +
  scale_fill_identity()

