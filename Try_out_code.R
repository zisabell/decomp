# New try-out with this suggestion: https://stackoverflow.com/questions/59492324/how-to-conduct-an-anova-of-several-variables-taken-on-individuals-separated-by-m

# aov_basic <- mt_4 %>%
#   # First pivot the data longer so each dependent variable is on its own row
#   pivot_longer(
#     cols = c(ph_kcl, som_percentage, b.d., moisture_percentage, NO2_NO3_mg_kg, NH4_mg_kg,
#                 Total_P_mg_kg, Olsen_P_mg_kg, earthworms_2022, biomass_ew_total, Springtails_nr_kg,
#                 Mites_oribatid_kg, Mites_other_kg, nr_nematodes_gr_dry,Bacteria_total, Microbial,
#                 Actinobacteria_total, Fungi_total, Total_plfa, AMF_NLFA),
#     names_to = "name",
#     values_to = "value"
#   ) %>%
#   # group by both group name and dependent variable
#   group_by(name) %>%
#   # nest the data, so each dataset is unique for each dependent and independent variable
#   nest() %>%
#   mutate(
#     # run an anova on each nested data frame
#     anova = map(data, ~lmer(value ~ landuse + (1|location), data=.x,
#                       na.action = "na.omit")), # may need to change aov() call here
#     # use broom to tidy the output
#     tidied_results = map(anova, broom.mixed::tidy)
#   )
#
# aov_basic %>%
#   # select columns of interest
#   dplyr::select(name, tidied_results) %>%
#   # unnest to access summary information of ANOVA
#   unnest(cols = c(tidied_results))
# # printing of results doesn't work...

# Code for earthworm stacked bar

``` {r plot, stacked bar earthworm functional groups, fig.dim = c(10,8)}

mt_ew_long_b <- mt_2 %>% pivot_longer(cols=c(Biomass_anecic, Biomass_endogeic, Biomass_epigeic, Biomass_juvenile),
                                      names_to="Groups",values_to="biomass")

mt_ew_long_b$location <- factor(mt_ew_long_b$location, levels = c("Haarzuilens", "Vlaardingen","Boelenslaan", "Amsterdam"),
                                labels = c("Location 1", "Location 2", "Location 3", "Location 4"))

mt_ew_long_b %>% 
  group_by(location, landuse, Groups) %>%
  summarize(average = mean(biomass, na.rm=TRUE), .groups="drop") %>%
  ggplot(aes(x=landuse, y=average, fill=Groups)) + 
  geom_col(width=0.6) +
  scale_fill_manual(values=wes_palette("Cavalcanti1"), labels=c("Anecic earthworms",
                                                                "Endogeic earthworms",
                                                                "Epigeic earthworms",
                                                                "Juveniles")) +
  facet_grid(~location) +
  theme_bw(base_size = 18) +
  theme(legend.position="bottom") +
  scale_x_discrete(labels=c("Food forest", "Grassland")) +
  labs(x="Landuse", y="Average earthworm biomass per plot (gr)",  fill="Microbial groups")

```