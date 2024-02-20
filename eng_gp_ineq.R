#Packages

pks <- c("tidyverse", "fingertips")

lapply(pks, require, character.only = T)

#Data

#GP practice lookup - from https://fingertips.phe.org.uk/profile/general-practice

gp_prac_lookup <- read.csv("indicators-GPs.data.csv")

#inspect

head(gp_prac_lookup)
glimpse(gp_prac_lookup)


#filter to deprivation data for 2019

gp_prac_lookup <- gp_prac_lookup %>%
  filter(Indicator.ID == 93553 & Time.period == "2019")

#Hypertension practice data
#from https://qof.digital.nhs.uk/

bp_df <- read.csv("gp_bp_data.csv")

head(bp_df)
head(gp_prac_lookup)
view(gp_prac_lookup)

names(bp_df)
names(gp_prac_lookup)
sapply(gp_prac_lookup, class)

bp_depriv <- left_join(bp_df, gp_prac_lookup, by = c("Practice.code" = "Area.Code"))

write.csv(bp_depriv, "bp_deprv.csv")

bp_depriv %>% select(Practice.code) %>% unique()

bp_depriv <- bp_depriv %>%
  select(Practice.code, Practice.name, Patients.receiving.Intervention...., Value) %>%
  rename(prac_code=1, prac_name=2, perc_bp=3, depriv=4) %>%
  mutate(perc_bp = as.double(perc_bp)) %>%
  na.omit()

lm_fit <- lm(perc_bp ~ depriv, data = bp_depriv)

predicted_df <- data.frame(bp_pred = predict(lm_fit, bp_depriv), perc_bp=bp_depriv$perc_bp)

summary(lm_fit)

bp_depriv %>%
  ggplot(aes(x=depriv, y=perc_bp)) +
  geom_point(color = "blue") + 
  geom_smooth(method = "lm", se = F, colour = "red") + 
  theme_minimal() + 
  labs(x="GP practice deprivation (IMD 2019)", 
       y="% of adults (18-79) on hypertension register with BP to target",
       title = "Relationship between GP practice deprivation and hypertension management",
       subtitle = "England, 2019",
       caption = "Source: NHS England and OHID, 2024")
  #geom_line(color='red',data = predicted_df, aes(x=bp_pred, y=perc_bp))

bp_depriv %>%
  select(Practice.code, Practice.name, Patients.receiving.Intervention...., Value) %>% write.csv("bp_depriv.csv")

sapply(bp_depriv, class)
