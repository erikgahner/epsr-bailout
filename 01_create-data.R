###
##
## Article:   Bailout or bust? Government evaluations in the wake of a bailout
## 
##            European Political Science Review
##
##            Erik Gahner Larsen          Robert Klemmensen       Michael Baggesen Klitgaard
##            E.G.Larsen@kent.ac.uk       rkl@sam.sdu.dk          mbk@sam.sdu.dk
##
##        
## Data:      European Social Survey: http://www.europeansocialsurvey.org/
##            Longitudinal Internet Studies for the Social Sciences: https://lissdata.nl
##            Eurobarometer: http://ec.europa.eu/commfrontoffice/publicopinion/
##
###

library("tidyverse")
library("rio")

liss07 <- import("~/Google Drev/data/liss/po/cv08a_1.1p_EN.dta")
liss08 <- import("~/Google Drev/data/liss/po/cv09b_2.1p_EN.dta")
liss.inc <- import("~/Google Drev/data/liss/in/ci08a_1.0p_EN.dta")
liss <- left_join(liss07, liss08, by="nomem_encr")
liss <- left_join(liss, liss.inc, by="nomem_encr")
ess <- import("../data/ESS4NL.dta")
eb04 <- import("~/Google Drev/data/eurobarometer/200804/ZA4744_v5-0-0.dta")
eb10 <- import("~/Google Drev/data/eurobarometer/200810/ZA4819_v3-0-2.dta")

liss <- liss %>%
  mutate(
    income = case_when(ci08a229 < 8000 ~ 1, 
                       ci08a228 > 8000 & ci08a228 <= 16000 ~ 2, 
                       ci08a228 > 16000 & ci08a228 <= 24000 ~ 3, 
                       ci08a228 > 24000 & ci08a228 <= 36000 ~ 4,
                       ci08a228 > 36000 & ci08a228 <= 48000 ~ 5,
                       ci08a228 > 48000 & ci08a228 <= 60000 ~ 6,
                       ci08a228 > 60000 & ci08a228 <= 200000 ~ 7,
                       TRUE ~ NA_real_),
    # Generate government variable
    government = case_when(
      cv08a058 == 3 | cv08a058 == 4 | cv08a058 == 9 ~ 1,
      cv08a058 != 998 & cv08a058 != 999 ~ 0,
      TRUE ~ NA_real_)
  ) %>%
  # Recode missing values (999 in LISS) to NA
  mutate_at(vars(cv08a030, cv08a043, cv09b030, cv09b043,
                 cv08a038, cv09b038, cv08a039, cv09b039, 
                 cv08a044, cv09b044, cv08a039, cv09b039,
                 cv08a040, cv09b040, cv08a041, cv09b041, 
                 cv08a042, cv09b042, cv08a044, cv09b044,
                 cv08a045, cv09b045, cv08a046, cv09b046), 
            function(x) case_when(x == 999 ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  drop_na(cv08a030, cv09b030, cv08a043, cv09b043) %>%
  mutate(eco = cv09b043 - cv08a043,
         gov = cv09b030 - cv08a030)


ess <- ess %>% 
  # Recode missing
  mutate_at(vars(stfgov, stfeco, stflife, stfdem), 
            function(x) case_when(x > 50 ~ NA_real_, TRUE ~ as.numeric(x))) %>%
  mutate(
    bailout = case_when(
      inwmme == 9 & inwdde < 28 ~ 0,
      inwmme == 9 & inwdde > 19 ~ 1,
      inwmme == 10 ~ 1,
      inwmme == 11 ~ 1,
      TRUE  ~  NA_real_
    ),
    Bailout = bailout,
    income = ifelse(hinctnta < 11, hinctnta, NA),
    government = case_when(
      prtvtcnl == 66 | prtvtcnl == 77 ~ NA_real_,
      prtvtcnl == 1 | prtvtcnl == 2 | prtvtcnl == 8 ~ 1,
      TRUE ~ 0),
    eduyrs = ifelse(eduyrs == 77 | eduyrs == 88, NA, eduyrs),
    lrscale = ifelse(lrscale == 77 | lrscale == 88, NA, lrscale),
    hinctnta = ifelse(hinctnta == 77 | hinctnta == 88, NA, hinctnta),
    uemp3m = ifelse(uemp3m == 7 | hinctnta == 8, NA, uemp3m),
    region_north = case_when(regionnl > 100 & regionnl < 200 ~ 1, TRUE ~ 0),
    region_east = case_when(regionnl > 200 & regionnl < 300 ~ 1, TRUE ~ 0),
    region_west = case_when(regionnl > 300 & regionnl < 400 ~ 1, TRUE ~ 0),
    region_south = case_when(regionnl > 400 & regionnl < 500 ~ 1, TRUE ~ 0)
  ) %>%
  drop_na(bailout)


eb04 <- eb04 %>%
  filter(v6 == 3) %>%
  mutate(
    tr = 0,
    eco = case_when(
      v90 == 1 ~ 2,
      v90 == 2 ~ 0,
      v90 == 3 ~ 1,
      TRUE  ~  NA_real_
    ),
    gov = case_when(
      v213 == 2 ~ 0,
      v213 == 1 ~ 1,
      TRUE  ~  NA_real_
    )
  ) %>%
  select(tr, eco, gov)


eb10 <- eb10 %>%
  filter(v6 == 3) %>%
  mutate(
    tr = 1,
    eco = case_when(
      v124 == 1 ~ 2,
      v124 == 2 ~ 0,
      v124 == 3 ~ 1,
      TRUE  ~  NA_real_
    ),
    gov = case_when(
      v228 == 2 ~ 0,
      v228 == 1 ~ 1,
      TRUE  ~  NA_real_
    )
  ) %>%
  select(tr, eco, gov)

eb <- rbind(eb04, eb10)

write_csv(eb, "data_eb.csv")

ess %>%
  select(stfeco, stfgov, stflife, stfdem, bailout, Bailout, income, government, gndr, agea, eduyrs, hinctnta, partner, uemp3m, polintr, lrscale, region_north, region_east, region_west, region_south) %>%
  write_csv(., "data_ess.csv")

liss %>% 
  select(nomem_encr,
         cv08a043, cv09b043, cv08a030, cv09b030,
         income, government, eco, gov,
         cv09b038, cv08a038, cv09b039, cv08a039, cv09b040, cv08a040, cv09b041, cv08a041, 
         cv09b042, cv08a042, cv09b044, cv08a044, cv09b045, cv08a045, cv09b046, cv08a046) %>%
  write_csv(., "data_liss.csv")
