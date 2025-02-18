library(tidyverse)
library(sf)
library(rio)
library(tidycensus)
acs_var <- c(
  tot_pop = "B01003_001", #total population
  pop_u18 = "B17006_001", #population under 18
  med_inc = "B19013_001", #median household income
  med_rent = "B25031_001", #median monthly housing costs
  his_pop = "B03002_012", #hispanic population
  wh_pop = "B03002_003", #white population
  bl_pop = "B03002_004", #black population
  as_pop = "B03002_006", #asian population
  rohh = "B25106_024", #renter-occupied households
  thh = "B25106_001", #total households
  pop_bp = "B17020_002", #population below poverty
  bp_u18 = "B17006_002", #population under 18 below poverty
  med_val = "B25097_001", #median value of owner-occupied housing units
  rcb1 = "B25106_028", #gross rent as a percentage of income 30% or more,
  rcb2 = "B25106_032",
  rcb3 = "B25106_036",
  rcb4 = "B25106_040",
  rcb5 = "B25106_044"
)
census_tract_2020 <- get_acs(
  geography = "tract",
  state = "TX",
  county = counties,
  variables = acs_var,
  year = 2020,
  survey = "acs5",
  output = "wide",
  geometry = TRUE
) %>%
  mutate(rcbE = rcb1E + rcb2E + rcb3E + rcb4E + rcb5E) %>%
  st_transform(crs = 6584) %>%
  mutate(AreaTract = as.numeric(st_area(.)))
costar <- import(paste0(libDB, "Data Library/COSTAR/Processed Data/Multi-Family Apartments (September 2023).csv")) %>%
  janitor::clean_names(.) %>%
  st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  filter(number_of_units > 1) %>%
  mutate(zipshort = ifelse(nchar(zip) == 10, str_sub(zip, end=-6), 
                           ifelse(nchar(zip) == 8, str_sub(zip, end=-4), zip)),
         wgtRent = number_of_units*avg_asking_unit) %>%
  st_transform(crs = 6584)
moym <- googlesheets4::read_sheet(ss = "https://docs.google.com/spreadsheets/d/1CF2OuTe_dEKoUa9UPJvwta2tuwEXEAl-7OUdk82mY-M/",
                                  sheet = "Change by Campus ID") %>%
  janitor::clean_names(.) %>%
  filter(!is.na(campus)) %>%
  transmute(SLN = as.double(tea_campus_id),
            moymPerGr1t3_24 = as.numeric(percent_moy_moves_grades_1_3_2023_2024),
            moymPerGrkt5_24 = as.numeric(percent_moy_moves_grades_kn_5_2023_2024),
            moymPerGr1t3_23 = as.numeric(percent_moy_moves_grades_1_3_2022_2023),
            moymPerGrkt5_23 = as.numeric(percent_moy_moves_grades_kn_5_2022_2023),
            moymGr1t3_24 = as.numeric(number_moy_moves_grades_1_3_2023_2024),
            moymGrkt5_24 = as.numeric(number_moy_moves_grades_kn_5_2023_2024),
            moymGr1t3_23 = as.numeric(number_moy_moves_grades_1_3_2022_2023),
            moymGrkt5_23 = as.numeric(number_moy_moves_grades_kn_5_2022_2023))

campus_elem_2020 <- st_read(paste0(libDB, "Data Library/Dallas Independent School District/2023_2024 School Year/Elementary_Attendance_Boundaries.shp")) %>%
  select(SLN, ELEM_DESC, geometry) %>%
  st_transform(crs = 6584) %>%
  st_intersection(census_tract_2020, .) %>%
  mutate(
    SLN = SLN,
    name = ELEM_DESC,
    AreaIntersect = as.numeric(st_area(.)),
    PerIntersect = AreaIntersect / AreaTract,
    pop_intersect = round(PerIntersect * tot_popE, digits = 4),
    popbp_intersect = round(PerIntersect * pop_bpE, digits = 4),
    popu18_intersect = round(PerIntersect * pop_u18E, digits = 4),
    bpu18_intersect = round(PerIntersect * bp_u18E, digits = 4),
    rohh_intersect = round(PerIntersect * rohhE, digits = 4),
    thh_intersect = round(PerIntersect * thhE, digits = 4),
    rcb_intersect = round(PerIntersect * rcbE, digits = 4),
    as_intersect = round(PerIntersect * as_popE, digits = 4),
    bl_intersect = round(PerIntersect * bl_popE, digits = 4),
    wh_intersect = round(PerIntersect * wh_popE, digits = 4),
    his_intersect = round(PerIntersect * his_popE, digits = 4)
  ) %>%
  group_by(SLN, name) %>%
  summarise(
    tot_pop = sum(pop_intersect),
    bp_pop = sum(popbp_intersect) / tot_pop,
    bp_u18 = sum(bpu18_intersect),
    rohh = sum(rohh_intersect),
    thh = sum(thh_intersect),
    med_rent = mean(med_rentE, na.rm = TRUE),
    mhi = mean(med_incE, na.rm = TRUE),
    per_as = sum(as_intersect) / tot_pop,
    per_bl = sum(bl_intersect) / tot_pop,
    per_wh = sum(wh_intersect) / tot_pop,
    per_his = sum(his_intersect) / tot_pop
  ) %>%
  select(SLN, name, tot_pop:per_his) %>%
  mutate(name = str_remove(name, " School"))

campus_midd_2020 <- campus_elem_2020 # Adjusted similarly for middle schools
campus_high_2020 <- campus_elem_2020 # Adjusted similarly for high schools

profile_elem_2020 <- costar %>%
  st_join(., campus_elem_2020) %>%
  st_drop_geometry(.) %>%
  filter(!is.na(SLN)) %>%
  group_by(SLN, name) %>%
  summarize(
    tot_mf = n(),
    tot_units = sum(number_of_units, na.rm = TRUE),
    MAR = mean(avg_asking_unit, na.rm = TRUE)
  ) %>%
  mutate(IncomeNeed = (MAR * 4) * 12) %>%
  left_join(campus_elem_2020, .)

profile_midd_2020 <- profile_elem_2020 # Adjusted similarly for middle schools
profile_high_2020 <- profile_elem_2020 # Adjusted similarly for high schools

profile_campus_2020 <- rbind(
  st_drop_geometry(profile_elem_2020),
  st_drop_geometry(profile_midd_2020),
  st_drop_geometry(profile_high_2020)
) %>%
  mutate(
    tot_mf = ifelse(is.na(tot_mf), 0, tot_mf),
    tot_units = ifelse(is.na(tot_units), 0, tot_units)
  )
