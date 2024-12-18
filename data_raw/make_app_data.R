# MAKE APP DATA FROM RESULTS_PARQUET.ZIP

librarian::shelf(
  arrow, dplyr,
  sf, sfarrow,
  giscoR
)

# 1. download results_parquet from https://zenodo.org/records/14004322
#   note download path


# 2. prepare geom data
#   (accesses V://)

# city points
unzip("V:/VolumeQ/AGteam/Eurostat/geography/URAU/URAU_PT_2020_4326.shp.zip")
geom_city <- st_read("URAU_PT_2020_4326.shp")
file.remove(grep("URAU_PT_2020_4326",dir(), value=T))

geom_city <- geom_city %>% filter(URAU_CATG!="FUA") %>%
  select(-POP_2020, -AREA_KM2, -URAU_CATG)
geom_city[geom_city$URAU_CODE=="EL001C","URAU_NAME"] <- "Athina"
geom_city[,c("x","y")] <- st_coordinates(geom_city)
geom_city$URAU_CODE <- as.factor(geom_city$URAU_CODE)

# country geoms
unzip("V:/VolumeQ/AGteam/Eurostat/geography/NUTS/NUTS_RG_20M_2021_4326.shp.zip")
geom_country <- st_read("NUTS_RG_20M_2021_4326.shp")
file.remove(grep("NUTS_RG_20M_2021_4326",dir(), value=T))

geom_country <- geom_country %>%
  filter(LEVL_CODE==0) %>%
  select(-LEVL_CODE, -NUTS_ID,-NUTS_NAME,-MOUNT_TYPE, -URBN_TYPE, -COAST_TYPE, -FID)

# geom regions
geom_region <- st_read("data_raw/EU_RGN/eu_region.shp")

# lookup labels (city dataset has ? characters)
disp_names <- read.csv("V:/VolumeQ/AGteam/Eurostat/lookup/URAU_NUTS_2021.csv") %>%
  select(city_code=URAU_CODE, city_name=CITY.NAME, country_code=CNTR_CODE) %>%
  distinct(.)
disp_names[disp_names$city_code=="EL001C","city_name"] <- "Athina"

# add some english names
engnames <- read.csv("V:/VolumeQ/AGteam/Eurostat/lookup/URAU_DisplayNames.csv") %>%
  select(city_code=URAU_CODE, city_name=LABEL) %>% filter(!grepl("F$",city_code)) %>%
  mutate(country_code=substr(city_code,1,2))
disp_names[disp_names$city_code%in%engnames$city_code,] <- engnames

cntr_names <-  gisco_countries %>%
  dplyr::select(country_code=CNTR_ID, country_name=NAME_ENGL) %>%
  st_drop_geometry(.);

lookup <- left_join(disp_names,cntr_names)
lookup[lookup$city_code=="ES069C","city_name"] <- "Castellón de la Plana"
rm(disp_names,cntr_names)

# 3. define function for joining parquet to geoms and pre-process

# factor columns
fac_cols <- c("period", "level","city","country","region",
              "ssp", "range", "ssp", "adapt","sc", "agegroup",
              "city_code","city_name","country_code","country_name")

# FX FROM PARQUET FILE TO APP-DATA WITH CITY, COUNTRIES, REGION GEOMS
prep_app_data <- function(readparquet, geom, lk=lookup, fct_cols=fac_cols) {

  if (geom=="city") {
    d <- left_join(readparquet, select(geom_city,-CC,-URAU_NAME),
                    by=c("city"="URAU_CODE")) %>%
      left_join(.,lk, by = c("city"="city_code")) %>%
      st_as_sf(., coords=c("x","y"), crs=4326)

  } else if  (geom=="ctry") {
    d <- left_join(readparquet, select(geom_country,-NAME_LATN),
                    by=c("country"="CNTR_CODE")) %>%
      left_join(., distinct(select(lk,country_name,country_code)),
                by = c("country"="country_code")) %>%
      st_as_sf(.,sf_column_name="geometry", crs=4326)

  } else if (geom=="rg") {
    d <- left_join(readparquet, geom_region, by=c("region"="region")) %>%
      st_as_sf(.,sf_column_name="geometry", crs=4326)
  }

  # FACTORS
  out <- d %>% mutate(across(
    any_of(fct_cols), as.factor
  )) %>%
  # ATTRIBUTABLE FRAC AND RATE TO BETTER VALUES
    mutate(across(contains("af"),~.x*100),
           across(contains("rate"),~.x*100000))

  return(out)
}

# 4. unzip and process files
#   unzip creates dir in working directory
unzip("C:/Users/lshad21/Downloads/results_parquet.zip", junkpaths = F)

pqfs <- setNames(
  as.list(grep(paste0(c("city","country","region"),
              collapse = "|"),
       dir("results_parquet/", full.names = T), value=T)),
  c("city_level", "city_period",
    "country_level", "country_period",
    "region_level","region_period"))

# WARNINGS EXPECTED (EXACT REASON UNKNOWN)
options(arrow.unsafe_metadata = F)

city_level <- read_parquet(pqfs$city_level)
city_period <- read_parquet(pqfs$city_period)
country_level <- read_parquet(pqfs$country_level)
country_period <- read_parquet(pqfs$country_period)
region_level <- read_parquet(pqfs$region_level)
region_period <- read_parquet(pqfs$region_period)

# somehow i can only manage to delete the files, leaving an empty dir
unlink(x=paste0(getwd(),"/results_parquet"), recursive=TRUE)
file.remove(dir("results_parquet", full.names = T))

# PROCESS
ci_le <- prep_app_data(city_level, "city")
ci_pe <- prep_app_data(city_period, "city")
co_le <- prep_app_data(country_level, "ctry")
co_pe <- prep_app_data(country_period, "ctry")
re_le <- prep_app_data(region_level, "rg")
re_pe <- prep_app_data(region_period, "rg")

## MAKE OPTIONS VECTORS - no need to rerun - sysdata.RDS loads all and is in git
level_ov <- levels(ci_le$level); names(level_ov) <- paste0(level_ov, "℃")
period_ov <- levels(co_pe$period);
adapt_ov <- levels(ci_le$adapt)
range_ov <- levels(co_pe$range); names(range_ov) <- c("Cold","Heat","Total")
ssp_ov <- levels(co_pe$ssp); names(ssp_ov) <- paste("SSP",ssp_ov)
sc_ov <- levels(ci_le$sc); names(sc_ov) <- c("Climate change", "Demographic change", "Both")
agegroup_ov <- levels(co_pe$agegroup); names(agegroup_ov) <- agegroup_ov; names(agegroup_ov)[6] <- "All"
city_ov <- levels(ci_pe$city);
country_ov <- levels(co_pe$country)
region_ov <- levels(re_pe$region)
outcomes_ov <- gsub("_est","",grep("est", names(re_le), value = T)); names(outcomes_ov) <-
  c("Excess deaths","Attributable fraction (%)", "Excess death rate (x10⁶)","Cumulative excess deaths")

usethis::use_data(adapt_ov,agegroup_ov,city_ov, country_ov, level_ov, outcomes_ov,
                  period_ov, range_ov, region_ov, ssp_ov, sc_ov,
                  internal=TRUE, overwrite = TRUE)

## SAVE PARITTIONED PARQUET FILES - warning expected
sfarrow::write_sf_dataset(ci_le,"inst/extdata/city_level",format="parquet", partitioning="agegroup")
sfarrow::write_sf_dataset(ci_pe,"inst/extdata/city_period",format="parquet", partitioning="agegroup")
sfarrow::write_sf_dataset(co_le,"inst/extdata/country_level",format="parquet", partitioning="agegroup")
sfarrow::write_sf_dataset(co_pe,"inst/extdata/country_period",format="parquet", partitioning="agegroup")
sfarrow::write_sf_dataset(re_le,"inst/extdata/region_level", format = "parquet", partitioning="agegroup")
sfarrow::write_sf_dataset(re_pe,"inst/extdata/region_period", format = "parquet", partitioning="agegroup")
