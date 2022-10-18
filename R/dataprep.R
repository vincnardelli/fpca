#### Data preparation
library(readxl)
library(dplyr)
library(sf)


population <- read.csv("data/nga_pop_adm2_2016.csv") %>% 
  select("level2code"="admin2Pcode", 
         population = Population2016)


shp <- st_read("data/maps/nga_adm_osgof_20190417/nga_admbnda_adm2_osgof_20190417.shp") %>% 
  filter(ADM1_EN %in% c("Kano", "Katsina", "Lagos", "Kaduna"))%>% 
  mutate("level0" = "Nigeria", 
         "level0code" = "NG", 
         "level3" = NA, 
         "level3code" = NA, 
         "level3urban" = NA, 
         area = st_area(.)) %>% 
  select(level0, level0code, 
         level1 = ADM1_EN, level1code = ADM1_PCODE,
         level2 = ADM2_EN, level2code = ADM2_PCODE, 
         level3, level3code, level3urban, area)  %>% 
  left_join(population) %>% 
  mutate(density = population/area, 
         level3urban_new = ifelse(as.numeric(density) > 0.0008, TRUE, FALSE))

# plot(hist(shp$density))
# 
# 
# 
# 
# shp2 <- st_read("data/maps/nga_bnd_adm3wards_062015/nga_bnd_adm3wards_062015.shp") %>% 
#   mutate("level0" = "Nigeria", 
#          "level0code" = "NG") %>% 
#   select(level0, level0code, 
#          level1 = StateName, level1code = StateCode,
#          level2 = LGAName, level2code = LGACode, 
#          level3 = WardName, level3code = WardCode, 
#          level3urban = Urban) %>% 
#   mutate(level2code = paste0("NG0", level2code), 
#          level3code = paste0("NG0", level3code)) %>% 
#   filter(level1 %in% c("Kano", "Katsina"))


map <- shp
#   
# 
# library(ggplot2)
# library(patchwork)
# plot1 <- ggplot(data=shp2) +
#   geom_sf(aes(fill=level3urban), lwd=0) +
#   ggtitle("Old map")
# 
# plot2 <- ggplot(data=shp) +
#   geom_sf(aes(fill=level3urban_new), lwd=0) +
#   ggtitle("New map")
# 
# plot1 | plot2

conversion_factor <- readxl::read_xlsx("data/PackagingConversionFactorTokg.xlsx") %>% 
  select(packaging, conversion = `Conversion factor (to kg)`)

market_reclassification <- read.csv("data/reclassification_market.csv", sep=";", 
                         header=FALSE, col.names = c("other_market", "other_market_new"))

packaging_reclassification <- read.csv("data/reclassification_packaging.csv") %>% 
  select(packaging, other_packaging, packaging_new)

save(map,
     conversion_factor, 
     market_reclassification, 
     packaging_reclassification,
     file="data/data.Rdata")


