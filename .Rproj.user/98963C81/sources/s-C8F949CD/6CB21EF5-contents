bikeRouteData <- readOGR("census team data/Bike Routes/geo_export_5acdd40a-defd-4e38-bc51-9d35252ce617.shp")
bikeRouteDataFort <- tidy(bikeRouteData)

roadsData <- readOGR("census team data/tl_2019_17_prisecroads/tl_2019_17_prisecroads.shp")
roadsDataFort <- tidy(roadsData)
roadsDataChi <- roadsDataFort %>% 
  filter(long > -87.82, lat > 41.64, lat < 42.03)

write_rds(bikeRouteDataFort, "app/census team data/Bike Routes/bikeRouteDataFort.RDS")
write_rds(roadsDataChi, "app/census team data/Bike Routes/roadsDataChi.RDS")


