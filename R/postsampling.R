postsampling <- function(data, buffer=0, graph=FALSE){
  map = readOGR("data/maps/nga_admbnda_adm2_osgof", verbose=F)
  #map = readOGR("data/maps/nga_bnd_adm3wards_062015", verbose=F)
  population = read.csv("data/LGA_population.csv")
  names(population)[2] <- "admin2Name"
  map <- sp::merge(map, population, by="admin2Name")
  map$full_area <- area(map)/1000000
  
  map <- subset(map, (map$admin1Name == 'Kano' | map$admin1Name == 'Katsina'))
  
  clusters <- data %>% 
    group_by(cluster) %>% 
    summarise(observations = n(), 
              price_kg = mean(price_kg)) %>% 
    filter(observations > 0, 
           cluster != "0") 
  clusters$pop <- NA
  clusters$area <- NA
  cluster_map = cluster_map_single = list()
  for(i in clusters$cluster){
    X <- cbind(data[data$cluster == i, ]$lon, data[data$cluster == i, ]$lat)
    border <- chull(X)
    
    if(length(border) > 2){
      border <- c(border, border[1])
      
      p = Polygon(X[border, ])
      ps = Polygons(list(p),1)
      sps = SpatialPolygons(list(ps))
      crs(sps) <- CRS("+proj=longlat +datum=WGS84")
      sps <- spTransform(sps, CRS("+init=epsg:32633"))
      sps = gBuffer(sps, width=buffer)
      cluster_map[[i]] <- sps
      
      map <- spTransform(map, CRS("+init=epsg:32633"))
      
      proj4string(sps) = proj4string(map)
      cluster <- intersect(sps, map)
      cluster$area <- area(cluster) / 1000000
      cluster$ratio <- round(cluster$area/cluster$full_area, 2)
      cluster$pop <- cluster$ratio*cluster$projected_2016
      cluster@data <- cluster@data %>% 
        filter(!is.na(projected_2016))
      clusters[i,]$area <- sum(cluster$area)
      clusters[i,]$pop <- sum(cluster$pop)
      cluster_map_single[[i]] <- cluster 
      
    }else{
      clusters[i,]$area <- 0
      clusters[i,]$pop <- 0
      cluster_map_single[[i]] <- NULL
    }
  }
  #clusters <- clusters[clusters$pop > 0, ]
  
  
  
  if(graph){
    
    plot(map, main=paste("Clusters"))
    for(i in 1:length(cluster_map)){
      if(!is.null(cluster_map_single[[i]])) plot(cluster_map[[i]], add=T, border="red", lwd=3)
    }
  }
  
  
  
  clusters <- clusters %>% 
    mutate(ratio = pop/observations, 
           optimal_obs = mean(ratio)*pop, 
           representation = ifelse(ratio<mean(ratio), "UNDER", "OVER")) %>% 
    dplyr::select(cluster, pop, area, price_kg, observations, optimal_obs, representation, ratio)
  
  
  #clusters$new <- clusters$optimal_obs/sum(clusters$optimal_obs)*sum(clusters$observations)
  
  price_ps <- sum((clusters$price_kg*clusters$ratio)/sum(clusters$ratio, na.rm=T), na.rm = TRUE)
  price_mu <- mean(data$price_kg)
  
  m <- clusters$observations
  n <- round(clusters$optimal_obs)
  m <- round(sum(n)/sum(m)*m)
  N <- sum(n)
  n_obs <- sum(clusters$observations)
  n_opt <-sum(round(clusters$optimal_obs))
  
  csr <- 1 - sum((m-n)^2) / (sum(n)^2 + 2*N*min(n) + N^2)
  cat('Post-sampling CSR:', csr, '\n')
  
  return(list(clusters=clusters, 
              price_ps=price_ps, 
              price_mu=price_mu,
              n_obs=n_obs,
              n_opt=n_opt,
              csr=csr))
}

