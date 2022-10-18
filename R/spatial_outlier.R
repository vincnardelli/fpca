spatial_outlier <- function(data, maxd, h=2, outlier.rm = TRUE){
  data$spatialoutlier <- FALSE
  coord  <- cbind(data$lat, data$lon)
  nb<-spdep::dnearneigh(coord,0,maxd, longlat=FALSE)
  isolated <- 0
  for(i in 1:nrow(data)){
    if(length(nb[[i]]) > 2){
      price_nb <- data[nb[[i]], ]$price_kg
      price_lag <- mean(price_nb)
      price_lagsd <- sd(price_nb)
      data$spatialoutlier[i] <- data$price_kg[i] > price_lag+price_lagsd*h | data$price_kg[i] <  price_lag-price_lagsd*h
    }else{
      isolated <- isolated + 1
    }
  }
  cat('Isolated points ', isolated, '\n')
  cat('Outlier detected', sum(data$spatialoutlier), '\n')
  
  if(outlier.rm){
    data <- data %>% 
      filter(spatialoutlier == FALSE)
  }
  
  return(data)
}
