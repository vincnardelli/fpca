outlier_detection <- function(data, 
                              h=2, 
                              method = "sigma",
                              graph=FALSE, 
                              eps, 
                              minPts=3, 
                              maxd=0.1, 
                              outlier.rm = TRUE, 
                              isolated.rm = FALSE){
  
  if(graph){
    map = readOGR("data/maps/nga_admbnda_adm2_osgof/", verbose=F)
    base_map <- ggplot() + 
      geom_polygon(data = map, 
                   aes(x = long, y = lat, group = group), 
                   colour = "gray80", 
                   fill="white", 
                   size=0.2) +
      coord_map(xlim = c(7, 9),ylim = c(11.2, 13.2))
    
    
    base_map + geom_point(data= data, 
                          aes(x=lon, y=lat)) + 
      theme_void() 
  }
  
  
  
  data$outlier <- FALSE
  dbscan <- dbscan(cbind(data$lon, data$lat), eps=eps, minPts= minPts)
  cat("1 - DBSCAN ( eps", eps, "| min data", minPts, ")\n")
  cat("N. of clusters:", max(unique(dbscan$cluster)), "\n")
  cat("N. of isolated points:", sum(dbscan$cluster==0), "\n\n")
  
  data$cluster <- as.factor(dbscan$cluster)
  data$relocation <- FALSE
  
  # enters a cluster
  cat("2 - Enters a cluster ( h", h, ")\n")
  if(max(unique(dbscan$cluster))==0){
    cat("ERROR NO CLUSTER \n")
    return(data)
  }
  for(cluster_id in 1:max(as.numeric(data$cluster)-1)){
    cluster_subset <- data %>% 
      filter(cluster==cluster_id)
    cluster_subset$outlier <- FALSE
    n <- nrow(cluster_subset)
    for (i in 1:n) {
      mu <- mean(cluster_subset$price_kg)
      sigma <- sd(cluster_subset$price_kg)
      value <- cluster_subset$price_kg[i]
      
      if(method=="sigma"){
        par <- sd(cluster_subset$price_kg)
        
        if (mu - (par * h) > value | value > mu + (par * h)) {
          cluster_subset$outlier[i] <- TRUE
        }
      }else if(method == "iqr"){
        par <- IQR(cluster_subset$price_kg)
        q1 <- quantile(cluster_subset$price_kg, 0.25)
        q3 <- quantile(cluster_subset$price_kg, 0.75)
        
        if (q1 - (par * h) > value | value > q3 + (par * h)) {
          cluster_subset$outlier[i] <- TRUE
        }
      }
      

    }
    data$outlier[data$cluster==cluster_id] <- cluster_subset$outlier
    if(graph){
      if(nrow(cluster_subset)>0){
        graph_print <- ggplot() +
          theme_minimal() +
        geom_point(data=cluster_subset, aes(x=id_form, y=price_kg, col=outlier)) + 
          ggtitle(paste("Outlier prices graph for cluster", cluster_id))
      print(graph_print)
      }
    }
  }
  
  cat("N. of outlier:", sum(data$outlier), "\n")
  cat("% of outlier:", mean(data$outlier), "\n\n")
  
  if(graph){
    graph_print <- base_map + geom_point(data= data[data$cluster!=0,], 
                                         aes(x=lon, y=lat, col=cluster), size=2) +
      geom_point(data= data[data$cluster==0,], 
                 aes(x=lon, y=lat, col="black"), shape=4, size=2) + 
      theme_void() +
      theme(legend.position="none") +
      ggtitle("Before")
    print(graph_print)
  }
  
  # isolated point
  
  clusters <- data %>% 
    filter(cluster!=0) %>% 
    group_by(cluster) %>% 
    summarise(lon=mean(lon), lat=mean(lat), mean=mean(price_kg), sd=sd(price_kg), n=n())
  cat("3 - Isolated point ( h", h, "| maxd", maxd, ")\n")
  for(i in which(data$cluster==0)){
    
    cluster_comparison <- clusters
    
    dist <- as.matrix(dist(rbind(cbind(data$lon[i], data$lat[i]), 
                                 cbind(clusters$lon, clusters$lat)), 
                           method="euclidean"))[-1,1]
    
    cluster_comparison$dist <- dist
    cluster_comparison$price_kg <- data$price_kg[i]
    new_cluster <- cluster_comparison %>% 
      filter(dist < maxd) %>% 
      mutate(enter=ifelse(mean - (sd * h) > price_kg | price_kg > mean + (sd * h), FALSE, TRUE)) %>% 
      arrange(dist) %>% 
      filter(enter==TRUE) %>% 
      head(n=1) %>% 
      dplyr::select(cluster)
    new_cluster <- as.numeric(new_cluster)-1
    if(!is.na(new_cluster)){
      data$cluster[i] <- new_cluster
      data$relocation[i] <- TRUE

      cat("Isolated point", i, "relocated to cluster", new_cluster, "\n")
    }
  }
  
  cat("\nN. of isolated points after relocation:", sum(data$cluster==0), "\n")
  
  if(graph){
    graph_print <- base_map + geom_point(data= data[data$cluster!=0,], 
                                   aes(x=lon, y=lat, col=cluster), size=2) +
      theme(legend.position="none") +
      ggtitle("After")
    
    if(sum(data$cluster==0)>0){
      graph_print <- graph_print + 
        geom_point(data= data[data$cluster==0,], 
                 aes(x=lon, y=lat, col="black"), shape=4, size=2) +
      theme_void() 
    }
    print(graph_print)
  }
  
  if(outlier.rm){
    data <- data %>% 
      filter(outlier == FALSE)
  }
  if(isolated.rm){
    data <- data %>% 
      filter(cluster != 0)
  }
  
   return(data)
}
