
global.postsampling <- function(data){
  load("data/data.Rdata")
  
  #map = readOGR("data/maps/nga_admbnda_adm2_osgof", verbose=F)
  # map = readOGR("data/maps/nga_bnd_adm3wards_062015", verbose=F)
  # population = read.csv("data/LGA_population.csv")
  #names(population)[3] <- "level2Name"
  # names(population)[2] <- "LGAName"
  
  #map <- sp::merge(map, population, by="level2Name")
  # map <- sp::merge(map, population, by="LGAName")
  #   map$full_area <- area(map)/1000000
  # 
  lga <- data %>%
    group_by(level2) %>%
    summarise(price_kg=mean(price_kg), observations=n())   %>%
    left_join(map, by="level2") %>% 
    dplyr::select(level2, price_kg, observations, pop=population) %>%
    filter(!is.na(pop))
    lga[is.na(lga)] <- 0
  lga <-  lga[!duplicated(lga$level2), ]
  
  lga$ratio <-  lga$pop/lga$observations
  mu <- mean(lga$ratio[lga$ratio < Inf])
  #mu <- mean(lga$ratio[!is.na(lga$ratio)])
  lga$optimal_obs <- mu*lga$pop
  
  price_ps <- sum((lga$price_kg*lga$ratio)[lga$ratio < Inf]/sum(lga$ratio[lga$ratio < Inf], na.rm=T), na.rm = TRUE)
  #price_ps <- sum((lga$price_kg*lga$ratio)[!is.na(lga$ratio)]/sum(lga$ratio[!is.na(lga$ratio)], na.rm=T), na.rm = TRUE)
  sd_price_ps <- sqrt(sum((lga$price_kg - mean(lga$price_kg))^2*lga$ratio)/sum(lga$ratio))
  n_price_ps <- nrow(lga[lga$ratio < Inf, ])  
  se_price_ps <- sd_price_ps / sqrt(n_price_ps)

  #state mean
  # price_mu <- mean(data$price_kg)
  # se_price_mu <- std.error(data$price_kg)
  # n_price_mu <- nrow(data)
  # sd_price_mu <- se_price_mu * sqrt(n_price_mu)
  #state mean over lga
  
  std.error <- function(x)  sd(x)/sqrt(sum(!is.na(x)))
  
  price_mu <- mean(lga$price_kg[lga$ratio < Inf])
  se_price_mu <- std.error(lga$price_kg[lga$ratio < Inf])
  n_price_mu <- nrow(lga[lga$ratio < Inf, ])
  sd_price_mu <- se_price_mu * sqrt(n_price_mu)
   
  
  n <- lga$observations
  m <- round(lga$optimal_obs)
  m <- round(sum(n)/sum(m)*m)
  N <- sum(n)
  n_obs <- sum(lga$observations)
  n_opt <-sum(round(lga$optimal_obs))
  
  cri <- 1 - sum((m-n)^2) / (sum(n)^2 + 2*N*min(n) + N^2)
  cat('Global Post-sampling CRI:', cri, '\n') 
  return(list(lga=lga,
              price_ps=price_ps, 
              price_mu=price_mu, 
              n_obs=n_obs,
              n_opt=n_opt,
              se_price_mu = se_price_mu,
              se_price_ps = se_price_ps,
              sd_price_mu = sd_price_mu,
              sd_price_ps = sd_price_ps,
              cri=cri))
}

