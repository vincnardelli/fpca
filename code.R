source("R/functions.R")


eps_dbs <- 0.0019  #~12 km --> radians = Km/6371
maxd_dbs <- 0.00784806  #50km
minPts_dbs_rtp <- 5 #min number of data points per cluster
minPts_dbs_fgp <- 4 #min number of data points per cluster
minPts_dbs_ws <- 5 #min number of data points per cluster
method_dbs <- "iqr" #Interquartile range method or "sigma" for standard dev method
h_dbs <- 2


data <- dataload()

#### step 0 - step 1 ----

step0 <- data$step0
step1 <- data$step1

save(step0, file="output/step0.Rdata")
save(step1, file="output/step1.Rdata")

# write.csv(step0, file="output/step0.csv")
# write.csv(step1, file="output/step1.csv")

####  step 2 ----

outlier_detection_wrapper <- function(data){
  
    tryCatch(
      expr = {
        
        if(head(data$price_type, 1) == "RTP") minPts_dbs <- minPts_dbs_rtp
        if(head(data$price_type, 1) == "FGP") minPts_dbs <- minPts_dbs_fgp
        if(head(data$price_type, 1) == "WS") minPts_dbs <- minPts_dbs_ws
        
        data <- outlier_detection(
          data,
          eps=eps_dbs,
          maxd = maxd_dbs, 
          h = h_dbs, 
          minPts = minPts_dbs,
          method = method_dbs,
          graph = FALSE, 
          outlier.rm = FALSE, 
          isolated.rm = FALSE)
        
        mu <- data %>% 
          filter(outlier == FALSE, cluster !=0) %>% 
          summarise(mean = mean(price_kg, na.rm=T)) %>% 
          pull(mean)
        data <- data %>% 
          mutate(add_outlier = case_when(price_kg > 2*mu ~ 1, 
                                         price_kg < mu/2 ~ 2))
      },
      error = function(e){
        cat("Error")
        return(data)
      }
    ) 

  data
}

# split step 1

today_date <- today()




start_date <- as_date("2000-08-20")
#today_date <- as_date("2021-07-13")
step1 <- step1 %>%
  filter(submission_date >= start_date, submission_date <= today_date)


step1_current_week <- step1 %>% 
  filter(prev_monday == floor_date(today_date, "week", 
         week_start = getOption("lubridate.week.start", 1)))


step1_historical <- step1 %>% 
  filter(prev_monday != floor_date(today_date, "week", 
         week_start = getOption("lubridate.week.start", 1)))

# historical

if (file.exists("output/step2_historical.Rdata")){
  load("output/step2_historical.Rdata")
  
  historical_weeks_to_add <- unique(step1$prev_monday[!(step1$prev_monday %in% step2_historical$prev_monday)])
  
    step2_historical_new <- step1_historical %>% 
      filter(prev_monday %in% historical_weeks_to_add) %>% 
      group_by(product, prev_monday, level1, price_type) %>% 
      group_split() %>% 
      purrr::map_df(outlier_detection_wrapper)
    
    step2_historical <- rbind(step2_historical, step2_historical_new)

}else{
  step2_historical <- step1_historical %>% 
    group_by(product, prev_monday, level1, price_type) %>% 
    group_split() %>% 
    purrr::map_df(outlier_detection_wrapper)
}

save(step2_historical, file="output/step2_historical.Rdata")

# current week
dates <- unique(as_date(step1_current_week$submission_date))

rolling <- function(date, step1){
  date <- as_date(date)
  step1 %>% 
    mutate(submission_date = as_date(submission_date)) %>% 
    filter(submission_date <= date, submission_date >= date - 7) %>% 
    group_by(product, level1, price_type) %>% 
    group_split() %>% 
    purrr::map_df(outlier_detection_wrapper) %>% 
    filter(submission_date == date)
}

step2_current_week <- map_df(dates, rolling, step1=step1)

step2 <- rbind(step2_historical, step2_current_week)

save(step2, file="output/step2.Rdata")
write.csv(step2, file="output/step2.csv", row.names = F)


#### step 3 ----

spatialpostsampling_wrapper <- function(data){
  tryCatch(
    expr = {
        sps <- spatialpostsampling(data)
        
        return(data.frame(level1=head(data$level1, 1),
                          submission_week=head(data$submission_week, 1),
                          submission_month=head(data$submission_month, 1),
                          submission_year=head(data$submission_year, 1),
                          prev_monday=head(data$prev_monday, 1),
                          product=head(data$product, 1),
                          price_type =head(data$price_type, 1),
                          price.mean=sps$price_mu,
                          price.ps=sps$price_ps,
                          se_price.mean = sps$se_price_mu,
                          sd_price.mean = sps$sd_price_mu,
                          se_price.ps = sps$se_price_ps,
                          sd_price.ps = sps$sd_price_ps,
                          n_price_mean = sps$n_price_mu,
                          n_price_ps = sps$n_price_ps,
                          CRI = sps$cri))
     
    },
    error = function(e){
      cat("Error")
    }
  
  )
}

step3_sps <- step2 %>% 
  filter(outlier == FALSE, is.na(add_outlier), cluster !=0) %>% 
  group_by(product, prev_monday, level1, price_type) %>% 
  group_split() %>% 
  purrr::map_df(spatialpostsampling_wrapper)

save(step3_sps, file="output/step3_sps.Rdata")
write.csv(step3_sps, file="output/step3_sps.csv", row.names = F)

global.postsampling_wrapper <- function(data){
  
  tryCatch(
    expr = {

        ps <- global.postsampling(data)
        
        return(data.frame(level1=head(data$level1, 1),
                          submission_week=head(data$submission_week, 1),
                          submission_month=head(data$submission_month, 1),
                          submission_year=head(data$submission_year, 1),
                          prev_monday = head(data$prev_monday, 1),
                          product=head(data$product, 1),
                          price_type = head(data$price_type, 1),
                          price.mean=ps$price_mu,
                          price.ps=ps$price_ps,
                          n_obs=ps$n_obs,
                          n_opt=ps$n_opt,
                          lga=ps$lga,
                          CRI=ps$cri))
      
    },
    error = function(e){
      cat("Error")
    }
    
  )
}


step3_gps <- step2 %>% 
  filter(outlier == FALSE, is.na(add_outlier), cluster !=0) %>% 
  group_by(product, prev_monday, level1, price_type) %>% 
  group_split() %>% 
  purrr::map_df(global.postsampling_wrapper)

save(step3_gps, file="output/step3_gps.Rdata")
write.csv(step3_gps, file="output/step3_gps.csv")

step3_gps_agg <- step3_gps %>% 
  select(-n_obs, -n_opt, -starts_with("lga")) %>% 
  unique()

save(step3_gps_agg, file="output/step3_gps_agg.Rdata")
write.csv(step3_gps_agg, file="output/step3_gps_agg.csv", row.names = F)



# Monthly analysis ----

spatialpostsampling_monthly_wrapper <- function(data){
  tryCatch(
    expr = {
      sps <- spatialpostsampling(data)
      
      return(data.frame(level1=head(data$level1, 1),
                        submission_week=head(data$submission_week, 1),
                        submission_month=head(data$submission_month, 1),
                        submission_year=head(data$submission_year, 1),
                        product=head(data$product, 1),
                        price_type =head(data$price_type, 1),
                        price.mean=sps$price_mu,
                        price.ps=sps$price_ps,
                        se_price.mean = sps$se_price_mu,
                        sd_price.mean = sps$sd_price_mu,
                        se_price.ps = sps$se_price_ps,
                        sd_price.ps = sps$sd_price_ps,
                        n_price_mean = sps$n_price_mu,
                        n_price_ps = sps$n_price_ps,
                        CRI = sps$cri))
      
    },
    error = function(e){
      cat("Error")
    }
    
  )
}

step3_sps_monthly <- step2 %>% 
  filter(outlier == FALSE, is.na(add_outlier), cluster !=0) %>% 
  group_by(product, submission_year, submission_month, level1, price_type) %>% 
  group_split() %>% 
  purrr::map_df(spatialpostsampling_monthly_wrapper)



global.postsampling_monthly_wrapper <- function(data){
  
  tryCatch(
    expr = {
      
      ps <- global.postsampling(data)
      
      return(data.frame(level1=head(data$level1, 1),
                        submission_week=head(data$submission_week, 1),
                        submission_month=head(data$submission_month, 1),
                        submission_year=head(data$submission_year, 1),
                        product=head(data$product, 1),
                        price_type = head(data$price_type, 1),
                        price.mean=ps$price_mu,
                        price.ps=ps$price_ps,
                        n_obs=ps$n_obs,
                        n_opt=ps$n_opt,
                        lga=ps$lga,
                        CRI=ps$cri))
      
    },
    error = function(e){
      cat("Error")
    }
    
  )
}


step3_gps_monthly <- step2 %>% 
  filter(outlier == FALSE, is.na(add_outlier), cluster !=0) %>% 
  group_by(product, submission_year, submission_month, level1, price_type) %>% 
  group_split() %>% 
  purrr::map_df(global.postsampling_monthly_wrapper)


step3_gps_agg_monthly <- step3_gps_monthly %>% 
  select(-n_obs, -n_opt, -starts_with("lga")) %>% 
  unique()
