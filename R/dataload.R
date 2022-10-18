dataload <- function(end.day = NA, 
                      end.month = NA, 
                      end.year = NA,
                      start.day = NA, 
                      start.month = NA, 
                      start.year = NA){
  
  cat("Retrieving data from ONA API...")
  # ADD HERE THE ID ON THE URL REQUEST
  #url <- "https://api.ona.io/api/v1/data/{id}?"
  if(!is.na(end.day) & !is.na(end.month) & !is.na(end.year)){
    url <- paste0(url, 
                  "&date_created__day__lte=", end.day, 
                  "&date_created__month__lte=", end.month, 
                  "&date_created__year__lte=", end.year)
  }
  if(!is.na(start.day) & !is.na(start.month) & !is.na(start.year)){
    url <- paste0(url, 
                  "&date_created__day__gte=", start.day, 
                  "&date_created__month__gte=", start.month, 
                  "&date_created__year__gte=", start.year)
  }
# ADD HERE AUTHORIZATION TOKEN
#  request <- GET(url,
#                 add_headers(authorization= ""))

  request
  # 
  # save(request, file="request.Rdata")
  data <- jsonlite::fromJSON(content(request, type="text", encoding="UTF-8"))
  data
  
  print(data)
  # data <- data %>%
  #   mutate(start = as_date(start)) %>%
  #   filter(start > as_date(start_date), start < as_date(stop_date))
  # nrow(data)
  
  extract_products <- function(data){
    
    convert_to_string <- function(x, col){
      id_form <- which(colnames(x) == col)
      x[,id_form] <- as.character(x[,id_form])
      return(x)
    }
    
    
    others_names <- c("Section_B1/other_package_Ymaize",
                      "Section_B2/other_package_Wmaize",
                      "Section_B3/other_package_Thailand",
                      "Section_B4/other_package_Indian",
                      "Section_A/other_market")
    
    
    new_cols <- others_names[!(others_names %in% colnames(data))]
    
    
    b5_names <- data %>%
      dplyr::select('_id', starts_with("Section_B5/")) %>%
      dplyr::mutate("Section_B5/repeat_local_rice" = lapply(.$"Section_B5/repeat_local_rice",
                                                            convert_to_string, col="Section_B5/repeat_local_rice/price_rice_local")) %>%
      tidyr::unnest(`Section_B5/repeat_local_rice`) %>%
      colnames()
    
    if(!("Section_B5/repeat_local_rice/other_package_local" %in% b5_names)){
      new_cols <- c(new_cols, "Section_B5/repeat_local_rice/other_package_local")
    }
    
    b6_names <- data %>%
      dplyr::select('_id', starts_with("Section_B6/")) %>%
      dplyr::mutate("Section_B6/repeat_redbeans" = lapply(.$"Section_B6/repeat_redbeans",
                                                          convert_to_string, col="Section_B6/repeat_redbeans/price_red_beans")) %>%
      tidyr::unnest(`Section_B6/repeat_redbeans`) %>%
      colnames()
    
    if(!("Section_B6/repeat_redbeans/other_package_local" %in% b6_names)){
      new_cols <- c(new_cols, "Section_B6/repeat_redbeans/other_package_local")
    }
    
    b7_names <- data %>%
      dplyr::select('_id', starts_with("Section_B7/")) %>%
      dplyr::mutate("Section_B7/repeat_whitebeans" = lapply(.$"Section_B7/repeat_whitebeans",
                                                            convert_to_string, col="Section_B7/repeat_whitebeans/price_White_beans")) %>%
      tidyr::unnest(`Section_B7/repeat_whitebeans`) %>%
      colnames()
    
    if(!('Section_B7/repeat_whitebeans/other_package_local' %in% b7_names)){
      new_cols <- c(new_cols, 'Section_B7/repeat_whitebeans/other_package_local')
    }
    
    
    new_cols_df <- data.frame(matrix(NA, nrow = nrow(data), ncol=length(new_cols)))
    colnames(new_cols_df) <- new_cols
    
    data <- cbind(data, new_cols_df)
    
    
    
    b1 <- data %>% 
      select('_id', starts_with("Section_B1/")) %>% 
      mutate(product = "maize_yellow", 
             type = NA) %>% 
      select(id_form = '_id', 
             product,
             type,
             packaging = 'Section_B1/packaging_Ymaize', 
             other_packaging = 'Section_B1/other_package_Ymaize',
             price = 'Section_B1/price_Ymaize') 
    
    b2 <- data %>% 
      select('_id', starts_with("Section_B2/")) %>% 
      mutate(product = "maize_white", 
             type = NA) %>% 
      select(id_form = '_id', 
             product,
             type,
             packaging = 'Section_B2/packaging_Wmaize', 
             other_packaging = 'Section_B2/other_package_Wmaize',
             price = 'Section_B2/price_Wmaize') 
    
    b3 <- data %>% 
      select('_id', starts_with("Section_B3/")) %>% 
      mutate(product = "thailand_rice", 
             type = NA) %>% 
      select(id_form = '_id', 
             product,
             type,
             packaging = 'Section_B3/packaging_Thailandrice', 
             other_packaging = 'Section_B3/other_package_Thailand',
             price = 'Section_B3/price_Thailand') 
    
    b4 <- data %>% 
      select('_id', starts_with("Section_B4/")) %>% 
      mutate(product = "indian_rice", 
             type = NA) %>% 
      select(id_form = '_id', 
             product,
             type,
             packaging = 'Section_B4/packaging_Indianrice', 
             other_packaging = 'Section_B4/other_package_Indian',
             price = 'Section_B4/price_Indian') 
    
    b5 <- data %>% 
      select('_id', starts_with("Section_B5/")) %>% 
      mutate("Section_B5/repeat_local_rice" = lapply(.$"Section_B5/repeat_local_rice", 
                                                     convert_to_string, col="Section_B5/repeat_local_rice/price_rice_local")) %>% 
      unnest(`Section_B5/repeat_local_rice`) %>% 
      mutate(product = "local_rice") %>% 
      select(id_form = '_id', 
             product,
             type = 'Section_B5/repeat_local_rice/local_type_list',
             packaging = 'Section_B5/repeat_local_rice/packaging_local_rice', 
             other_packaging = 'Section_B5/repeat_local_rice/other_package_local',
             price = 'Section_B5/repeat_local_rice/price_rice_local')
    
    
    b6 <- data %>% 
      select('_id', starts_with("Section_B6/")) %>% 
      mutate("Section_B6/repeat_redbeans" = lapply(.$"Section_B6/repeat_redbeans", 
                                                   convert_to_string, col="Section_B6/repeat_redbeans/price_red_beans")) %>% 
      unnest(`Section_B6/repeat_redbeans`) %>% 
      mutate(product = "red_beans") %>% 
      select(id_form = '_id', 
             product,
             type = 'Section_B6/repeat_redbeans/local_type_list',
             packaging = 'Section_B6/repeat_redbeans/packaging_redbeans', 
             other_packaging = 'Section_B6/repeat_redbeans/other_package_local',
             price = 'Section_B6/repeat_redbeans/price_red_beans')
    
    
    
    b7 <- data %>% 
      select('_id', starts_with("Section_B7/")) %>% 
      mutate("Section_B7/repeat_whitebeans" = lapply(.$"Section_B7/repeat_whitebeans", 
                                                     convert_to_string, col="Section_B7/repeat_whitebeans/price_White_beans")) %>% 
      unnest(`Section_B7/repeat_whitebeans`) %>% 
      mutate(product = "white_beans") %>% 
      select(id_form = '_id', 
             product,
             type = 'Section_B7/repeat_whitebeans/local_type_list',
             packaging = 'Section_B7/repeat_whitebeans/packaging_whitebeans', 
             other_packaging = 'Section_B7/repeat_whitebeans/other_package_local',
             price = 'Section_B7/repeat_whitebeans/price_White_beans')
    
    
    b8 <- data %>% 
      select('_id', starts_with("Section_B8/")) %>% 
      mutate(product = "soybean", 
             type = NA, 
             other_packaging = NA) %>% 
      select(id_form = '_id', 
             product,
             type,
             packaging = 'Section_B8/packaging_soybean', 
             other_packaging,
             price = 'Section_B8/price_soybean') 
    
    b9 <- data %>% 
      select('_id', starts_with("Section_B9/")) %>% 
      unnest(`Section_B9/repeat_garri`) %>% 
      mutate(product = "garri", 
             other_packaging = NA) %>% 
      select(id_form = '_id', 
             product,
             type = 'Section_B9/repeat_garri/garri_types_list',
             packaging = 'Section_B9/repeat_garri/packaging_garri', 
             other_packaging,
             price = 'Section_B9/repeat_garri/price_garri')
    
    
    b10 <- data %>% 
      select('_id', starts_with("Section_B10/")) %>% 
      mutate(product = "soybean", 
             type = NA, 
             other_packaging = NA) %>% 
      select(id_form = '_id', 
             product,
             type,
             packaging = 'Section_B10/packaging_soybean',
             other_packaging,
             price = 'Section_B10/price_soybean')
    
    b <- rbind(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10) %>% 
      filter(!is.na(packaging) | !is.na(price)) %>% 
      mutate(price = as.numeric(price))
    return(b)
  }
  
  products <- extract_products(data) %>% 
    left_join(packaging_reclassification, by = c("packaging", "other_packaging")) %>% 
    mutate(packaging = ifelse(packaging == 'Other', packaging_new, packaging)) %>% 
    dplyr::filter(!is.na(packaging)) %>% 
    dplyr::select(-other_packaging, -packaging_new) %>% 
    left_join(conversion_factor, by = "packaging") %>% 
    dplyr::filter(!is.na(conversion)) %>% 
    mutate(price_observed = price, 
           price_kg = price_observed*conversion) %>% 
    dplyr::select(-price) %>% 
    dplyr::filter(!is.na(price_kg)) %>% 
    group_by(id_form) %>% 
    mutate(count = 1:n(), 
           id = paste0(id_form, count)) %>% 
    select(-count) %>% 
    ungroup() 
  
  
  # check duplicates
  id_forms <- products %>% 
    group_by(id_form, product, type, packaging) %>% 
    summarise(n = n()) %>% 
    filter(n > 1) %>% 
    pull(id_form) 
  
  products <- filter(products, !(id_form %in% id_forms))
  
  forms_step0 <- data %>% 
    select(id_form = "_id", 
           start,
           volunteer_id = "VC_ID",
           deviceid = "deviceid", 
           submission_time = "_submission_time", 
           gps = "Section_A/gps",
           market_type = "Section_A/market_type", 
           other_market = "Section_A/other_market", 
           buying = "Section_A/buying", 
           buying_purpose = "Section_A/buying_purpose", 
           market_distance = "Section_A/market_distance", 
           seller_phone = "seller_phone", 
           time_start = "timeStart")
  
  step0 <- products %>% 
    inner_join(forms_step0, by = "id_form")
  
  
  forms <- data %>% 
    select(id_form = "_id", 
           start,
           volunteer_id = "VC_ID",
           deviceid = "deviceid", 
           submission_time = "_submission_time", 
           gps = "Section_A/gps",
           market_type = "Section_A/market_type", 
           other_market = "Section_A/other_market", 
           buying = "Section_A/buying", 
           buying_purpose = "Section_A/buying_purpose", 
           market_distance = "Section_A/market_distance", 
           seller_phone = "seller_phone", 
           time_start = "timeStart") %>% 
    rowwise() %>% 
    mutate(gps = str_split(gps, pattern=" "), 
           latitude = as.numeric(gps[1]), 
           longitude =  as.numeric(gps[2]), 
           lat = as.numeric(gps[1]), 
           lon =  as.numeric(gps[2]), 
           alt =  as.numeric(gps[3]), 
           pre =  as.numeric(gps[4])) %>% 
    select(-gps) %>% 
    filter(!is.na(lat) & !is.na(lon)) %>% 
    st_as_sf(coords = c('longitude', 'latitude'), crs = 4326) %>% 
    st_join(map, join = st_within) %>% 
    filter(!is.na(level1)) %>% 
    left_join(market_reclassification, by = "other_market") %>% 
    mutate(market_type = ifelse(market_type == 'Other', other_market_new, market_type)) %>% 
    select(-other_market, -other_market_new, -geometry) %>% 
    filter(!is.na(market_type)) %>% 
    mutate(market_type_cat = case_when(market_type == "Supermarket" ~ "Large shops", 
                                       market_type == "Neighborhood_shops_kiosk" ~ "Medium and small shops", 
                                       market_type %in% c("Open air_or_covered market",
                                                          "Local_village_market", 
                                                          "City_market") ~ "Markets", 
                                       market_type == "Mobile_shops_street vendors" ~ "Street outlets", 
                                       market_type == "Bulk_and_discount stores" ~ "Bulk and discount stores", 
                                       market_type == "Specialized_stores" ~ "Specialized stores", 
                                       market_type == "Directly_from_Farmer" ~ "Directly from farmer", 
                                       market_type == "Online" ~ "Online"), 
           price_type = case_when(market_type == "Directly_from_Farmer" ~ "FGP", 
                                  market_type == "Bulk_and_discount stores" ~ "WS", 
                                  TRUE ~ "RTP"), 
           submission_date = as_date(submission_time), 
           submission_week  = isoweek(submission_time), 
           submission_month  =  month(submission_time),
           submission_year  =  year(submission_time),
           prev_monday  =  floor_date(submission_date, "week", 
                                      week_start = getOption("lubridate.week.start", 1))
           
    ) %>% 
    st_drop_geometry()
  
  
  
  
  step1 <- products %>% 
    inner_join(forms, by = "id_form") %>% 
    mutate(price_type = case_when(packaging %in% c("50kg bag", "100kg bag") & 
                                    market_type_cat %in% c("Markets", "Street outlets") ~ "WS", 
                                  packaging == "Mudu/Kwano/Tiyya" & market_type_cat == "Directly from farmer" ~ "RTP", 
                                  TRUE ~ price_type))
  
  
  
  return(list(request=request,
              data=data,
              step0 = step0,
              step1 = step1))
}
