##########################################################################################
pre_process_PCR_GLOBWB = function(df, withdrawal_based){
  #Reference for unit conversion: https://www.researchgate.net/post/How-to-convert-cmip5-monthly-precipitation-kg-m2-s1-into-mm-month
  if(withdrawal_based == T) {
    print("Assuming no environmental flow requirements")
    env_factor = 0 #proportion of runoff left for environmental demands
  } else {
    env_factor = 0.4 #proportion of runoff left for environmental demands
  }
  
  #Monthly
  output = 
    df %>%
    tidytable::select(lon:cell_id, qs, qsb, adomuse:pirrww) %>%
    #filter(year>2000) %>% #Subset to the recent 5 years
    tidytable::mutate(across(qs:pirrww, ~ .x*86400)) %>% #(kg m-2 s-1) to (kg m-2 day-1)
    tidytable::mutate(across(qs:pirrww, ~ .x*(days/1000))) %>% #(kg m-2 day-1) to (kg m-2 month-1) # kg m-2 == (1/1000) m 
    tidytable::mutate(bwa_m_month = (1-env_factor)*(qs + qsb),
                      gwa_m_month = airrusegreen + arainfusegreen) 
  
  print("Data subsetted; converted units; estimated blue/green water availability")
  
  return(output)
}

##########################################################################################

pixel_level_scarcity = function(df, withdrawal_based, agg_func = 'median'){
  print("0 - Kicking things off")
  if(withdrawal_based == T) {
    #Get yearly water metrics by pixel by year
    output = 
      df %>%
      merge(ref_raster_df, by = c('lon', 'lat'), all.x=T) %>%
      merge(ds2_transform, by = c('lon', 'lat', "year", "month"), all.x=T) %>%
      dplyr::select(lon, lat, area:REGION_WB, year, month, gwa_m_month, bwa_m_month, airrww, aindww, aliveww, adomww,
                    airruse, ainduse, aliveuse, adomuse, pirruse, ETc) 
    ##########
    print("1 - Datasets merged; variable subset taken")
    ##########
    output = 
      output %>%
      tidytable::mutate(across(airrww:pirruse, ~ .x*area, .names = "{.col}_m3"),
                        ETc_m3 = ETc,
                        gwa_m3 = gwa_m_month*area,
                        bwa_m3 = bwa_m_month*area) %>%
      tidytable::mutate(actual_water_withdrawal_m3 = airrww_m3+aindww_m3+aliveww_m3+adomww_m3,
                        actual_water_consumed_m3 = airruse_m3+ainduse_m3+aliveuse_m3+adomuse_m3) 
    ##########
    print("2 - Values converted to volumes; withdrawal and consumption calculated")
    ##########
    output = 
      output %>%
      #Calculate scarcity at the monthly-scale
      tidytable::mutate(gws_pixel = (ETc_m3 - gwa_m3)/ETc_m3, #Potential Irrigation Water Requirements to Potential Water Demands
                        bws_pixel = actual_water_withdrawal_m3/bwa_m3 #Irrigation Water Consumption to Blue Water Availability
      ) 
    ##########
    print("3 - Monthly scarcity estimated for each pixel")
    ##########
    output = 
      output %>%
      tidytable::mutate(gws_pixel = ifelse(gws_pixel<0, 0, gws_pixel)) %>% #Incase the gwa exceeds ETc
      tidytable::mutate(gws_pixel_bin = ifelse(gws_pixel > 0.1, 1, 0), #Estimate the monthly stressed regions
                        bws_pixel_bin = ifelse(bws_pixel > 1, 1, 0)) 
    ##########
    print("4 - Scarcity values converted to binary for each month for each pixel")
    ##########
    
    if(agg_func == 'median'){
      output = 
        output %>%
        tidytable::group_by(lon, lat, WB_NAME, ISO_A3, year) %>% #Convert from monthly to yearly by taking the sum
        tidytable::summarise(across(airrww_m3:actual_water_consumed_m3, ~ sum(.x, na.rm = T), .names = "{.col}_year"),
                             across(gws_pixel:bws_pixel, ~median(.x, na.rm = T)),
                             across(gws_pixel_bin:bws_pixel_bin, ~ sum(.x, na.rm = T), .names = "{.col}_year"))
      
      ##########
      print("5 - Monthly values converted to yearly; using median")
      ##########
    } else if (agg_func == 'max'){
      output = 
        output %>%
        tidytable::group_by(lon, lat, WB_NAME, ISO_A3, year) %>% #Convert from monthly to yearly by taking the sum
        tidytable::summarise(across(airrww_m3:actual_water_consumed_m3, ~ sum(.x, na.rm = T), .names = "{.col}_year"),
                             across(gws_pixel:bws_pixel, ~max(.x, na.rm = T)),
                             across(gws_pixel_bin:bws_pixel_bin, ~ sum(.x, na.rm = T), .names = "{.col}_year"))
      
      ##########
      print("5 - Monthly values converted to yearly; using max")
      ##########
    }
    ##########
    output = 
      output %>%
      #Calculate scarcity at the yearlu-scale
      tidytable::mutate(gws_pixel_year = (ETc_m3_year - gwa_m3_year)/ETc_m3_year, 
                        bws_pixel_year = actual_water_withdrawal_m3_year/bwa_m3_year) %>%
      tidytable::mutate(gws_pixel_year = ifelse(gws_pixel_year<0, 0, gws_pixel_year)) %>% #Incase the gwa exceeds ETc
      tidytable::mutate(gws_atleast1_bin = ifelse(gws_pixel_bin_year >= 1, 1, 0), #Get pixels with atleast 1 month of scarcity
                        bws_atleast1_bin = ifelse(bws_pixel_bin_year >= 1, 1, 0)) %>%
      tidytable::mutate(gws_pixel_year_bin = ifelse(gws_pixel_year > 0.1, 1, 0), #Get the bin value using yearly pixel level scarcity metrics
                        bws_pixel_year_bin = ifelse(bws_pixel_year > 1, 1, 0)) %>%
      tidytable::filter(!is.na(WB_NAME)) %>%
      merge(df_crop, by = c('lon', 'lat'), all.x=T) #Add a cropland layer to remove low ag regions
    
    ##########
    print("6 - Yearly scarcity calculated (incl. binary); crop layer merged")
    ##########
    
  } else {
    
    #Get yearly water metrics by pixel by year
    output = 
      df %>%
      merge(ref_raster_df, by = c('lon', 'lat'), all.x=T) %>%
      merge(ds2_transform, by = c('lon', 'lat', "year", "month"), all.x=T) %>%
      tidytable::select(lon, lat, area:REGION_WB, year, month, gwa_m_month, bwa_m_month, airrww, aindww, aliveww, adomww,
                        airruse, ainduse, aliveuse, adomuse, pirruse, ETc) 
    ##########
    print("1 - Datasets merged; variable subset taken")
    ##########
    output = 
      output %>%
      tidytable::mutate(across(airrww:pirruse, ~ .x*area, .names = "{.col}_m3"),
                        ETc_m3 = ETc,
                        gwa_m3 = gwa_m_month*area,
                        bwa_m3 = bwa_m_month*area) %>%
      tidytable::mutate(actual_water_withdrawal_m3 = airrww_m3+aindww_m3+aliveww_m3+adomww_m3,
                        actual_water_consumed_m3 = airruse_m3+ainduse_m3+aliveuse_m3+adomuse_m3) 
    ##########
    print("2 - Values converted to volumes; withdrawal and consumption calculated")
    ##########
    output = 
      output %>%
      #Calculate scarcity at the monthly-scale
      tidytable::mutate(gws_pixel = (ETc_m3 - gwa_m3)/ETc_m3, #Potential Irrigation Water Requirements to Potential Water Demands
                        bws_pixel = actual_water_consumed_m3/bwa_m3 #Irrigation Water Consumption to Blue Water Availability
      ) 
    ##########
    print("3 - Monthly scarcity estimated for each pixel")
    ##########
    output = 
      output %>%
      tidytable::mutate(gws_pixel = ifelse(gws_pixel<0, 0, gws_pixel)) %>% #Incase the gwa exceeds ETc
      tidytable::mutate(gws_pixel_bin = ifelse(gws_pixel > 0.1, 1, 0), #Estimate the monthly stressed regions
                        bws_pixel_bin = ifelse(bws_pixel > 1, 1, 0)) 
    ##########
    print("4 - Scarcity values converted to binary for each month for each pixel")
    ##########
    
    if(agg_func == 'median'){
      output = 
        output %>%
        tidytable::group_by(lon, lat, WB_NAME, ISO_A3, year) %>% #Convert from monthly to yearly by taking the sum
        tidytable::summarise(across(airrww_m3:actual_water_consumed_m3, ~ sum(.x, na.rm = T), .names = "{.col}_year"),
                             across(gws_pixel:bws_pixel, ~median(.x, na.rm = T)),
                             across(gws_pixel_bin:bws_pixel_bin, ~ sum(.x, na.rm = T), .names = "{.col}_year"))
      
      ##########
      print("5 - Monthly values converted to yearly; using median")
      ##########
    } else if (agg_func == 'max'){
      output = 
        output %>%
        tidytable::group_by(lon, lat, WB_NAME, ISO_A3, year) %>% #Convert from monthly to yearly by taking the sum
        tidytable::summarise(across(airrww_m3:actual_water_consumed_m3, ~ sum(.x, na.rm = T), .names = "{.col}_year"),
                             across(gws_pixel:bws_pixel, ~max(.x, na.rm = T)),
                             across(gws_pixel_bin:bws_pixel_bin, ~ sum(.x, na.rm = T), .names = "{.col}_year"))
      
      ##########
      print("5 - Monthly values converted to yearly; using max")
      ##########
    }
    
    ##########
    output = 
      output %>%
      #Calculate scarcity at the yearlu-scale
      tidytable::mutate(gws_pixel_year = (ETc_m3_year - gwa_m3_year)/ETc_m3_year, 
                        bws_pixel_year = actual_water_consumed_m3_year/bwa_m3_year) %>%
      tidytable::mutate(gws_pixel_year = ifelse(gws_pixel_year<0, 0, gws_pixel_year)) %>% #Incase the gwa exceeds ETc
      tidytable::mutate(gws_atleast1_bin = ifelse(gws_pixel_bin_year >= 1, 1, 0), #Get pixels with atleast 1 month of scarcity
                        bws_atleast1_bin = ifelse(bws_pixel_bin_year >= 1, 1, 0)) %>%
      tidytable::mutate(gws_pixel_year_bin = ifelse(gws_pixel_year > 0.1, 1, 0), #Get the bin value using yearly pixel level scarcity metrics
                        bws_pixel_year_bin = ifelse(bws_pixel_year > 1, 1, 0)) %>%
      tidytable::filter(!is.na(WB_NAME)) %>%
      merge(df_crop, by = c('lon', 'lat'), all.x=T) #Add a cropland layer to remove low ag regions
    
    ##########
    print("6 - Yearly scarcity calculated (incl. binary); crop layer merged")
    ##########
    
  }
  
  return(output)
}

##########################################################################################
country_level_scarcity = function(df, withdrawal_based, cropland_threshold = F){
  print("0 - Kicking things off")
  
  if(cropland_threshold == T){
    print("1 - Applying cropland threshold")
    #Get yearly water metrics by country by year
    output = 
      df %>%
      #Essentially remove the non-croplands; this prevents regions with limited cropped area
      #from skewing the aggregated metrics
      tidytable::mutate(gws_pixel = ifelse(cropland_5 == 1, gws_pixel, NA),
                        gws_pixel_bin_year = ifelse(cropland_5 == 1, gws_pixel_bin_year, NA),
                        gws_pixel_year = ifelse(cropland_5 == 1, gws_pixel_year, NA),
                        gws_pixel_year_bin = ifelse(cropland_5 == 1, gws_pixel_year_bin, NA),
                        gws_atleast1_bin = ifelse(cropland_5 == 1, gws_atleast1_bin, NA))
  } else {
    print("1 - NOT Applying cropland threshold")
    #Get yearly water metrics by country by year
    output = 
      df
  }
  
  if(withdrawal_based == T) {
    
    output = 
      output %>%
      tidytable::group_by(year, WB_NAME, ISO_A3) %>%
      tidytable::summarise(across(airrww_m3_year:actual_water_consumed_m3_year, ~ sum(.x, na.rm = T)),
                           across(gws_pixel:bws_pixel_year, ~median(.x, na.rm = T), .names = "{.col}_median"),
                           across(gws_atleast1_bin:bws_pixel_year_bin, ~ sum(.x, na.rm = T)),
                           total_pixels = n()) 
    
    ##########
    print("2 - Pixel values aggregated to country-level")
    ##########
    
    output = 
      output %>%
      tidytable::mutate(gws_year_country_4 = (ETc_m3_year - gwa_m3_year)/ETc_m3_year, #Potential Irrigation Water Consumption to Potential Water Demands
                        bws_year_country_4 = actual_water_withdrawal_m3_year/bwa_m3_year, #Irrigation Water Consumption to Blue Water Availability
                        gws_area_scarce_5 = gws_pixel_year_bin/total_pixels,
                        bws_area_scarce_5 = bws_pixel_year_bin/total_pixels,
                        gws_area_atleast1_6 = gws_atleast1_bin/total_pixels,
                        bws_area_atleast1_6 = bws_atleast1_bin/total_pixels) %>%
      tidytable::mutate(gws_year_country_4 = ifelse(gws_year_country_4<0, 0, gws_year_country_4)) 
    
    ##########
    print("3 - Country-level scarcity calculated (incl. binary)")
    ##########
    
    output = 
      output %>% #Incase the gwa exceeds ETc
      merge(WF_data, by = 'WB_NAME', all.x = T) %>%
      as.data.table() %>%
      tidytable::select(year, WB_NAME, ISO_A3, gws_atleast1_bin:total_pixels,
                        gws_monthly_aggregate_1 = gws_pixel_median, bws_monthly_aggregate_1 = bws_pixel_median,
                        gws_monthly_binary_2 = gws_pixel_bin_year_median, bws_monthly_binary_2 = bws_pixel_bin_year_median,
                        gws_pixel_year_3 = gws_pixel_year_median, bws_pixel_year_3 = bws_pixel_year_median,
                        gws_year_country_4, bws_year_country_4,
                        gws_area_scarce_5, bws_area_scarce_5,
                        gws_area_atleast1_6, bws_area_atleast1_6) %>%
      arrange(WB_NAME, year)
    
    ##########
    print("4 - Water Footprint data merged; variables subsetted")
    ##########
  } else {
    
    output = 
      output %>%
      tidytable::group_by(year, WB_NAME, ISO_A3) %>%
      tidytable::summarise(across(airrww_m3_year:actual_water_consumed_m3_year, ~ sum(.x, na.rm = T)),
                           across(gws_pixel:bws_pixel_year, ~median(.x, na.rm = T), .names = "{.col}_median"),
                           across(gws_atleast1_bin:bws_pixel_year_bin, ~ sum(.x, na.rm = T)),
                           total_pixels = n()) 
    
    ##########
    print("2 - Pixel values aggregated to country-level")
    ##########
    
    output = 
      output %>%
      tidytable::mutate(gws_year_country_4 = (ETc_m3_year - gwa_m3_year)/ETc_m3_year, #Potential Irrigation Water Consumption to Potential Water Demands
                        bws_year_country_4 = actual_water_consumed_m3_year/bwa_m3_year, #Irrigation Water Consumption to Blue Water Availability
                        gws_area_scarce_5 = gws_pixel_year_bin/total_pixels,
                        bws_area_scarce_5 = bws_pixel_year_bin/total_pixels,
                        gws_area_atleast1_6 = gws_atleast1_bin/total_pixels,
                        bws_area_atleast1_6 = bws_atleast1_bin/total_pixels) %>%
      tidytable::mutate(gws_year_country_4 = ifelse(gws_year_country_4<0, 0, gws_year_country_4)) 
    
    ##########
    print("3 - Country-level scarcity calculated (incl. binary)")
    ##########
    
    output = 
      output %>% #Incase the gwa exceeds ETc
      merge(WF_data, by = 'WB_NAME', all.x = T) %>%
      as.data.table() %>%
      tidytable::select(year, WB_NAME, ISO_A3, gws_atleast1_bin:total_pixels,
                        gws_monthly_aggregate_1 = gws_pixel_median, bws_monthly_aggregate_1 = bws_pixel_median,
                        gws_monthly_binary_2 = gws_pixel_bin_year_median, bws_monthly_binary_2 = bws_pixel_bin_year_median,
                        gws_pixel_year_3 = gws_pixel_year_median, bws_pixel_year_3 = bws_pixel_year_median,
                        gws_year_country_4, bws_year_country_4,
                        gws_area_scarce_5, bws_area_scarce_5,
                        gws_area_atleast1_6, bws_area_atleast1_6) %>%
      arrange(WB_NAME, year)
    
    ##########
    print("4 - Water Footprint data merged; variables subsetted")
    ##########
  }
  
  return(output)
}

##########################################################################################