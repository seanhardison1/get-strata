# Use choose_strata to build rasters for different stock areas. This function allows for downsampling the raster, which is 
# useful if you'd like to map stock areas to data sets with different spatial resolutions. The file "seasonal_stock_strata.csv"
# is used to link svspp species code, common name, and depth strata. 

# svspp is species code
# season is fall, spring, winter
# mask_type is a categorical variable for filtering stock regions ("unit", "gbk",
#  "gom", "south", "north", "snemab", "gbkgom", "ccgom", or "sne")
# 

choose_strata <- function(svspp, season, mask_type, downsample = F){
  
  sea_stock_strata <- read.csv('data/seasonal_stock_strata.csv', stringsAsFactors = F)
  
  #Filter strata
  temp_strata <- sea_stock_strata %>% filter(season_ == season, 
                                             SVSPP == svspp,
                                             stock_area == mask_type)
  #Break if no matching variables
  if (nrow(temp_strata) == 0){
    stop('Seasonal stock strata, species code, and stock area ("mask type") must correspond.\n See seasonal_stock_strata.csv for complete listing.')
  }
  
  #read in shape file
  strata <- rgdal::readOGR(dsn = 'data/strata_shapefiles', verbose = F)
  
  if (mask_type == 'nes'){
    nes <- unique(sea_stock_strata$strata)
    stock_strata <- strata[strata@data$STRATA %in% nes,]
  } else {
    stock_strata <- strata[strata@data$STRATA %in% temp_strata$strata,]
  }
  
  if (downsample){
    #create empty raster
    r1 <- raster::raster()
    e <- raster::extent(-75.950, -65.450, 35.650, 44.650)
    raster::extent(r1) <- e
    
    #fill with strata
    r1 <- raster::rasterize(stock_strata, r1, field = 1, fun = mean)
    raster::crs(r1) <- NA
    
    #create raster to resample with
    r2 <- raster::raster(nrow = 90, ncol = 105)
    raster::extent(r2) <- e
    raster::crs(r2) <- NA
    
    #resample high res raster to match data
    r.new <- raster::resample(r1, r2, method="bilinear")
    r.new[is.finite(r.new)] <- 1 
  } else {
    r.new <- strata
  }
  
  return(r.new)
  
}

