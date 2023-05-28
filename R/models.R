real_estate_offer <- function(offer_area, prediction_method){
  
  stopifnot("Offer area must be a spatial object" = is(offer_area, "sf"))
  stopifnot("Prediction method must be specified as linear, orthogonal or spline" = is.character(prediction_method))
  
  require("dplyr")
  ppp_coords <- st_coordinates(offer_area)
  ppp_data <- offer_area %>% mutate(lon=ppp_coords[,"X"], 
                                    lat=ppp_coords[,"Y"], 
                                    tipo=as.factor(tipo)) %>% 
    dplyr::select(lat,lon,tipo)
  
  # logistic adjustment
  require(splines)
  
  if (prediction_method=='linear'){
    #linear
    logistic_adj <- glm(tipo ~ lon+lat, data=ppp_data, family="binomial")
  } else if (prediction_method=='orthogonal') {
    # Orthogonal Polynomials
    logistic_adj <- glm(tipo ~ poly(lon,3)*poly(lat,3), data=ppp_data, family="binomial")
  }
     else if (prediction_method=='splines'){
    # Polynomial spline
    logistic_adj <- glm(tipo ~ bs(lon,4)*bs(lat,4), data=ppp_data, family="binomial")
  }
  
  # grid predictions
  cant <- 100
  grid_canvas <- expand.grid(lon=seq(min(ppp_data$lon), max(ppp_data$lon),length.out=cant),
                             lat=seq(min(ppp_data$lat), max(ppp_data$lat),length.out=cant))
  
  pred <- predict(logistic_adj, newdata=grid_canvas, type="response")
  summary(pred)
  
  # Empty raster
  require(raster)
  rowxcol <- cant
  raster_canvas <- raster(nrows=rowxcol, ncols=rowxcol,
                          xmn=min(ppp_data$lon), xmx=max(ppp_data$lon),
                          ymn=min(ppp_data$lat), ymx=max(ppp_data$lat))
  
  # Rasterize prediction
  raster_pred <- raster::rasterize(grid_canvas, raster_canvas, field=pred, fun=median)
  sf_collection <- st_union(ppp_data)
  study_area <- st_convex_hull(sf_collection)
  raster_mask <- raster::mask(raster_pred, as_Spatial(study_area))
  raster_vals <- raster::values(raster_mask)
  
  # Plot prediction results
  require(leaflet)
  
  colorsvec <- c('red', 'blue', 'green')
  cat_breaks <-seq(min(raster_vals, na.rm=T), max(raster_vals, na.rm=T), length.out=20)
  col_bins <- colorBin(palette = colorsvec, bins=cat_breaks, domain=cat_breaks, na.color="#00000000")
  
  leaflet(width=900, height=400) %>% 
    addTiles() %>% 
    addProviderTiles(providers$CartoDB.Positron) %>% 
    addRasterImage(raster_mask, opacity=0.75, colors=colorsvec) %>% 
    addLegend(pal=col_bins, values=cat_breaks)
}