# MAP
  # map projection
    p4s_latlon = CRS("+proj=longlat +datum=WGS84")
    p4s = "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=-72 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs"
    
  # create OR load map and set map projection and prepare ggplot with map
    # create map a new
        #tmin = getData('worldclim', var = 'tmin', res = 10) 
         # z1 = tmin[[ c('tmin6')]]
         # z1 = aggregate(z1, 4)
         # z = calc( z1, function(x) x/10)
         # zp = projectRaster(z, crs = p4s)
         # zp = rasterToPoints(zp) %>% data.frame %>% data.table
         # latlon = spTransform(SpatialPoints( zp[, .(x,y)] , proj4string = CRS(p4s)), CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0")) %>% coordinates
         # zp = cbind(zp, latlon)
         # v

  # load map & prepare map-plot to add/show  the breeding site
    load(file = here::here('Data/map_for_supplement_10res.Rdata'))#10 res
     m  =  ggplot() +
        coord_equal() +
        geom_raster(data = zp, aes(x = x, y = y, fill=temp))   +
        scale_fill_gradientn(colours = c("grey90","grey90"), guide="none")+
        #geom_path(data = fortify(grt) ,aes(x = long, y = lat, group = group), colour = "white", size   = .3)+
        theme_map() + theme(plot.margin = grid::unit(c(0.5,0.5,0.5,0.5), "mm")) 


