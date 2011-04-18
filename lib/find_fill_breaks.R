find_fill_breaks <- function(map.data, field, colpal, ...){
   
     require(classInt);
     
     # melt map data based on field variables
      map.data.m  = melt(map.data, measure.vars = field);
     
     # hack needed to make classIntervals work with ... arguments
     x = list(var = map.data.m$value, ...);
     
     # find breaks using method specified (default = quantile)
 	  breaks = do.call("classIntervals", x)$brks;
     return(breaks);
}

plot_choro <- function(
	
### Generic function to create a choropleth map using a shapefile and a data ### file. Minimum input required is a shapefile, an id variable representing ### the region, a field of values to plot and arguments to the 
	.poly, 
	 id1, 
	.df       = .poly@data, 
	 id2      = id1, 
	 field, 
	.title    = "", 
	.legtitle = "Values", 
	 colpal   = 'PuRd',
	 fortify  = TRUE, 
     ...){
                            
     # load required libraries
     library(maptools); library(spatial); library(RColorBrewer);
     library(classInt); library(ggplot2); gpclibPermit();
     
     map.data = merge_poly_df(.poly, id1, .df, id2, fortify);
     breaks   = find_fill_breaks(map.data, field, colpal, ...);
     
     # plot choropleth map
     
     if (fortify == T) {
       map.data.m =  melt(map.data, measure.vars = field);
       map.data.m =  within(map.data.m, {
	        value_d = cut(value, breaks,incl = T)
	     });
	     map.data.m = arrange(map.data.m, order) 
       p1 = ggplot(map.data.m, aes(long, lat, group = group)) + 
         geom_polygon(aes(fill = value_d)) +
         scale_fill_brewer(.legtitle, pal = colpal) + 
         facet_wrap(~ variable) +
         theme_map() +
         opts(title = .title);
          
     } else {
       
       colors    = brewer.pal(length(breaks) - 1, colpal);
      .poly@data = map.data;
       p1 = spplot(.poly, field, at = breaks, col.regions = colors, 
	       col = 'transparent', main = .title, aspect = 'fill');
     }
           
     return(p1)
}