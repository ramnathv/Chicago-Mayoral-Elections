choropleth <- function(.poly, id1, .df = .poly@data, id2 = id1, 
	 field, .title    = "", .legtitle = "Values", .ggplot = TRUE, 
   colpal = 'PuRd', ...){
                            
     # load required libraries
     library(maptools); library(spatial); library(RColorBrewer);
     library(classInt); library(ggplot2); gpclibPermit();
     
     map.data = merge_poly_df(.poly, id1, .df, id2, .ggplot);
     breaks   = find_fill_breaks(map.data, field, colpal, ...);
     
     # plot choropleth 
     
     if (.ggplot) {
       map.data.m =  melt(map.data, measure.vars = field);
       map.data.m$value_d =  with(map.data.m, cut(value, breaks,incl = T));
       p1 = ggplot(arrange(map.data.m, order), aes(long, lat, group = group)) + 
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