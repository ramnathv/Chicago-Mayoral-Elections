choropleth_gg <- function(.poly, id1, .df = .poly@data, id2 = id1, field, 
                          .title = "", .legtitle = "Values", colpal = 'PuRd', ...){
                            
     # load required libraries
     library(maptools); library(spatial); library(RColorBrewer);
     library(classInt); library(ggplot2); gpclibPermit();
                            
     # create id to match shapefile with data
     id1 = llply(id1, as.name); 
     id2 = llply(id2, as.name);
    .poly@data$id = with(.poly@data, do.call('paste', c(id1, sep = "-")));
    .df$id        = with(.df,        do.call('paste', c(id2, sep = "-")));
     
      # fortify shapefile
      map = fortify(.poly, region = 'id'); 
      
      # merge shapefile (fortified) with data 
      map.data     = merge(map, .df, by = 'id');
      
      # choose breaks for values
      map.data.m  = melt(map.data, measure.vars = field);
      breaks      = do.call("classIntervals", list(var = map.data.m$value, ...))$brks;
      
      
      # plot choropleth map
      map.data.m  = within(map.data.m, {value_d = cut(value, breaks, incl = T)});
      p1 = ggplot(arrange(map.data.m, order), aes(long, lat, group = group)) + 
           geom_polygon(aes(fill = value_d)) +
           scale_fill_brewer(.legtitle, pal = colpal) + 
           facet_wrap(~ variable) +
           theme_map() +
           opts(title = .title);
           
      return(p1)
}






  

    
