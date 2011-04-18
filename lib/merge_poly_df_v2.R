merge_poly_df <- function(.poly, id1, .data = .poly@data, id2 = id1, .ggplot = F){
  
   require(ggplot2); require(maptools); gpclibPermit()
   
   # create id for shapefile and data file 
	 id1 = llply(id1, as.name); id2   = llply(id2, as.name);
  .poly@data$id = with(.poly@data , do.call('paste', c(id1, sep = "-")));
  .data$id      = with(.data      , do.call('paste', c(id2, sep = "-"))); 
    
	 # merge shapefile with data (fortify if required)
   if (.ggplot) {
     .map      = fortify(.poly, region = 'id');
      map.data = merge(.map, .data, sort = F);
      map.data = arrange(map.data, order)
      
   } else {
     .map      = .poly@data;
      map.data = merge(.map, .data, sort = F);
      map.data = map.data[match(.map$id, map.data$id),];
   }
     
	 return(map.data);
}

# Test 1: 
# london <- readShapeSpatial('1_data/london/maps/london_sport.shp');
# df1 = merge_poly_df(london, 'name');
# df2 = merge_poly_df(london, 'name', .ggplot = T);

# Test 2: 
# immig  <- read.csv('http://goo.gl/lhvDW');
# df3    = merge_poly_df(london, 'name', immig, 'Area');
# df4    = merge_poly_df(london, 'name', immig, 'Area', .ggplot = T);


# Test 3:
# chicago  =  readShapeSpatial('1_data/chicago/maps/Precincts.shp');
# election =  read.csv('1_data/chicago/precincts.csv');   
# election = election[,-grep('count', names(election))];         
# names(election) = sub('_pct', '', names(election));
# 
# df5 = merge_poly_df(chicago, list('WARD', 'PRECINCT'), election, list('ward', 'precinct'));
# df6 = merge_poly_df(chicago, list('WARD', 'PRECINCT'), election, list('ward', 'precinct'), f = T);
