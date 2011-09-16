merge_poly_df <- function(.poly, id1, .df, id2, type){
  
   require(ggplot2);
  
	 # create id for shapefile 
	 id1 = llply(id1, as.name); 
	.poly@data$id = with(.poly@data, do.call('paste', c(id1, sep = "-")));
  
   # if data file exists, create id 
   if (all(!is.na(.df))){
     id2   = llply(id2, as.name);
	  .df$id = with(.df, do.call('paste', c(id2, sep = "-"))); 
   } 
	 
   if (type == 'ggplot'){
     
	     # fortify shape file and merge with data
     
       if(all(is.na(.df))) {.df = .poly@data};
	     map      = fortify(.poly, region = 'id');
       map.data = merge(map, .df, by = 'id');
       map.data = map.data[order(map.data$order),];
     
   } else {
       # merge data with shape file
       
       if(all(!is.na(.df))) {  
           .data = .df[match(.poly@data$id, .df$id),];
           .poly@data = data.frame(.poly@data, .data);
           .poly@data$id.1 = NULL;
       }
       map.data  = .poly@data;
   }
	 return(map.data);
}