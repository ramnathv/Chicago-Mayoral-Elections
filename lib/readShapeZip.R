# FUNCTION TO READ SHAPE FILE FROM A ZIPPED FOLDER

readShapeZip <- function(url, .tempfile = tempfile(), 
                  .tempdir = tempdir()){

   require(maptools); gpclibPermit();
   download.file(url = url, .tempfile, quiet = T);
  .files    = unzip(.tempfile, exdir = .tempdir);
  .shpfile  = head(.files)[grep('shp', head(.files))];
  .poly     = readShapeSpatial(.shpfile);
   return(.poly)
  
}

# Example URLs to Test
# lon.url <- 'http://spatialanalysis.co.uk/files/2010/09/London_Sport.zip';
# chi.url <- 'http://goo.gl/EDvIt'