# FUNCTION TO READ SHAPE FILE FROM A ZIPPED FOLDER

readShapeZip <- function(
  url,                     # url containing zip folder
 .tempfile = tempfile(),   # file to download url 
 .tempdir  = tempdir()     # directory to download url
  ){

   require(maptools); gpclibPermit();
   download.file(url = url, .tempfile, quiet = T);     # download file
  .files    = unzip(.tempfile, exdir = .tempdir);      # unzip to .tempdir
  .shpfile  = head(.files)[grep('shp', head(.files))]; # find .shp file
  .poly     = readShapeSpatial(.shpfile);              # read .shp file
   return(.poly)        
  
}

# Example URLs to Test
# lon.url <- 'http://spatialanalysis.co.uk/files/2010/09/London_Sport.zip';
# lon.shp = readShapeZip(lon.url)
# chi.url <- 'http://goo.gl/EDvIt'
# chi.shp = readShapeZip(chi.url)