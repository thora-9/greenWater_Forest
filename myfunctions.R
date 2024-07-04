#Custom function
per_na = function(x){
  out = apply(x, MARGIN = 2, FUN = function(x){round(sum(is.na(x))*100/length(x), 2)})
  return(out)
}


process_nc <- function (nc_filepath) {
  nc = 
    tidync(nc_filepath) %>%
    activate("D1,D0,D2") %>%
    hyper_tibble() %>% as.data.table()
}

process_nc_as_raster <- function (nc_filepath) {
  nc = 
    rast(nc_filepath)
}
