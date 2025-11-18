# point from minimal distance from st_distance() 
st_nearest_points <- function(patches, type){
  
  library(sf)
  
  data_n <- NULL
  lines_n <- NULL
  
  for(i in 1:nrow(patches)){
    
    data_n_i <- NULL
    lines_n_i <- NULL
    
    for(j in 1:nrow(patches)){
      
      if(j < i){      
        next 
      }
      
      cat(i, "and", j, "\n")
      
      if(type == "edge"){
        line_n_ij <- sf::st_nearest_points(patches[i, ], patches[j, ])
        coords_n_ij <- sf::st_coordinates(line_n_ij)[, -3]
        
      } else if(type == "centroid"){
        cent_i <- suppressWarnings(sf::st_as_sfc(sf::st_centroid(patches[i, ])))
        cent_j <- suppressWarnings(sf::st_as_sfc(sf::st_centroid(patches[j, ])))
        line_n_ij <- suppressWarnings(sf::st_cast(sf::st_union(cent_i, cent_j), "LINESTRING"))
        coords_n_ij <- sf::st_coordinates(line_n_ij)[, -3]
      }
      
      length_n_ij <- sf::st_length(line_n_ij)
      
      line_n_ij <- line_n_ij %>% 
        sf::st_as_sf() %>% 
        dplyr::mutate(length = as.numeric(length_n_ij)) %>% 
        dplyr::mutate(i = i, j = j, .before = 1)
      
      data_n_ij <- coords_n_ij %>%
        tibble::as_tibble() %>% 
        dplyr::mutate(length = length_n_ij) %>% 
        dplyr::mutate(i = i, j = j, .before = 1) %>% 
        dplyr::filter(!i == j)
      
      data_n_i <- rbind(data_n_i, data_n_ij) 
      lines_n_i <- rbind(lines_n_i, line_n_ij) 
      
    }
    
    data_n <- rbind(data_n, data_n_i)
    lines_n <- dplyr::filter(rbind(lines_n, lines_n_i), i != j)
    
  }
  
  coords_n <- sf::st_as_sf(data_n, coords = c("X", "Y"), crs = sf::st_crs(patches))
  centroid_n <- suppressWarnings(sf::st_centroid(patches))
  
  return(list(data = data_n, 
              coordinates = coords_n, 
              lines = lines_n, 
              centroids = centroid_n))
  
}
