# point from minimal distance from st_distance() 
st_nearest_dist <- function(patches){
  
  library(sf)
  
  data_n <- NULL
  lines_n <- NULL
  points_n <- NULL
  coords_n <- NULL
  
  nearest_dist <- sf::st_nearest_points(patches, patches)
  
  lines_n <- nearest_dist[st_geometry_type(nearest_dist) == "LINESTRING", ] %>% 
    sf::st_as_sf() %>% 
    tibble::rowid_to_column(var = "lid") %>% 
    dplyr::mutate(l_length = round(as.numeric(sf::st_length(.)), 0), .after = 1)
  
  points_b_n <- lines_n %>% 
    sf::st_cast("POINT", warn = FALSE) %>% 
    sf::st_buffer(.01) %>% 
    sf::st_as_sf() %>% 
    sf::st_join(p) %>% 
    dplyr::arrange(lid) %>% 
    dplyr::relocate(pid, .before = 1)
  
  points_n <- lines_n %>% 
    sf::st_cast("POINT", warn = FALSE) %>% 
    dplyr::mutate(pid = points_b_n$pid, .before = 1)
  
  data_n <- points_b_n %>% 
    sf::st_drop_geometry() %>% 
    tibble::as_tibble() %>% 
    dplyr::group_by(lid, l_length) %>%
    dplyr::mutate(pid_index = paste0("pid", row_number())) %>%
    tidyr::spread(pid_index, pid) %>% 
    dplyr::ungroup() %>% 
    dplyr::relocate(pid1, pid2, lid, l_length)
  
  lines_n <- lines_n %>% 
    dplyr::left_join(data_n, by = join_by(lid, l_length)) %>% 
    dplyr::relocate(pid1, pid2, .before = 1)
  
  coords_n <- points_n %>%
    sf::st_drop_geometry() %>% 
    tibble::as_tibble() %>% 
    dplyr::mutate(X = sf::st_coordinates(points_n)[, 1],
                  Y = sf::st_coordinates(points_n)[, 2])
  
  centroid_n <- suppressWarnings(sf::st_centroid(patches))
  
  return(list(data = data_n, 
              points = points_n,
              coordinates = coords_n, 
              lines = lines_n, 
              centroids = centroid_n))
  
}
