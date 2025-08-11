#' wrap_flow_routing_d8_edgelist
#'
#' Compute downstream pixel indices from flow directions.
#' 
#' @param DEM (terra::SpatRaster) Digital elevation model
#'
#' @import terra
#'
#' @return A list containing vectors of source and target pixels for each edge.
#' 
#' @export

flow_routing_d8_edgelist <- function(DEM){
  
  # Compute inputs from raw DEM
  SD <- flow_routing_d8_carve(DEM)
  nodes <- get_grid_data(SD$source)
  directions <- get_grid_data(SD$direction)
  
  # Compute flow routing using libtopotoolbox
  outputs <- single(length(nodes$z))
  results <- .C("wrap_flow_routing_d8_edgelist",
                sourceR = as.integer(outputs), # ptrdiff_t
                targetR = as.integer(outputs), # ptrdiff_t
                nodeR = as.integer(nodes$z), # ptrdiff_t
                directionR = as.integer(directions$z), # uint8_t
                dimsR = as.integer(nodes$dims), # ptrdiff_t
                NAOK = TRUE)
  
  # Determine edge count through source pixels, including 0-th element
  edge_count <- sum(results$sourceR > 0) + 1
  
  # Write outputs
  return(list("source" = results$sourceR[1:edge_count],
              "target" = results$targetR[1:edge_count]))
}