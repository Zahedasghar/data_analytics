

library(magick)
library(sf)
library(stars)
library(tidyverse)
#i <- 
#  image_read('https://images.photowall.com/products/59143/audrey-hepburn-3.jpg?h=699&q=85') |> 
#  image_resize("300x")

i <- 
 image_read('images/zahid5.jpg') |> 
  image_resize("300x")


i
# Define some variables for this step
size <- 300
n_shades <- 16

i_processed <-
  i |>
  image_resize(paste0(size,"x",size,"^")) |>
  image_crop(geometry = paste0(size,"x",size), gravity = "center") |>
  image_convert(type = "grayscale") |>
  image_quantize(max = n_shades, dither=FALSE) |>
  image_flip()

i_processed

i_sf <- 
  i_processed |> 
  image_raster() |> 
  mutate(
    col2rgb(col) |> t() |> as_tibble(),
    col = scales::rescale(red, to = c(1,0))) |> 
  select(-green, -blue, -red) |> 
  stars::st_as_stars() |> 
  st_as_sf(as_points = FALSE, merge = TRUE) |> 
  st_make_valid() |> 
  st_set_agr("constant") |> 
  st_normalize()

ggplot() + 
  geom_sf(data = i_sf, col = NA, aes(fill = col)) + 
  scale_fill_viridis_c(direction = -1)

#' Create spiral coordinates
#'
#' @param xo Spiral origin x coordinate
#' @param yo Spiral origin y coordinate
#' @param n_points Number of points on whole spiral (equally spaced in angle)
#' @param n_turns Number of turns in spiral
#' @param r0 Spiral inner radius
#' @param r1 Spiral outer radius
#' @param offset_angle Offset angle for start of spiral (in degrees)
spiral_coords <- function(xo, yo, n_points, n_turns, r0, r1, offset_angle){
  
  b <- (r1 - r0)/(2*pi*n_turns)
  l <- seq(0, 2*pi*n_turns, l=n_points)
  
  tibble(
    x = (r0 + (b*l))*cos(l + offset_angle*(pi/180)) + xo,
    y = (r0 + (b*l))*sin(l + offset_angle*(pi/180)) + yo)
}

n_turns <- 50
spiral_r1 <- 0.5

spiral <-
  spiral_coords(
    xo = 0.5,
    yo = 0.5,
    n_points = 5000,
    n_turns = n_turns,
    r0 = 0,
    r1 = spiral_r1,
    offset_angle = 0) |>
  as.matrix() |>
  sf::st_linestring()

ggplot()+
  geom_sf(data = i_sf, aes(fill = col), col = NA) +
  geom_sf(data = spiral, col = "black") +
  scale_fill_viridis_c("", alpha = 0.75, direction = -1)+
  theme(legend.position = "")

thin <- 0.00025
thick <- ((spiral_r1/n_turns)/2)*0.95

intersections <-
  st_intersection(i_sf, spiral) |>
  mutate(n = scales::rescale(col, to=c(thin, thick))) |>
  mutate(geometry = st_buffer(geometry, n, endCapStyle = "ROUND"))

ggplot() + geom_sf(data = intersections, fill = "black", col = NA)

ggplot() + 
  geom_sf(
    data = intersections |> arrange(desc(n)), 
    aes (fill = n),
    col = NA) +
  scale_fill_gradient(low = "white", high = "black")+
  theme_void()+
  theme(legend.position = "")

ggplot() + 
  geom_sf(
    data = intersections |> arrange(desc(n)), 
    aes (fill = n),
    col = NA) +
  scale_fill_viridis_c(direction = -1, option = "plasma")+
  theme_void()+
  theme(legend.position = "")

ggplot() + 
  geom_sf(
    data = intersections |> arrange(desc(n)), 
    aes (fill = n),
    col = NA) +
  scale_fill_gradient(low = "white", high = "black")+
  theme_void()+
  theme(legend.position = "")

ggplot() + 
  geom_sf(
    data = intersections |> arrange(desc(n)), 
    aes (fill = n),
    col = NA) +
  scale_fill_viridis_c(direction = -1, option = "plasma")+
  theme_void()+
  theme(legend.position = "")

ggplot() + geom_sf(data = intersections |> st_union(), col = 1, fill = "red")
last_plot() + coord_sf(xlim = c(0.4, 0.6), ylim = c(0.4, 0.6))

spiral_image <-
  function(
    img,
    invert = FALSE,
    size = 300,
    n_shades = 16,
    spiral_points = 5000,
    spiral_turns = 50,
    spiral_r0 = 0,
    spiral_r1_f = 1,
    thin = 0.00025,
    thick_f = 0.95,
    spiral_offset_angle = 0,
    col_line = "black",
    col_bg = "white"){
    
    # Read image --------------------------------------------------------------
    if(class(img) == "magick-image"){i <- img} else {i <- magick::image_read(img)}
    
    # Process image to sf polygon
    i_sf <-
      i |>
      magick::image_resize(paste0(size,"x",size,"^")) |>
      magick::image_crop(geometry = paste0(size,"x",size), gravity = "center") |>
      magick::image_convert(type = "grayscale") |>
      magick::image_quantize(max = n_shades, dither=FALSE) |>
      magick::image_flip() |> 
      magick::image_raster() |>
      dplyr::mutate(
        col2rgb(col) |> t() |> as_tibble(),
        col = scales::rescale(red, to = if(invert){c(0,1)}else{c(1, 0)})) |> 
      dplyr::select(-green, -blue, -red) |> 
      stars::st_as_stars() |>
      sf::st_as_sf(as_points = FALSE, merge = TRUE) |>
      sf::st_make_valid() |>
      sf::st_set_agr("constant") |> 
      sf::st_normalize()
    
    # Generate spiral ----------------------------------------------------------
    spiral <-
      spiral_coords(
        xo = 0.5,
        yo = 0.5,
        n_points = spiral_points,
        n_turns = spiral_turns,
        r0 = spiral_r0,
        r1 = 0.5 * spiral_r1_f,
        offset_angle = spiral_offset_angle) |>
      as.matrix() |>
      sf::st_linestring()
    
    # Compute the thick value
    thick <- ((((0.5*spiral_r1_f) - spiral_r0)/spiral_turns)/2)*thick_f
    
    intersections <-
      sf::st_intersection(i_sf, spiral) |>
      dplyr::mutate(n = scales::rescale(col, to=c(thin, thick))) |>
      dplyr::mutate(geometry = sf::st_buffer(geometry, n, endCapStyle = "ROUND")) |>
      sf::st_union()
    
    ggplot2::ggplot() + 
      ggplot2::geom_sf(data = intersections, fill = col_line, col = NA)+
      ggplot2::theme_void()+
      ggplot2::theme(panel.background = ggplot2::element_rect(fill = col_bg, colour = NA))+
      ggplot2::scale_x_continuous(limits = c(0,1))+
      ggplot2::scale_y_continuous(limits = c(0,1))
  }

spiral_image(i)
spiral_image(i, invert = TRUE)
spiral_image(i, col_bg = "grey20", col_line = "hotpink", invert = TRUE)
spiral_image(i, spiral_r0 = 0.1)


spiral_image(i, spiral_points = 150, spiral_turns = 50, thick_f = 0.5)

spiral_image(i, spiral_points = 200, spiral_turns = 50, thick_f = 0.5)
spiral_image(i, spiral_points = 200, spiral_turns = 50, thick_f = 0.5, spiral_offset_angle = 45)

spiral_image(
  i, 
  spiral_points = 250, 
  spiral_turns = 50, 
  invert = TRUE, 
  col_bg = "grey10", 
  col_line = "cyan", 
  thick_f = 0.5)
