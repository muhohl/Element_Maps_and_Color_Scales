library(tidyverse)

# Rainbow colormap
rainbow_palette <- rev(c("#CCCCCC", "#8B0000", "#FF0000", "#FFA500",
                     "#FFFF00", "#00FF00", "#90EE90",
                     "#ADD8E6", "#00008B",  "#A020F0", "#000000"))

# Heat colormap
heat_palette <- rev(c("#FFFF00", "#FFA500", "#FF0000", "#8B0000", "#000000"))


# Plots

i <- 1
rainbow_element_maps <- list()
for (element in names(df_element_map[-c(1:2)])) {
    p.rainbow <- ggplot(data = df_element_map,
                aes(x, y,
                    fill = !!sym(element))) +
    geom_raster(interpolate = TRUE) +
    coord_fixed(ratio = 1) +
    scale_y_discrete(expand = c(0,0)) +
    scale_x_discrete(expand = c(0,0)) +
    labs(fill = "",
         y = "",
         x = "") +
    ggtitle(paste(element, "ppm")) +
    scale_fill_gradientn(colors = rainbow_palette,
                         trans = "log",
                         breaks = c(1, 10, 100, 1000, 10000),
                         labels = scales::label_scientific()) +
    theme_bw() +
    theme(panel.border = element_blank(),
          text = element_text(family = "serif",
                              size = 16)) +
    guides(fill = guide_colorbar(barwidth = unit(1, "lines"),
                                 barheight = unit(8, "lines"),
                                 ticks.colour = "black", 
                                 frame.colour = "black"))
    rainbow_element_maps[[i]] <- p.rainbow
    i <- i+1
}
p.g.rainbow <- cowplot::plot_grid(plotlist = rainbow_element_maps)

p.g.rainbow



i <- 1
heat_element_maps <- list()
for (element in names(df_element_map[-c(1:2)])) {
    p.rainbow <- ggplot(data = df_element_map,
                aes(x, y,
                    fill = !!sym(element))) +
    geom_raster(interpolate = TRUE) +
    coord_fixed(ratio = 1) +
    scale_y_discrete(expand = c(0,0)) +
    scale_x_discrete(expand = c(0,0)) +
    labs(fill = "",
         y = "",
         x = "") +
    ggtitle(paste(element, "ppm")) +
    scale_fill_gradientn(colors=heat_palette,
                         trans = "log",
                         breaks = c(1, 10, 100, 1000, 10000),
                         labels = scales::label_scientific()) +
    theme_bw() +
    theme(panel.border = element_blank(),
          text = element_text(family = "serif",
                              size = 16)) +
    guides(fill = guide_colorbar(barwidth = unit(1, "lines"),
                                 barheight = unit(8, "lines"),
                                 ticks.colour = "black", 
                                 frame.colour = "black"))
    heat_element_maps[[i]] <- p.rainbow
    i <- i+1
}
p.g.heat <- cowplot::plot_grid(plotlist = heat_element_maps)

p.g.heat


viridis_d_element_maps <- 
    geochem::laser_map(data = df_element_map, 
                       selected_elements = names(df_element_map[-c(1:2)]),
                       fontsize = 16)
cowplot::plot_grid(plotlist = viridis_d_element_maps)

viridis_b_element_maps <- 
    geochem::laser_map(data = df_element_map, 
                       selected_elements = names(df_element_map[-c(1:2)]),
                       option = "B", fontsize = 16)
cowplot::plot_grid(plotlist = viridis_b_element_maps)

