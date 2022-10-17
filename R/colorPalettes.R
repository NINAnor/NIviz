
# CUSTOM COLOR PALETTES #
#-----------------------#

# Set colours
NIviz_colours = list(
  IndMap_cols = c("#A44B4B", "#EA4B4B", "#FD7F4B", "#FDC44B", "#F0FD58",
                  "#A9FD9F", "#4BCFFD", "#4B8AFD", "#4B4BF6", "#4B4BAF"),
  EcoSys_cols = c("#1F8C81", # 1: Freshwater (ferskvann)
                  "#CFD1D3", # 2: Mountain (fjell)
                  "#2759A1", # 3: Ocean bottom (havbunn)
                  "#2759A1", # 4: Ocean, pelagic (hav-pelagisk)
                  "#89B3D9", # 5: Coast bottom (kystbunn)
                  "#89B3D9", # 6: Coast, pelagic (kyst-pelagisk)
                  "#8A4584", # 7: Wetland (vaatmark)
                  "#1DAC60", # 8: Woodland (skog)
                  "#A7DB78", # 9: Open lowland (aapent lavland)
                  "#2759A1", # 10: Ocean (hav)
                  "#89B3D9", # 11: Coast (kystvann)
                  "#FFFFFF" # 12: Status ecosystems
  )
)

# Make palettes
NIviz_palettes = function(name, n, all_palettes = NIviz_colours, type = c("discrete", "continuous")) {
  palette = all_palettes[[name]]
  if (missing(n)) {
    n = length(palette)
  }
  type = match.arg(type)
  out = switch(type,
               continuous = grDevices::colorRampPalette(palette)(n),
               discrete = palette[1:n]
  )
  structure(out, name = name, class = "palette")
}

# Make ggplot functions
scale_colour_NIviz_d = function(name) {
  ggplot2::scale_colour_manual(values = NIviz_palettes(name,
                                                       type = "discrete"))
}
scale_fill_NIviz_d = function(name) {
  ggplot2::scale_fill_manual(values = NIviz_palettes(name,
                                                     type = "discrete"))
}
scale_colour_NIviz_c = function(name) {
  ggplot2::scale_colour_gradientn(colours = NIviz_palettes(name = name,
                                                           type = "continuous"))
}
scale_fill_NIviz_c = function(name) {
  ggplot2::scale_fill_gradientn(colours = NIviz_palettes(name = name,
                                                         type = "continuous"))
}
scale_color_NIviz_d = scale_colour_NIviz_d
scale_color_NIviz_c = scale_colour_NIviz_c
