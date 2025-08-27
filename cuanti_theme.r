theme_cuanti <- function(base_size=16, base_family="Helvetica") {
  require(grid)
  require(ggthemes)
  (theme_foundation(base_size=base_size, base_family=base_family)
    + theme(plot.title = element_text(face = "bold",
                                      size = rel(1.2), hjust = 0.5),
            text = element_text(),
            plot.tag = element_text(face="bold", size=rel(1.5)),
            panel.background = element_rect(colour = "#b4a5fa"),
            plot.background = element_rect(colour = NA),
            panel.border = element_rect(colour = NA),
            axis.title = element_text(face = "bold",size = rel(1)),
            axis.title.y = element_text(angle=90,vjust =2),
            axis.title.x = element_text(vjust = -0.2),
            axis.text = element_text(), 
            axis.line = element_line(colour="black"),
            axis.ticks = element_line(),
            panel.grid.major = element_line(colour="#f0f0f0"),
            panel.grid.minor = element_blank(),
            legend.key = element_rect(colour = NA),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size= unit(.6, "cm"),
            legend.spacing = unit(0, "cm"),
            legend.title = element_text(face="italic"),
            plot.margin=unit(c(10,5,5,5),"mm"),
            strip.background=element_rect(colour="#f0f0f0",fill="#f0f0f0"),
            strip.text = element_text(face="bold")
    ))
}

# --- Paleta: Tableau 20 (hasta 20) -> Glasbey (21+) ---
cuanti_pal <- function(n, anchor = "#6C3BF6") {
  pal <-
    if (n <= 20) {
      ggthemes::tableau_color_pal("Tableau 20")(n)
    } else {
      pals::glasbey(n)  # separa bien 20–80+
    }
  pal[1] <- anchor     # anclá el 1º color a tu violeta institucional
  pal
}

# Escalas discretas (fill y color) usando la paleta de arriba
scale_fill_categorical <- function(..., na.value = "grey85", guide = "legend") {
  discrete_scale("fill", "cuanti", palette = cuanti_pal,
                          na.value = na.value, guide = guide, ...)
}
scale_color_categorical <- function(..., na.value = "grey60", guide = "legend") {
  discrete_scale("colour", "cuanti", palette = cuanti_pal,
                          na.value = na.value, guide = guide, ...)
}



# scale_fill_categorical <- function(...){
#   require(scales)
#   discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
# }
# 
# scale_color_categorical <- function(...){
#   require(scales)
#   discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
# }
# 
# scale_fill_categorical <- function(...){
#   require(scales)
#   discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
# }


require(gt)

gt_theme_cuanti <- function(gtobj, realce = "#6C3BF6",fondo="#e7e6ff",violeta="#b4a5fa") {
  gtobj |>
    #tab_style(
     #  style = list(cell_borders(sides = "bottom", weight = px(3), color = violeta)),
     #  locations = list(cells_column_labels(everything()),
     #                   cells_stubhead()) 
     # ) |>
     # tab_style(
     #   style = list( cell_borders(sides = c("top","bottom"), weight = px(1), color = violeta)),
     #    locations = list(cells_body(everything()),
     #         cells_stub(),
     #         cells_grand_summary(everything()),
     #         cells_stub_grand_summary()
     #       ) 
     #    
     #   
     #  ) |>
    tab_style(
      style = cell_text(weight = "bolder"),locations = cells_title()
    ) |> 
    tab_style(style = cell_text(weight = "bold"),
              locations = list(cells_column_spanners(),
                               cells_stubhead()) ) |> 
    tab_options(
      table.border.bottom.color = violeta,
      table.border.top.color = violeta,
      grand_summary_row.border.color  = violeta,
      grand_summary_row.border.width = px(0),
      #grand_summary_row.background.color = fondo,
      column_labels.background.color = fondo,
      column_labels.border.top.color    = violeta,
      column_labels.border.bottom.color = violeta,
      stub.background.color = fondo,
      stub.border.color = violeta,
      stub.border.width = px(1),
      table_body.hlines.width =  px(0),
      table_body.vlines.width = px(0),
      data_row.padding = px(8),
      table.font.names = c("Roboto","Helvetica","Arial","sans-serif","Poppins")
     ) |> 
    fmt_number(decimals=0,dec_mark = ",",sep_mark = "")
}

