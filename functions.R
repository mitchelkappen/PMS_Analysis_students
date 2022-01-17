# violin function
geom_flat_violin <-
  function(mapping = NULL,
           data = NULL,
           stat = "ydensity",
           position = "dodge",
           trim = TRUE,
           scale = "area",
           show.legend = NA,
           inherit.aes = TRUE,
           ...) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomFlatViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(trim = trim, scale = scale, ...)
    )
  }

GeomFlatViolin <- ggproto(
  "GeomFlatViolin",
  Geom,
  setup_data = function(data, params) {
    data$width <- data$width %||%
      params$width %||% (resolution(data$x, FALSE) * 0.9)
    # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
    data %>%
      group_by(group) %>%
      mutate(
        ymin = min(y),
        ymax = max(y),
        xmin = x,
        xmax = x + width / 2
      )
  },
  draw_group = function(data, panel_scales, coord) {
    # Find the points for the line to go all the way around
    data <-
      transform(data,
                xminv = x,
                xmaxv = x + violinwidth * (xmax - x))
    # Make sure it's sorted properly to draw the outline
    newdata <-
      rbind(plyr::arrange(transform(data, x = xminv), y),
            plyr::arrange(transform(data, x = xmaxv), -y))
    # Close the polygon: set first and last point the same
    # Needed for coord_polar and such
    newdata <- rbind(newdata, newdata[1,])
    ggplot2:::ggname("geom_flat_violin",
                     GeomPolygon$draw_panel(newdata, panel_scales, coord))
  },
  draw_key = draw_key_polygon,
  default_aes = aes(
    weight = 1,
    colour = "grey20",
    fill = "white",
    size = 0.5,
    alpha = NA,
    linetype = "solid"
  ),
  required_aes = c("x", "y")
)
#standard error function
standard_error <- function(x)
  sd(x) / sqrt(length(x))

#function for split violin
GeomSplitViolin <- ggproto(
  "GeomSplitViolin",
  GeomViolin,
  draw_group = function(self, data, ..., draw_quantiles = NULL) {
    data <-
      transform(
        data,
        xminv = x - violinwidth * (x - xmin),
        xmaxv = x + violinwidth * (xmax - x)
      )
    grp <- data[1, "group"]
    newdata <-
      plyr::arrange(transform(data, x = if (grp %% 2 == 1)
        xminv
        else
          xmaxv), if (grp %% 2 == 1)
            y
        else
          - y)
    newdata <-
      rbind(newdata[1, ], newdata, newdata[nrow(newdata), ], newdata[1, ])
    newdata[c(1, nrow(newdata) - 1, nrow(newdata)), "x"] <-
      round(newdata[1, "x"])
    if (length(draw_quantiles) > 0 &
        !scales::zero_range(range(data$y))) {
      stopifnot(all(draw_quantiles >= 0), all(draw_quantiles <=
                                                1))
      quantiles <-
        ggplot2:::create_quantile_segment_frame(data, draw_quantiles)
      aesthetics <-
        data[rep(1, nrow(quantiles)), setdiff(names(data), c("x", "y")), drop = FALSE]
      aesthetics$alpha <-
        rep(1, nrow(quantiles))
      both <- cbind(quantiles, aesthetics)
      quantile_grob <-
        GeomPath$draw_panel(both, ...)
      ggplot2:::ggname("geom_split_violin",
                       grid::grobTree(GeomPolygon$draw_panel(newdata, ...), quantile_grob))
    }
    else {
      ggplot2:::ggname("geom_split_violin",
                       GeomPolygon$draw_panel(newdata, ...))
    }
  }
)

geom_split_violin <-
  function(mapping = NULL,
           data = NULL,
           stat = "ydensity",
           position = "identity",
           ...,
           draw_quantiles = NULL,
           trim = TRUE,
           scale = "area",
           na.rm = FALSE,
           show.legend = NA,
           inherit.aes = TRUE) {
    layer(
      data = data,
      mapping = mapping,
      stat = stat,
      geom = GeomSplitViolin,
      position = position,
      show.legend = show.legend,
      inherit.aes = inherit.aes,
      params = list(
        trim = trim,
        scale = scale,
        draw_quantiles = draw_quantiles,
        na.rm = na.rm,
        ...
      )
    )
  }

rescalepostive <- function(x) {
  (x - min(x)) / (max(x) - min(x)) + 0.000000001
}

# Trait viz
traitplot <-function(data, emmean_dataframe, var, title){
  ggplot(data, aes(x = PMS, y = .data[[var]])) +
    geom_flat_violin(aes(fill=PMS),position = position_nudge(x =.2, y = 0), alpha=.5, adjust = 1.5, colour = NA)+
    geom_boxplot(aes(x = PMS, y = .data[[var]], fill = PMS), outlier.shape=NA, alpha= .45, width = .1, colour = "black") +
    geom_point(data= emmean_dataframe, aes(x = PMS, y = emmean, fill=PMS), size=4)+
    scale_colour_manual(values = c("blue", "red", "purple"))+
    scale_fill_manual(values = c("blue", 'red', 'purple'),
                      name='',labels=c(paste0('noPMS \n n=', as.character(sum(data$PMS == "noPMS"))), paste0('PMS \n n=', as.character(sum(data$PMS == "PMS"))), paste0('PMDD \n n=', as.character(sum(data$PMS == "PMDD")))))+
    guides(fill = guide_legend(reverse=TRUE))+
    labs(y=title)+
    theme(
      legend.key.size=unit(1.3, 'cm'),
      legend.text=element_text(size=13),
      plot.title = element_text(size=rel(2)),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(colour = "black"),
      panel.grid.major.y = element_line( size=.1, color="#dedede" ),
      axis.text.x=element_text(size=rel(1.5)),
      axis.title.y=element_text(size=rel(1.4)),
      axis.title.x = element_blank())
}

# State Viz
stateplot <-function(data, emmean_dataframe, var, title){
  ggplot()+ 
    geom_flat_violin(data= data, aes(x= Moment, y= .data[[var]], fill=PMS),position = position_nudge(x =.3, y = 0), adjust = 1.5, alpha = .5, colour = NA)+ # flat violin distribution, .3 points to the right. alpha=.5 so see-through
    geom_boxplot(data= data, aes(x=Moment, y=.data[[var]], fill=PMS), outlier.shape=NA, alpha=.5, width=.3, colour='black')+ #boxplot, see through, no outline, 
    geom_point(data= emmean_dataframe, aes(x = Moment, y = emmean, fill=PMS), position= position_dodge(0.3), size=4)+ #points representing the emmeans
    scale_fill_manual(values = c("blue", 'red', 'purple'), #colours used in plot, repressent PMDD, PMS and noPMS
                      name='', #legend gets no name
                      labels=c(paste0('noPMS \n n=', as.character(sum(data$PMS == "noPMS")/2)), paste0('PMS \n n=', as.character(sum(data$PMS == "PMS")/2)), paste0('PMDD \n n=',as.character(sum(data$PMS == "PMDD")/2))))+ #labels names
    guides(fill = guide_legend(reverse=TRUE))+ # show labels in different order 
    labs(y=title)+
    theme(
      legend.key.size=unit(1.3, 'cm'), # make keys of legend bigger
      legend.text=element_text(size=13), # text legend bigger
      plot.title = element_text(size=rel(2)), # plot title bigger
      panel.border = element_blank(), # no border panel (APA)
      panel.background = element_blank(), #white simple background
      axis.line = element_line(colour = "black"), # axis lines black
      panel.grid.major.y = element_line( size=.1, color="#dedede" ), #slight grey horizontal lines
      axis.text.x=element_text(size=rel(2)), #size x axis title
      axis.title.y=element_text(size=rel(1.5)), #size y axis title
      axis.title.x = element_blank()) # leave away extra x title (only 'foll' and 'lut')
}

