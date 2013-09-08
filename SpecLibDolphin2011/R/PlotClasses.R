PlotClasses <- function(lib = dolphin2011.meta) {
  
  pdf(file = "SpecLibDolphin2011_plot.pdf", width = 3.25, height = 6.09375)
  ## height = 4.0625 two panels

  grid.newpage()

  pushViewport(plotViewport(c(3, 2.75, 0.1, 0.1),
                            layout = grid.layout(3,1)))

  ## top panel - halogenation
  
  pushViewport(dataViewport(layout.pos.row = 1,
                            xData = lib$rt.1D,
                            yData = lib$rt.2D.adjusted))

  br <- lib[lib$halogens == "brominated", ]
  cl <- lib[lib$halogens == "chlorinated", ]
  mi <- lib[lib$halogens == "mixed", ]

  point.pch <- c(21, 23, 24)	
  point.fill <- c("#66C2A5", "#FC8D62", "#8DA0CB")
  point.line <- c("black", "black", "black")
  point.alpha <- 1
  point.lwd <- 0.8

  grid.points(x = cl$rt.1D,
              y = cl$rt.2D.adjusted,
              size = unit(2, "mm"),
              pch = point.pch[2],
              gp = gpar(col = point.line[2],
                fill = point.fill[2],
                alpha = point.alpha,
                lwd = point.lwd))

  grid.points(x = br$rt.1D,
              y = br$rt.2D.adjusted,
              size = unit(2, "mm"),
              pch = point.pch[1],
              gp = gpar(col = point.line[1],
                fill = point.fill[1],
                alpha = point.alpha,
                lwd = point.lwd))

  grid.points(x = mi$rt.1D,
              y = mi$rt.2D.adjusted,
              size = unit(2, "mm"),
              pch = point.pch[3],
              gp = gpar(col = point.line[3],
                fill = point.fill[3],
                alpha = point.alpha,
                lwd = point.lwd))

  # legend

  pushViewport(viewport(x = 0.175, y = 0.725, width = 0.3,
                        height = 0.4))

  legend.pos <- 1:4 / 4

  grid.text("A. Halogenation", x = 0, y = legend.pos[4],
            just = "left", gp = gpar(cex = 0.9))
  grid.points(x = 0.1, y = legend.pos[3], default.units = "npc",
              pch = point.pch[1], size = unit(2, "mm"),
              gp = gpar(col = point.line[1], fill = point.fill[1],
                lwd = point.lwd))
  grid.points(x = 0.1, y = legend.pos[2], default.units = "npc",
              pch = point.pch[2], size = unit(2, "mm"),
              gp = gpar(col = point.line[2], fill = point.fill[2],
                lwd = point.lwd))
  grid.points(x = 0.1, y = legend.pos[1], default.units = "npc",
              pch = point.pch[3], size = unit(2, "mm"),
              gp = gpar(col = point.line[3], fill = point.fill[3],
                lwd = point.lwd))

  grid.text("brominated", x = 0.2, y = legend.pos[3],
            gp = gpar(cex = 0.8), just = "left")
  grid.text("chlorinated", x = 0.2, y = legend.pos[2],
            gp = gpar(cex = 0.8), just = "left")
  grid.text("mixed Br/Cl", x = 0.2, y = legend.pos[1],
            gp = gpar(cex = 0.8), just = "left")

  popViewport()

  grid.rect()

  grid.yaxis(gp = gpar(cex = 0.75))
  
  popViewport()

  ## middle panel - non-typical identification examples

  pushViewport(dataViewport(layout.pos.row = 2,
                            xData = lib$rt.1D,
                            yData = lib$rt.2D.adjusted))

  mbp <-  lib[lib$category.2 == "MBP", ]
  pcde <- lib[lib$category.2 == "PCDE", ]
  cs <-   lib[lib$category.2 == "chlorinated styrene", ]

  point.fill <- c("#8DD3C7", "#FFFFB3", "#BEBADA")

  grid.points(x = mbp$rt.1D,
              y = mbp$rt.2D.adjusted,
              size = unit(2, "mm"),
              pch = point.pch[1],
              gp = gpar(col = point.line[1],
                fill = point.fill[1],
                alpha = point.alpha,
                lwd = point.lwd))

  grid.points(x = pcde$rt.1D,
              y = pcde$rt.2D.adjusted,
              size = unit(2, "mm"),
              pch = point.pch[2],
              gp = gpar(col = point.line[2],
                fill = point.fill[2],
                alpha = point.alpha,
                lwd = point.lwd))

  grid.points(x = cs$rt.1D,
              y = cs$rt.2D.adjusted,
              size = unit(2, "mm"),
              pch = point.pch[3],
              gp = gpar(col = point.line[3],
                fill = point.fill[3],
                alpha = point.alpha,
                lwd = point.lwd))

  # legend

  pushViewport(viewport(x = 0.175, y = 0.725, width = 0.3,
                        height = 0.4))

  legend.pos <- 1:4 / 4

  grid.text("B. Example Classes", x = 0, y = legend.pos[4],
            just = "left", gp = gpar(cex = 0.9))
  grid.points(x = 0.1, y = legend.pos[3], default.units = "npc",
              pch = point.pch[1], size = unit(2, "mm"),
              gp = gpar(col = point.line[1], fill = point.fill[1],
                lwd = point.lwd))
  grid.points(x = 0.1, y = legend.pos[2], default.units = "npc",
              pch = point.pch[2], size = unit(2, "mm"),
              gp = gpar(col = point.line[2], fill = point.fill[2],
                lwd = point.lwd))
  grid.points(x = 0.1, y = legend.pos[1], default.units = "npc",
              pch = point.pch[3], size = unit(2, "mm"),
              gp = gpar(col = point.line[3], fill = point.fill[3],
                lwd = point.lwd))

  grid.text("MBP", x = 0.2, y = legend.pos[3],
            gp = gpar(cex = 0.8), just = "left")
  grid.text("PCDE", x = 0.2, y = legend.pos[2],
            gp = gpar(cex = 0.8), just = "left")
  grid.text("PCS", x = 0.2, y = legend.pos[1],
            gp = gpar(cex = 0.8), just = "left")

  popViewport()

  grid.rect()

  grid.yaxis(gp = gpar(cex = 0.75))

  grid.text(label = "2nd dimension retention time (s)",
            vjust = -19, rot = 90, gp = gpar(cex = 0.75))
            
  
  popViewport()
  
  ## bottom panel - unknowns

  pushViewport(dataViewport(layout.pos.row = 3,
                            xData = lib$rt.1D,
                            yData = lib$rt.2D.adjusted))

  ug1 <- lib[lib$category.4 == "UG1", ]
  ug2 <- lib[lib$category.4 == "UG2", ]
  ug3 <- lib[lib$category.4 == "UG3", ]
  ug4 <- lib[lib$category.4 == "UG4", ]

  point.pch <- c(22, 23, 24, 25)
  point.line <- c("black", "black", "black", "black")
  point.fill <- c("#7FC97F", "#BEAED4", "#FDC086", "#FFFF99")

  grid.points(x = ug1$rt.1D,
              y = ug1$rt.2D.adjusted,
              size = unit(2, "mm"),
              pch = point.pch[1],
              gp = gpar(col = point.line[1],
                fill = point.fill[1],
                alpha = point.alpha,
                lwd = point.lwd))

  grid.points(x = ug2$rt.1D,
              y = ug2$rt.2D.adjusted,
              size = unit(2, "mm"),
              pch = point.pch[2],
              gp = gpar(col = point.line[2],
                fill = point.fill[2],
                alpha = point.alpha,
                lwd = point.lwd))
  
  grid.points(x = ug3$rt.1D,
              y = ug3$rt.2D.adjusted,
              size = unit(2, "mm"),
              pch = point.pch[3],
              gp = gpar(col = point.line[3],
                fill = point.fill[3],
                alpha = point.alpha,
                lwd = point.lwd))

  grid.points(x = ug4$rt.1D,
              y = ug4$rt.2D.adjusted,
              size = unit(2, "mm"),
              pch = point.pch[4],
              gp = gpar(col = point.line[4],
                fill = point.fill[4],
                alpha = point.alpha,
                lwd = point.lwd))

  # legend

  pushViewport(viewport(x = 0.175, y = 0.7, width = 0.3,
                        height = 0.45))

  legend.pos <- 1:5 / 5

  grid.text("C. Unknowns", x = 0, y = legend.pos[5],
            just = "left", gp = gpar(cex = 0.9))
  grid.points(x = 0.1, y = legend.pos[4], default.units = "npc",
              pch = point.pch[1], size = unit(2, "mm"),
              gp = gpar(col = point.line[1], fill = point.fill[1],
                lwd = point.lwd))
  grid.points(x = 0.1, y = legend.pos[3], default.units = "npc",
              pch = point.pch[2], size = unit(2, "mm"),
              gp = gpar(col = point.line[2], fill = point.fill[2],
                lwd = point.lwd))
  grid.points(x = 0.1, y = legend.pos[2], default.units = "npc",
              pch = point.pch[3], size = unit(2, "mm"),
              gp = gpar(col = point.line[3], fill = point.fill[3],
                lwd = point.lwd))
  grid.points(x = 0.1, y = legend.pos[1], default.units = "npc",
              pch = point.pch[4], size = unit(2, "mm"),
              gp = gpar(col = point.line[4], fill = point.fill[4],
                lwd = point.lwd))

  grid.text("Group 1", x = 0.2, y = legend.pos[4],
            gp = gpar(cex = 0.8), just = "left")
  grid.text("Group 2", x = 0.2, y = legend.pos[3],
            gp = gpar(cex = 0.8), just = "left")
  grid.text("Group 3", x = 0.2, y = legend.pos[2],
            gp = gpar(cex = 0.8), just = "left")
  grid.text("Group 4", x = 0.2, y = legend.pos[1],
            gp = gpar(cex = 0.8), just = "left")

  popViewport()

  grid.rect()

  grid.yaxis(gp = gpar(cex = 0.75))

  grid.xaxis(gp = gpar(cex = 0.75))

  grid.text(label = "1st dimension retention time (s)",
            vjust = 16, gp = gpar(cex = 0.75))
  
  dev.off()

}
