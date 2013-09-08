LibraryReport <- function(spectra = dolphin2011.spec,
                          metadata = dolphin2011.meta,
                          structures = paste(system.file(package = "SpecLibDolphin2011"), "/extdata/struct/", sep = ""),
                          pdf = TRUE,
                          pdf.file = "SpecLibDolphin2011.pdf",
                          pdf.title = "SpecLibDolphin2011 Library",
                          x.min = 40) {

  ## TODO make more informative error messages:
  ## TODO print which columns are absent
  ## TODO, which spectra are missing, maybe make warning
  
  require(grid)
  require(png)


  ## Extract structure files to temporary directory - not used
  ## unzip(zipfile = structures, exdir = tempdir())
  ## tmp.directory <- paste(tempdir(), "/struct/", sep = "")
  ## structure.list <- unlist(lapply(strsplit(dir(tmp.directory), split = "\\."), function(x) x[1]))

  structure.list <- unlist(lapply(strsplit(dir(structures), split = "\\."), function(x) x[1])) 
  
  
  ## Error checks
  
  meta.check <- c("compound",   "filename",
                  "sample",     "instrument",
                  "category.1", "category.2",
                  "category.3", "rt.1D",
                  "rt.2D",      "formula",
                  "comment") %in% names(metadata)
  
  if(FALSE %in% meta.check) {
    stop("one or more required metadata column names are not
          present")
  }

  
  spec.check <- c("filename", "mz", "intensity", "display", "identity") %in% names(spectra)

  if(FALSE %in% spec.check) {
    stop("one or more required spectra column names are not
          present")
  }

  
  metadata <- metadata[!is.na(metadata$filename), ]
  

  filename.check <- metadata$filename %in% unique(spectra$filename)

  if(FALSE %in% filename.check) {
    stop("metadata$filename contains strings not found in
          unique(spectra$filename)")
  }
  
  
  if (pdf == TRUE) {
    pdf(file = pdf.file, width = 10.5, height = 8, title = pdf.title,
        paper = "usr")
  }


  ## Make title page 

  if(pdf == TRUE) {

    grid.newpage()

    title.layout <- grid.layout(nrow = 2, ncol = 1,
                                heights = unit(c(0.1, 0.9), "npc"))
  
    pushViewport(viewport(layout = title.layout))

    pushViewport(viewport(layout.pos.row = 1))

    grid.text("SpecLibDolphin2011 Mass Spectral Library",
              x = 0.5, y = 0.5, gp = gpar(cex = 1.25))

    grid.lines(x = unit(c(0,1), "npc"), y = unit(c(0,0), "npc"))

    popViewport()

    pushViewport(viewport(layout.pos.row = 2))

    grid.text("Massachusetts Common Dolphin (Delphinus delphis) Blubber", y = 0.9, gp = gpar(cex = 2))

    grid.text("Literature Reference:" , x = 0.5, y = 0.8, gp = gpar(cex = 1.25))
    
    grid.text("Eunha Hoh, Nathan G. Dodder, Steven J. Lehotay, Kristin C. Pangallo, Christopher M. Reddy, and Keith A. Maruya 
\"Nontargeted Comprehensive Two-Dimensional Gas Chromatography/Time-of-Flight Mass Spectrometry Method and Software for 
Inventorying Persistent and Bioaccumulative Contaminants in Marine Environments\", 
Environmental Science and Technology, 2012, 46 (15), 8001-8008. DOI: 10.1021/es301139q.",
              y = 0.7, gp = gpar(cex = 1))
    
    grid.text("Web Reference: ", x = 0.5, y = 0.55, gp = gpar(cex = 1.25))
    grid.text("http://orgmassspecr.r-forge.r-project.org/",
              y = 0.5, gp = gpar(cex = 1.25)) 

    session.info <- sessionInfo()

    grid.text(paste("Prepared:", Sys.time()), y = 0.4, gp = gpar(cex = 1.25))

    grid.text(paste("SpecLibDolphin2011 version",
                    session.info$otherPkgs$SpecLibDolphin2011$Version),
              x = 0.5, y = 0.3, gp = gpar(cex = 1))
  
    grid.text(paste("OrgMassSpecR version",
                    session.info$otherPkgs$OrgMassSpecR$Version),
              x = 0.5, y = 0.25, gp = gpar(cex = 1))
  
    grid.text(paste("png version",
                    session.info$otherPkgs$png$Version),
              x = 0.5, y = 0.2, gp = gpar(cex = 1))
  
    grid.text(session.info$R.version$version.string,
             x = 0.5, y = 0.15, gp = gpar(cex = 1))

    popViewport()
    
  }


  ## DrawSpectrum makes one page for each spectrum. The order is
  ## determined by the order in the meta.data
  
  DrawSpectrum <- function(current.filename, structure.list, structures) {
  
    current.spectrum <- spectra[spectra$filename == current.filename, ]
    current.metadata <- metadata[metadata$filename == current.filename, ]

    if (pdf == FALSE) 
      dev.new(width = 10.5, height = 8)

    message("Making spectrum for ", current.metadata$compound)

    grid.newpage()

    ## Write information at top of page

    spec.layout <- grid.layout(nrow = 3, ncol = 1, 
                               heights = unit(c(0.15, 0.5, 0.3), "npc"))

    pushViewport(viewport(layout = spec.layout))
    
    pushViewport(viewport(layout.pos.row = 1, name = "chemical.name"))
    
    grid.text(paste("Name:", current.metadata$compound), 
              x = 0.025, y = 1, hjust = 0, gp = gpar(cex = 1.25))

    grid.text(paste("Elemental Formula:", current.metadata$formula), 
              x = 0.7, y = 1, hjust = 0)

    grid.text(paste("Sample: ", current.metadata$sample), 
              x = 0.025, y = 0.75, hjust = 0)
    
    grid.text(paste("Instrument: ", current.metadata$instrument), 
              x = 0.025, y = 0.50, hjust = 0)

    grid.text(paste("RT (s) (1D): ", current.metadata$rt.1D, ", ",
                    "RT (s) (2D): ", current.metadata$rt.2D,
                    sep = ""),
              x = 0.025, y = 0.25, hjust = 0)

    grid.text(paste("Source:", current.metadata$category.1),
              x = 0.7, y = 0.75, hjust = 0)
    grid.text(paste("Class:", current.metadata$category.2),
              x = 0.7, y = 0.50, hjust = 0)
    grid.text(paste("Identification:", current.metadata$category.3),
              x = 0.7, y = 0.25, hjust = 0)
    grid.text(paste("Comment:", current.metadata$comment),
              x = 0.025, y = 0, hjust = 0)
    
    popViewport()


    ## Draw spectrum

    pushViewport(viewport(layout.pos.row = 2, name = "mass.spectrum"))

    current.spectrum$percent.intensity <- with(current.spectrum,
      intensity / max(intensity) * 100)

    ## calculate molecular weight to set x-axis upper limit

    if (!is.na(current.metadata$formula)) {
      mw <- MolecularWeight(formula = ListFormula(current.metadata$formula))
      x.max <- mw + (mw * 0.03)
    } else {
      m <- max(current.spectrum$mz) 
      x.max <- m + (m * 0.03)
    }

    plot.data <- current.spectrum[current.spectrum$mz >= x.min &
                                  current.spectrum$mz <= x.max, ]
    
    pushViewport(plotViewport(c(3.75, 3.5, 1.5, 1)))
    pushViewport(dataViewport(xscale = c(x.min, x.max),
                              yscale = c(0, 110)))
    
    grid.rect()
    p.ticks <- pretty(plot.data$mz, n = 10)
    x.ticks <- p.ticks[p.ticks >= x.min & p.ticks <= x.max]
    grid.xaxis(at = x.ticks)
    grid.yaxis(at = c(0, 25, 50, 75, 100))
    
    grid.segments(plot.data$mz,
                  plot.data$percent.intensity,
                  plot.data$mz,
                  rep(0, length(plot.data$intensity)),
                  default.units = "native",
                  gp = gpar(lwd = 0.75))

    ## print m/z values in plot
    
    display.values <- plot.data$mz[plot.data$display == TRUE]
    if (length(display.values) > 0) {
    grid.text(display.values,
              x = display.values,
              y = plot.data$percent.intensity[plot.data$display == TRUE] + 5,
              default.units = "native",
              gp = gpar(col = "blue"))
    }
    
    grid.text("intensity (%)", x = unit(-3.2, "lines"), rot = 90)
    grid.text("m/z", y = unit(-2.5, "lines"))

    ## print mark indicating identified ions
    
    id.data <- plot.data[!is.na(plot.data$identity), ]
    if (nrow(id.data) > 0) {
      
      mark.color <- vector(mode = "character", length = nrow(id.data))
      mark.color[grepl("interference", id.data$identity)] <- "grey"
      mark.color[!grepl("interference", id.data$identity)] <- "red"
                           
      grid.segments(plot.data$mz[!is.na(plot.data$identity)],
                  rep(115, length(plot.data$percent.intensity[plot.data$display == TRUE])),
                  plot.data$mz[!is.na(plot.data$identity)],
                  rep(110, length(plot.data$percent.intensity[plot.data$display == TRUE])),
                  default.units = "native",
                  gp = gpar(lwd = 1, col = mark.color))
    }
    
    popViewport(2)

    popViewport()


    ## Define area below spectrum

    pushViewport(viewport(layout.pos.row = 3, name = "structure"))
    info.layout <- grid.layout(nrow = 1, ncol = 2)
    pushViewport(viewport(layout = info.layout))
    

    ## Display structure

    pushViewport(viewport(x = unit(0.25, "npc"),
                          y = unit(0.6, "npc"),
                          width = unit(0.4, "npc"),
                          height = unit(0.9, "npc")))
    
    ## Display raster image, code follows the example in png::readPNG

    if(as.character(current.filename) %in% structure.list) {

      img <- readPNG(paste(structures, current.filename, ".png", sep = ""))

      if (names(dev.cur()) == "windows" & pdf != TRUE) { # windows graphics device lacks some features
        
        ## to flatten the image to eliminate semi-transparency
        transparent <- img[,,4] == 0
        img <- as.raster(img[,,1:3])
        img[transparent] <- NA
     
        ## interpolate must be FALSE
        grid.raster(img, interpolate = FALSE)  
     
      } else {
                                        
        grid.raster(img)
     
      }
    }
    

    ## Write filename in corner

    grid.text(paste("Filename:", current.metadata$filename),
                    x = -0.075,
                    y = -0.15,
                    just = "left",
              gp = gpar(col = "dark grey"))
    
    popViewport()

    
    ## Write fragment ion identifications
    
    pushViewport(viewport(layout.pos.col = 2))

    grid.text(c("m/z", "Identity"), x = unit(c(0.1, 0.2), "npc"), 
              y = unit(1, "npc"), gp=gpar(col="red"), hjust = 0)

    grid.text(current.spectrum$mz[!is.na(current.spectrum$identity)], 
              x = 0.1, 
              y = c(0.9, 0.8, 0.7, 0.6, 0.5,
                0.4, 0.3, 0.2, 0.1, 0)[1:length(na.omit(current.spectrum$identity))], 
              hjust = 0)

    grid.text(current.spectrum$identity[!is.na(current.spectrum$identity)], 
              x = 0.2, 
              y = c(0.9, 0.8, 0.7, 0.6, 0.5,
                0.4, 0.3, 0.2, 0.1, 0)[1:length(na.omit(current.spectrum$identity))], 
              hjust = 0)

  }

  sapply(metadata$filename, DrawSpectrum,
         structure.list = structure.list,
         structures = structures)

  if (pdf == TRUE)
    dev.off()

}

