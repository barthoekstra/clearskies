library(tidyverse)
library(bioRad)
library(pbmcapply)
library(raster)
library(ggnewscale)

files <- list.files(path = "data/RBC/RDS", full.names = TRUE)

plot_ppi_vis <- function(filepath) {
  rbc <- readRDS(filepath)
  data <- do.call(function(y) rbc$data[y], list(c("VIR", "rain")))
  data <- raster::as.data.frame(stack(data), xy = T)
  # bbox <- coord_fixed(xlim = c(-160000, 160000), ylim = c(-160000, 160000))
  zlim = c(0, 5000)
  index <- which(data[, 3] < zlim[1])
  if (length(index) > 0) {
    data[index, 3] <- zlim[1]
  }
  index <- which(data[, 3] > zlim[2])
  if (length(index) > 0) {
    data[index, 3] <- zlim[2]
  }
  # rindex <- which(data[, 4] < 1)
  # # if (length(rindex) > 0) {
  #   data[data$rain < 1, 4] <- NA
  # }
  data$VIR[round(data$VIR) == 0] <- NA
  data$rain[data$rain < 1] <- NA
  
  ggplot(data = data) +
    geom_raster(aes(x = x, y = y, fill = VIR)) +
    scale_fill_viridis_c(na.value = "transparent", option = "plasma", trans = "log10") +
    new_scale_fill() +
    geom_raster(aes(x = x, y = y, fill = rain, alpha = rain)) +
    scale_fill_gradient(na.value = "transparent", low = "white", high = "#7fb4ff") +
    scale_alpha_continuous(range = c(0.9, 1)) +
    theme_dark() +
    # bbox +
    labs(title = paste0(rbc$radar, ": ", rbc$datetime)) +
    theme(legend.position = "none")
  
  ggsave(paste("data/plots/", tools::file_path_sans_ext(basename(filepath)), ".png", sep = ""))
  print(paste0("Finished: ", filepath))
}

# filepath <- "data/RBC/RDS/NLDHL_RBC_20201002T0540.RDS"
# plot_ppi_vis(filepath)
# plot_ppi_vis("data/RBC/RDS/NLDHL_RBC_20201002T0540.RDS")
# lapply(files, plot_ppi_vis)
# cores <- 6
# processing <- pbmclapply(files, plot_ppi_vis, mc.cores = cores, mc.preschedule = FALSE, mc.silent = FALSE)
plot_ppi_vis("data/RBC/RDS/NLHRW_RBC_20201001T1740.RDS")
