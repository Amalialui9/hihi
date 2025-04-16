setwd("~/utsspasial")

library(sf)
library(raster)
library(spdep)
library(spatialreg)
library(readxl)
library(ggplot2)
library(viridis)

# Input shapefile sebagai sf object
spindonesia <- st_read("gadm41_IDN_1.shp")

# Cek kolom yang tersedia
names(spindonesia)

# Tampilkan peta Indonesia
ggplot(spindonesia) +
  geom_sf(fill = "lightblue", color = "black") +
  labs(title = "Peta Indonesia") +
  theme_minimal()

# matrix pembobot queen
spindonesia_sp <- as(spindonesia, "Spatial")
queen.nb <- poly2nb(spindonesia_sp, snap = 1e-6)
queen.listw <- nb2listw(queen.nb, zero.policy = TRUE)
queen.Provinsi <- queen.listw

rook.nb <- poly2nb(spindonesia_sp, queen = FALSE)
rook.listw <- nb2listw(rook.nb, zero.policy = TRUE)
rook.Provinsi <- rook.listw

bobot.queen <- listw2mat(queen.listw)
write.csv(bobot.queen, file = "Matriks_Bobot_Queen_Provinsi.csv", row.names = FALSE)
bobot.rook <- listw2mat(rook.listw)
write.csv(bobot.rook, file = "Matriks_Bobot_Rook_Provinsi.csv", row.names = FALSE)

data <- read.csv("~/utsspasial/dataspasialbenenr.csv", header=TRUE)
head(data, 5)

spindonesia$Y <- data$AHH
spindonesia$X1 <- data$sanitasilayak
spindonesia$X2 <- data$faskes
spindonesia$X3 <- data$RLS
spindonesia$X4 <- data$IPM

# Visualisasi tiap variabel dengan ggplot2
for (var in c("Y", "X1", "X2", "X3", "X4")) {
  print(
    ggplot(spindonesia) +
      geom_sf(aes_string(fill = var), color = "white") +
      scale_fill_viridis_c() +
      labs(title = paste("Peta", var)) +
      theme_minimal()
  )
}

moran.test(spindonesia$Y, queen.listw, randomisation = FALSE)
moran.plot(spindonesia$Y, queen.listw)

model1 <- lm(Y ~ X1 + X2 + X3 + X4, data = spindonesia)
LM <- lm.LMtests(model1, queen.listw, test = "all")

# Hapus data dengan NA neighbor
no_neighbors <- which(card(queen.nb) == 0)
keep_index <- !(1:length(queen.nb) %in% no_neighbors)
queen.nb.clean <- subset(queen.nb, subset = keep_index)
queen.listw.clean <- nb2listw(queen.nb.clean, zero.policy = TRUE)
spindonesia_clean <- spindonesia[keep_index, ]

model_sar <- lagsarlm(Y ~ X1 + X2 + X3 + X4, data = spindonesia_clean, listw = queen.listw.clean)
summary(model_sar)

model_sem <- errorsarlm(Y ~ X1 + X2 + X3 + X4, data = spindonesia_clean, listw = queen.listw.clean)
summary(model_sem)

mean_Y <- mean(spindonesia_clean$Y, na.rm = TRUE)
var_Y <- var(spindonesia_clean$Y, na.rm = TRUE)
mean_Y
var_Y

model_poisson <- glm(Y ~ X1 + X2 + X3 + X4, family = poisson(), data = spindonesia_clean)
dispersion_stat <- sum(residuals(model_poisson, type = "pearson")^2) / model_poisson$df.residual
dispersion_stat
