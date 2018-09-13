rm(kplrdata)
rm(k8462934)
files <- list.files(path="./008462934", pattern="*.fits", full.names=TRUE, recursive=FALSE)
for (file in files){
  if (!exists("kplrdata")){
    kplrdata <- readFrameFromFITS(file, hdu = 1)
    # PDCSAP_FLUX data need to be normalized per file before appending
    dmed <- median(kplrdata$PDCSAP_FLUX, na.rm = TRUE)
    kplrdata <- kplrdata %>% mutate(norm_flux = PDCSAP_FLUX / dmed)
    rm(dmed)
  }
  if (exists("kplrdata")){
    temp_dataset <- readFrameFromFITS(file, hdu = 1)
    # PDCSAP_FLUX data need to be normalized per file before appending
    tmed <- median(temp_dataset$PDCSAP_FLUX, na.rm = TRUE)
    temp_dataset <- temp_dataset %>% mutate(norm_flux = PDCSAP_FLUX / tmed)
    kplrdata <- rbind(kplrdata, temp_dataset)
    rm(temp_dataset)
    rm(tmed)
  }
}
k8462934 <- kplrdata %>% select(TIME, PDCSAP_FLUX, norm_flux)
g <- ggplot(k8462934, aes(TIME, norm_flux))
g + geom_line(aes(TIME), size=0.5, color='blue')

