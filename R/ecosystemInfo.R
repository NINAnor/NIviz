## Assemble ecosystem info

EcoSysInfo <- data.frame(
  ecosystemID = c(1:11),
  ecosystem = c("Ferskvann", "Fjell", "Havbunn", "Hav-pelagisk", "Kystvann-bunn", "Kystvann-pelagisk", "Våtmark", "Skog", "Åpent lavland", "Hav", "Kyst")
)

saveRDS(EcoSysInfo, file = "data/EcosystemInfo.rds")



