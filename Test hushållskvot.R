library(httr)
library(jsonlite)
library(tidyverse)
library(readxl)
library(ggplot2)
library(dplyr)

# Ange det senaste året med statistik
stat_ar <- 2023

# Ange det år prognosen ska gå till
slut_ar <- 2033

# Ange de region- alt. kommunkoder som ska ingå i beräkningen
omrade <- c("2026", "2029", "2031", "2080", "2081", "2082")

# ----

# Avgör tidsintervalen för beräkningen 
ar_prog <- seq(stat_ar + 1, slut_ar)
ar_hist <- seq(2006, stat_ar)

# OBS!!! Se över hur det sista året hoss SCB är formaterat
# Anger vilka åldrar ska vara med
alder <- seq(16, 99)

# SCB API-bas-URL och endpoint
base_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/"

# Lägg till den specifika dataset-endpointen för befolkningsprognosen
data_url_prog <- paste0(base_url, "BE/BE0401/BE0401A/BefProgRegFakN") 

# Hämta metadata för att förstå datasetets struktur
response <- GET(data_url_prog)
metadata <- content(response, "text", encoding = "UTF-8")
metadata_json <- fromJSON(metadata)

# Skapa JSON-begäran för POST-anrop
query_prog <- list(
  query = list(
    list(code = "Tid", selection = list(filter = "item", values = as.character(ar_prog))),
    list(code = "Region", selection = list(filter = "item", values = as.character(omrade))),
    list(code = "Alder", selection = list(filter = "item", values = as.character(alder)))
  ),
  response = list(format = "json")
)

# Skicka POST-anrop och hämta data
response <- POST(data_url_prog, body = toJSON(query_prog, auto_unbox = TRUE), encode = "json")
data_json_prog <- content(response, "text", encoding = "UTF-8")
data <- fromJSON(data_json_prog)


# Omvandla till data.frame
data_prog <- as.data.frame(data$data)

data_prog <- data_prog %>%
  mutate(key = gsub('c\\(|\\)|"', '', key)) %>%
  separate(key, into = c("Region", "Alder", "Tid"), sep = ",")




# Hämtar historisk demografisk data
data_url_hist <- paste0(base_url, "/BE/BE0101/BE0101A/BefolkningNy")

response <- GET(data_url_hist)
metadata <- content(response, "text", encoding = "UTF-8")
metadata_json <- fromJSON(metadata)

query_hist <- list(
  query = list(
    list(code = "Tid", selection = list(filter = "item", values = as.character(ar_hist))),
    list(code = "Region", selection = list(filter = "item", values = as.character(omrade))),
    list(code = "Alder", selection = list(filter = "item", values = as.character(alder)))
  ),
  response = list(format = "json")
)

response <- POST(data_url_hist, body = toJSON(query_hist, auto_unbox = TRUE), encode = "json")
data_json_hist <- content(response, "text", encoding = "UTF-8")
data <- fromJSON(data_json_hist)


data_hist <- as.data.frame(data$data)

data_hist <- data_hist %>%
  mutate(key = gsub('c\\(|\\)|"', '', key)) %>%
  mutate(values = gsub('c\\(|\\)|"', '', values)) %>%
  separate(key, into = c("Region", "Alder", "Tid"), sep = ",") %>%
  separate(values, into = c("values", "bef_forandring", sep = ","))

data_hist <- data_hist %>%
  select(Region, Alder, Tid, values)



# Hämtar bostads data
data_url_bost <- paste0(base_url, "/BO/BO0104/BO0104D/BO0104T04")


response <- GET(data_url_bost)
metadata <- content(response, "text", encoding = "UTF-8")
metadata_json <- fromJSON(metadata)

query_bost <- list(
  query = list(
    list(code = "Tid", selection = list(filter = "item", values = as.character(ar_hist))),
    list(code = "Region", selection = list(filter = "item", values = as.character(omrade)))
      ),
  response = list(format = "json")
)

response <- POST(data_url_bost, body = toJSON(query_bost, auto_unbox = TRUE), encode = "json")
data_json_bost <- content(response, "text", encoding = "UTF-8")
data <- fromJSON(data_json_bost)

data_bost <- as.data.frame(data$data)

data_bost <- data_bost %>%
  mutate(key = gsub('c\\(|\\)|"', '', key)) %>%
  mutate(values = gsub('c\\(|\\)|"', '', values)) %>%
  separate(key, into = c("Region", "Tid"), sep = ",") %>%
  rename(bostader = values)


# ----------------------------------

#Importera kvot data
kvot <- read_xlsx("kvot.xlsx")

kvot <- kvot %>%
  mutate(Alder = as.numeric(Alder)) 

# Skapar en gemensam df för all demografisk data.
dem_data <- rbind(data_hist, data_prog) %>%
  mutate(values = as.numeric(values)) %>%
  mutate(Alder = as.numeric(Alder)) %>%
  mutate(Tid = as.numeric(Tid))

dem_data <- dem_data %>%
  left_join(kvot, by = "Alder")


dem_data <- dem_data %>%
  mutate(dem_data, forva_hushall = values * kvot)

dem_data <- dem_data %>%
  group_by(Region, Tid) %>%
  summarise(forv_hushall_per_ar = sum(forva_hushall, na.rm = TRUE))

data_bost <- data_bost %>%
  mutate(Tid = as.numeric(Tid))

total <- dem_data %>%
  left_join(data_bost, by = c("Region", "Tid")) %>%
  mutate(bostader = as.numeric(bostader))

#Beräkna över- underskott av bostäder
bost_prog <- total %>%
  mutate(underskott = forv_hushall_per_ar - bostader) # %>%
#  mutate(underskott = ifelse(underskott < 0, 0, underskott)) %>% frågan om denna ska få vara kvar. kan vara bra att veta inför andra beräkningar
#  mutate(ackumulerad_underskott = cumsum(underskott)) 

forandring <- bost_prog %>%
  filter(Tid %in% c(stat_ar, slut_ar)) %>%
  select(Region, forv_hushall_per_ar)

forandring <- bost_prog %>%
  filter(Tid %in% c(stat_ar, slut_ar)) %>%
  arrange(Region, Tid) %>% 
  group_by(Region) %>%
  summarise(skillnad = diff(forv_hushall_per_ar)) %>%
  mutate(vaxer_regionen = skillnad > 0)

bost_prog <- bost_prog %>%
  left_join(forandring, by = "Region")
  
if (bost_prog) {
  prognos_vaxer <- prognosDF # OBS!!!! Fixa denna är inte klar. Vi behöver implementera prognos data.
  mutate(bostader = ifelse(Tid == stat_ar + 1, hushall_per_ar * 1,01))
}
  
#Checkar om antalet hushåll förväntas växa. Om SANT läggs en buffert på antalet bostäder som behövs




#Beräknar den procentuella ökningen bland hushållen
tidigaste_ar_talet <- min(bostader_mot_husall$ar, na.rm = TRUE) #Här tar vi reda på det minsta värdet

forandring <- bostader_mot_husall %>%
  filter(ar %in% c(tidigaste_ar_talet, stat_ar)) %>% #stat_ar bör kanske vara sista året i framskrivningen istället
  select(ar, hushall_per_ar)
foran <- forandring %>%
  summarise(forandring_mellan_ar = hushall_per_ar[2] - hushall_per_ar[1])

#Checkar om antalet hushåll förväntas växa. Om SANT läggs en buffert på antalet bostäder som behövs
vaxer_regionen <- all(foran$forandring_mellan_ar >= 0)

if (vaxer_regionen) {
  prognos_vaxer <- prognosDF # OBS!!!! Fixa denna är inte klar. Vi behöver implementera prognos data.
  mutate(prognos = ifelse(ar == stat_ar + 1, hushall_per_ar * 1,01))
}
message("Körningen är klar!")

