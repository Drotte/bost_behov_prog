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

# Visa metadata
print(metadata_json)
view(metadata_json)

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
  separate(key, into = c("Region", "Ålder", "Tid"), sep = ",")


# Lägg till den specifika dataset-endpointen för nuvarande och tidigare befolkning
data_url_hist <- paste0(base_url, "/BE/BE0101/BE0101A/BefolkningNy")

# OBS!!! Kommer säkert behöva justeras. Hoppas att det inte är några problem att skriva över tidigare värden
#Hämta metadata för att förstå datasetets struktur
response <- GET(data_url_hist)
metadata <- content(response, "text", encoding = "UTF-8")
metadata_json <- fromJSON(metadata)

# Skapa JSON-begäran för POST-anrop
query_hist <- list(
  query = list(
    list(code = "Tid", selection = list(filter = "item", values = as.character(ar_hist))),
    list(code = "Region", selection = list(filter = "item", values = as.character(omrade))),
    list(code = "Alder", selection = list(filter = "item", values = as.character(alder)))
  ),
  response = list(format = "json")
)

# Skicka POST-anrop och hämta data
response <- POST(data_url_hist, body = toJSON(query_hist, auto_unbox = TRUE), encode = "json")
data_json_hist <- content(response, "text", encoding = "UTF-8")
data <- fromJSON(data_json_hist)


# Omvandla till data.frame
data_hist <- as.data.frame(data$data)

data_hist <- data_hist %>%
  mutate(key = gsub('c\\(|\\)|"', '', key)) %>%
  mutate(values = gsub('c\\(|\\)|"', '', values)) %>%
  separate(key, into = c("Region", "Ålder", "Tid"), sep = ",") %>%
  separate(values, into = c("values", "bef_forandring", sep = ","))

data_hist <- data_hist %>%
  select(Region, Ålder, Tid, values)




# Lägg till den specifika dataset-endpointen för nuvarande och tidigare bostadsbestånd
data_url_bost <- paste0(base_url, "XXXXXXX")

# OBS!!! Kommer säkert behöva justeras. Hoppas att det inte är några problem att skriva över tidigare värden
#Hämta metadata för att förstå datasetets struktur
response <- GET(data_url_bost)
metadata <- content(response, "text", encoding = "UTF-8")
metadata_json <- fromJSON(metadata)

# Skapa JSON-begäran för POST-anrop
query_bost <- list(
  query = list(
    list(code = "Tid", selection = list(filter = "item", values = as.character(ar_hist))),
    list(code = "Region", selection = list(filter = "item", values = as.character(omrade)))
      ),
  response = list(format = "json")
)

# Skicka POST-anrop och hämta data
response <- POST(data_url_bost, body = toJSON(query_bost), encode = "json")
data_json <- content(response, "text", encoding = "UTF-8")
data <- fromJSON(data_json)

# Omvandla till data.frame
data_bost <- as.data.frame(data$data)
print(data_bost)


# ----------------------------------

#Importera kvot data
kvot <- read_xlsx()

#Importera och formatera demografisk data
test_df <- read_xlsx("test.xlsx")

test_df <- test_df %>%
  mutate(test_df, man_kvot = man * kvot,
         kvinna_kvot = kvinna * kvot,
         pro_hushall = man_kvot + kvinna_kvot)


Total_hushall_per_ar <- test_df %>%
  group_by(ar) %>%
  summarize(hushall_per_ar = sum(pro_hushall, na.rm = TRUE))


#Importera data bostäder
test_bostader <- read_excel("test_bostader.xlsx")

# väver ihop två df grupperat på år
bostader_mot_husall <- left_join(Total_hushall_per_ar, test_bostader, by = "ar")

# beräknar över / underskott
bostader_mot_husall <- bostader_mot_husall %>%
  mutate(bostader_mot_husall, ovar_underskott = hushall_per_ar - bostader)

bostader_mot_husall <- bostader_mot_husall %>%
  mutate(ackumulerad_underskott = cumsum(ovar_underskott))
  
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

