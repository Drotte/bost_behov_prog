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

# OBS!!! Få in denna alder <- c(seq(16, 99), "100+") och formatera så att även de som är över hundra kan räknas in. Alt så förkastas dem då de är relativt få
# Anger vilka åldrar ska vara med
alder <- seq(16, 99)

# Lägg till den specifika dataset-endpointen för befolkningsprognosen
data_url_prog <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgRegFakN" 

### Kör denna kod för att få metadata:
# response <- GET(data_url_prog)
# metadata <- content(response, "text", encoding = "UTF-8")
# metadata_json <- fromJSON(metadata)

# Här skapas en lista över vilka variabler som ska hämtas från SCB baserat på tidigare angivna värden. En sådan lista skapas för varje förfrågan till SCB
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
data_url_hist <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"

### Kör denna kod för att få metadata:
# response <- GET(data_url_hist)
# metadata <- content(response, "text", encoding = "UTF-8")
# metadata_json <- fromJSON(metadata)

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
data_url_bost <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BO/BO0104/BO0104D/BO0104T04"

### Kör denna kod för att få metadata:
# response <- GET(data_url_bost)
# metadata <- content(response, "text", encoding = "UTF-8")
# metadata_json <- fromJSON(metadata)

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



#Importera kvot data
kvot <- read_xlsx("kvot.xlsx")

kvot <- kvot %>%
  mutate(Alder = as.numeric(Alder)) 

data_bost <- data_bost %>%
  mutate(Tid = as.numeric(Tid))

# Skapar en gemensam df för all demografisk data.
dem_data <- rbind(data_hist, data_prog) %>%
  mutate(values = as.numeric(values)) %>%
  mutate(Alder = as.numeric(Alder)) %>%
  mutate(Tid = as.numeric(Tid))


dem_data <- dem_data %>%
  left_join(kvot, by = "Alder") %>%
  mutate(dem_data, forva_hushall = values * kvot) %>%
  group_by(Region, Tid) %>%
  summarise(forv_hushall_per_ar = sum(forva_hushall, na.rm = TRUE)) 

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
  arrange(Region, Tid) %>% 
  group_by(Region) %>%
  summarise(skillnad = diff(forv_hushall_per_ar)) %>%
  mutate(vaxer_regionen = skillnad > 0)

bost_prog <- bost_prog %>%
  left_join(forandring, by = "Region")

bost_prog <- bost_prog %>%
  mutate(bostads_prognos = ifelse(vaxer_regionen & Tid >= stat_ar & Tid <= slut_ar, 
                                      forv_hushall_per_ar * 1.01, 
                                      NA))

total_utvalda_regioner <- bost_prog %>%
  group_by(Tid) %>%
  summarise(
    forv_hushall_per_ar = sum(forv_hushall_per_ar, na.rm = TRUE),
    bostader = sum(bostader, na.rm = TRUE),
    underskott = sum(underskott, na.rm = TRUE),
    skillnad = sum(skillnad, na.rm = TRUE),
    bostads_prognos = sum(bostads_prognos, na.rm = TRUE)
  ) %>%
  mutate(Region = "Total") %>%
  select(Region, everything())%>%
  # TA BORT OCH GÖR PÅ NÅGOT SNYGGARE SÄTT. Ersätt alla 0 med NA i hela dataframen
  mutate(across(everything(), ~ ifelse(. == 0, NA, .)))


ggplot(bost_prog, aes(x = Tid)) +
  geom_line(aes(y = bostader, color = "Bostäder"), size = 1) +
  geom_line(aes(y = bostads_prognos, color = "Bostadsprognos"), size = 1, linetype = "dashed") +
  labs(
    title = "Utveckling av bostäder och prognos över tid per region",
    x = "Tid",
    y = "Antal",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Bostäder" = "blue", "Bostadsprognos" = "red")) +
  theme_minimal() +
  facet_wrap(~ Region, scales = "free_y")


ggplot(total_utvalda_regioner, aes(x = Tid)) +
  geom_line(aes(y = bostader, color = "Bostäder"), size = 1) +
  geom_line(aes(y = bostads_prognos, color = "Bostadsprognos"), size = 1, linetype = "dashed") +
  labs(
    title = "Utveckling av bostäder och prognos över tid",
    x = "Tid",
    y = "Antal",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Bostäder" = "blue", "Bostadsprognos" = "red")) +
  theme_minimal()
  
message("Körningen är klar!")
