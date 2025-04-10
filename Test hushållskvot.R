library(tidyverse)
library(pxweb)
library(pacman)

# Ange det år prognosen ska gå till
slut_ar <- 2033

# Ange de region- alt. kommunkoder som ska ingå i beräkningen.
omrade <- c("2026", "2029", "2031", "2080", "2081", "2082")

# ----

# Ladda in funktioner som Region Dalarna har gjort
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/refs/heads/main/func_API.R")

# Hämtar historisk demografisk data från SCB. 2006-angivet i "stat-ar"

px_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy" 

## Kör denna kod för att få metadata:
px_meta <- pxweb_get(px_url)

max_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "Tid" ) %>% 
  max()%>% 
  as.numeric() 

hist_tid <- c(2006:max_ar) %>% 
  as.character()

query <- list(
  Region = omrade,
  Alder = c(16:99, "100+"),
  Tid = hist_tid,
  Kon = "*",
  ContentsCode = "BE0101N1"
)

px_uttag <- pxweb_get(px_url, query = query) 

data_hist <- px_uttag %>% 
  as.data.frame()

# Hämtar SCBs framskrivning
px_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgRegFakN" 

px_meta <- pxweb_get(px_url)


min_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "Tid" ) %>% 
  min() %>% 
  as.numeric() 
if (min_ar == max_ar) {
  min_ar <- min_ar + 1 # Finns här för att det inte ska råka bli dubbla år när framskrivningen och befolkningsstatistiken överlappar
}

ar_prog <- c(min_ar:slut_ar) %>% 
  as.character()

query <- list(
  Region = omrade,
  Alder = c(16:99, "100+"),
  Tid = ar_prog,
  Kon = "*",
  ContentsCode = "000005RC"
)

px_uttag <- pxweb_get(px_url, query = query) 

data_prog <- px_uttag %>% 
  as.data.frame()

data_prog <- data_prog %>%
  cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
          select(regionkod = "Region")) %>% 
  relocate(regionkod, .before = region)

data_prog <- data_prog %>% 
  rename(Folkmängd = Antal)

data_prog <- full_join(
  data_prog,
  data_hist,
  by = NULL)

# Skapa df för att beräkna hushållskvoter
# Män
alder <- c(16:99, "100+")

alder <- c(paste(alder, "år"))

# Nationella kvoter män och 1-årsintervall 
kvot <- c(0.003616637, 0.011083744, 0.027061045, 0.076774194, 0.151074589, 
          0.253188776, 0.36560433, 0.439585731, 0.496484586, 0.533262936, 
          0.574934726, 0.604477612, 0.608262817, 0.619094978, 0.633168317, 
          0.65, 0.655226209, 0.695986806, 0.719082261, 0.727739726, 
          0.731940299, 0.746390458, 0.749056604, 0.73657289, 0.730563003, 
          0.739372822, 0.74507138, 0.762465374, 0.758120249, 0.758691207, 
          0.762508339, 0.772336201, 0.770819884, 0.769961977, 0.767769608, 
          0.778365667, 0.778550725, 0.778235294, 0.779445727, 0.789320952, 
          0.791466346, 0.79081316, 0.800516462, 0.800954329, 0.800957592, 
          0.808926081, 0.831258645, 0.831920904, 0.839805825, 0.842696629, 
          0.838801712, 0.836956522, 0.83908046, 0.827740492, 0.820089955, 
          0.819880419, 0.810534016, 0.819732938, 0.83866171, 0.84423676, 
          0.854132901, 0.871278459, 0.86763285, 0.858064516, 0.880473373, 
          0.885714286, 0.885714286, 0.881508079, 0.912423625, 0.904076739, 
          0.901554404, 0.909836066, 0.908794788, 0.915611814, 0.967391304, 
          0.967391304, 0.967391304, 0.967391304, 0.967391304, 0.967391304, 
          0.967391304, 0.967391304, 0.967391304, 0.967391304, 0.967391304
)

kon <- c(rep("män", 85))

data_kvot <- data.frame(kvot = kvot, ålder = alder, kön = kon)

# Kvinnor
# Nationella kvoter kvinnor och 1-årsintervall 
kvot <- c(0.001230012, 0.001831502, 0.022018349, 0.070128119, 0.169542386, 
          0.25317401, 0.3010279, 0.331905782, 0.368386675, 0.361578266, 
          0.386601106, 0.399878271, 0.42015855, 0.408426966, 0.412188366, 
          0.414135837, 0.418710782, 0.410532615, 0.423558897, 0.427062706, 
          0.425487465, 0.433988764, 0.434092478, 0.447459986, 0.449022346, 
          0.456866805, 0.45841785, 0.471736205, 0.457863305, 0.458169935, 
          0.4808814, 0.486679662, 0.487851662, 0.505537459, 0.509446254, 
          0.502786378, 0.51421508, 0.509886159, 0.503222027, 0.514202899, 
          0.530513595, 0.528598366, 0.534128562, 0.513092711, 0.502519798, 
          0.491768074, 0.502432245, 0.508855586, 0.518493151, 0.523351648, 
          0.521961933, 0.517952636, 0.516653757, 0.509789157, 0.521260997, 
          0.539333806, 0.547505126, 0.563113604, 0.570703408, 0.588541667, 
          0.606518283, 0.622466216, 0.643317972, 0.68213228, 0.709782609, 
          0.726290516, 0.741854637, 0.748998665, 0.77574048, 0.817619784, 
          0.856164384, 0.871939736, 0.906187625, 0.924205379, 0.95531781, 
          0.95531781, 0.95531781, 0.95531781, 0.95531781, 0.95531781, 
          0.95531781, 0.95531781, 0.95531781, 0.95531781, 0.95531781
)

kon <- c(rep("kvinnor", 85))

data_kvinnor <- data.frame(kvot = kvot, ålder = alder, kön = kon)

data_kvot <- full_join(
  data_kvot,
  data_kvinnor,
  by = NULL)

data_prog <- data_prog %>%
  left_join(data_kvot %>% 
              select(ålder, kön, kvot), by = c("ålder", "kön"))

# Hämtar bostads data
px_url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BO/BO0104/BO0104D/BO0104T04" 

## Kör denna kod för att få metadata:
px_meta <- pxweb_get(px_url)


max_ar <- hamta_giltiga_varden_fran_tabell(px_meta, "Tid" ) %>% 
  max()%>% 
  as.numeric() 

hist_tid <- c(2006:max_ar) %>% 
  as.character()


query <- list(
  Region = omrade,
    Tid = hist_tid,
    ContentsCode = "BO0104AH"
)

px_uttag <- pxweb_get(px_url, query = query) 

data_bost <- px_uttag %>% 
  as.data.frame()

data_bost <- data_bost %>% 
  rename(Bostäder = Antal)

# Kvoter för att kunna beräkna framtida hushåll
# kvot <- data.frame(
#   Alder = c(16:99),
#   kvot = c(
#     rep(0.03, 4), 
#     rep(0.41, 5),
#     rep(0.6, 10),
#     rep(0.59, 10),
#     rep(0.63, 10),
#     rep(0.64, 10),
#     rep(0.68, 5),
#     rep(0.72, 5),
#     rep(0.75, 5),
#     rep(0.87, 20)
#   )
# )
 

data_prog <- data_prog %>%
  mutate(data_prog, forva_hushall = Folkmängd * kvot)  %>%
  group_by(region, år) %>%
  summarise(forv_hushall_per_ar = sum(forva_hushall, na.rm = TRUE))

data_prog <- full_join(
  data_prog,
  data_bost,
  by = NULL)

# Beräkna över- underskott av bostäder
data_prog <- data_prog %>%
  mutate(underskott = forv_hushall_per_ar - Bostäder) # %>%
#  mutate(underskott = ifelse(underskott < 0, 0, underskott)) %>% frågan om denna ska få vara kvar. kan vara bra att veta inför andra beräkningar
#  mutate(ackumulerad_underskott = cumsum(underskott)) 

# Se över om den här fungerar så som den ska 
forandring <- data_prog %>%
  filter(år %in% c(max_ar, slut_ar)) %>%
  arrange(region, år) %>%
  group_by(region) %>%
  summarise(skillnad = diff(forv_hushall_per_ar)) %>%
  mutate(vaxer_regionen = skillnad > 0)

data_prog <- data_prog %>%
  left_join(forandring, by = "region")

data_prog <- data_prog %>%
  mutate(bostads_prognos = ifelse(vaxer_regionen & år >= max_ar & år <= slut_ar,
                                      forv_hushall_per_ar * 1.01,
                                      NA))

data_prog$år <- as.numeric(data_prog$år)

data_prog <- data_prog %>%
  mutate(forandring_bostads_prognos = bostads_prognos - lag(bostads_prognos))

total_utvalda_regioner <- data_prog %>%
  group_by(år) %>%
  summarise(
    forv_hushall_per_ar = sum(forv_hushall_per_ar, na.rm = TRUE),
    Bostäder = sum(Bostäder, na.rm = TRUE),
    underskott = sum(underskott, na.rm = TRUE),
    skillnad = sum(skillnad, na.rm = TRUE),
    bostads_prognos = sum(bostads_prognos, na.rm = TRUE)
  ) %>%
  mutate(region = "Total") %>%
  select(region, everything())%>%
  # TA BORT OCH GÖR PÅ NÅGOT SNYGGARE SÄTT. Ersätt alla 0 med NA i hela dataframen
  mutate(across(everything(), ~ ifelse(. == 0, NA, .)))


ggplot(data_prog, aes(x = år)) +
  geom_line(aes(y = Bostäder, color = "Bostäder"), linewidth = 1) +
  geom_line(aes(y = bostads_prognos, color = "Bostadsprognos"), linewidth = 1, linetype = "dashed") +
  labs(
    title = "Utveckling av bostäder och prognos över tid per region",
    x = "Tid",
    y = "Antal",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Bostäder" = "blue", "Bostadsprognos" = "red")) +
  theme_minimal() +
  facet_wrap(~ region, scales = "free_y")


ggplot(total_utvalda_regioner, aes(x = år)) +
  geom_line(aes(y = Bostäder, color = "Bostäder"), linewidth = 1) +
  geom_line(aes(y = bostads_prognos, color = "Bostadsprognos"), linewidth = 1, linetype = "dashed") +
  labs(
    title = "Utveckling av bostäder och prognos över tid",
    x = "År",
    y = "Antal",
    color = "Legend"
  ) +
  scale_color_manual(values = c("Bostäder" = "blue", "Bostadsprognos" = "red")) +
  theme_minimal()
  
message("Körningen är klar!")

