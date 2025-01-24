Prognosmodell för bostadsmarknaden
Detta projekt implementerar en modell för att analysera och prognostisera bostadsmarknaden i olika svenska regioner, inspirerad av Boverkets metodik. Koden hämtar demografiska och bostadsrelaterad data från 
SCB (Statistiska centralbyrån), beräknar antalet framtida hushåll och visualiserar resultaten i form av grafer.

Förutsättningar
För att kunna köra denna kod behöver du:

R installerat på din dator.
Följande R-bibliotek installerade:
httr
jsonlite
tidyverse
ggplot2

1. Hämta data från SCB
Koden hämtar data för:

Befolkningsframskrivningar (framtida år).
Demografiska data (historiska år).
Befintligt bostadsbestånd.
Anpassade förfrågningar skickas till SCB:s API som baseras på specifika regionkoder och tidsintervall.

2. Parametrar för körning
Koden kan anpassas med följande inställningar:

stat_ar: Det senaste året med statistik (exempel: 2023).
slut_ar: Det sista året för prognosen (exempel: 2033).
omrade: Lista med region- eller kommunkoder (exempel: c("2026", "2029")).
alder: Åldersintervall för personer som inkluderas i beräkningarna (16 år och äldre enligt Boverkets modell).
3. Beräkningar
Förväntat antal hushåll baseras på demografiska data och hushållskvoter.
Underskott/överskott av bostäder beräknas genom att jämföra förväntade antalet hushåll med befintliga bostäder.
4. Visualiseringar
Koden genererar två typer av grafer:

Utveckling av bostadsbestånd och prognoser per region.
Sammanlagd utveckling för alla utvalda regioner.
Så här använder du koden
Anpassa parametrarna
Justera variabler som stat_ar, slut_ar, omrade och alder i början av skriptet för att matcha dina behov.

Kör koden
Exekvera skriptet i R eller RStudio. Skriptet kommer att:

Hämta data från SCB.
Utföra nödvändiga beräkningar.
Generera visualiseringar.
Tolka resultaten

Graferna visar bostadsbestånd, prognostiserade hushållsbehov och underskott över tid.
Data kan användas för att planera framtida bostadsbyggande i olika regioner.
Viktiga anteckningar
Data från SCB innehåller åldersgruppen 100+, men dessa exkluderas automatiskt i vissa beräkningar. Detta kommer i en uppdaterad version att inkluderas i beräkningarna.
Prognosen tar hänsyn till om en region förväntas växa. Om regionen inte växer, görs ingen prognos för framtida hushållsbehov.
Exempel på grafer
Utveckling av bostäder och prognos per region:
Visar skillnader mellan faktiska och prognostiserade behov för varje region.

Sammanlagd utveckling för utvalda regioner:
Summerade data för alla regioner, vilket ger en översiktlig bild.


Möjlig utvecklingspotential
Beräkna och visualisera förväntat tillskottsbehov av bostäder per år baserat på ingående över-/underskott i separat tabell och diagram.
Ytterligare alternativ vid beräkning av kvoter.
Möjlighet att välja ett redan definierat FA-område istället för att skriva in regionkoder.
