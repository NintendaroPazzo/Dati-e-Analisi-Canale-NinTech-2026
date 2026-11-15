#=============================================================================
  # Analisi storico mensile NinTech (storico_iscritti.xlsx)
  #  - Iscritti nel tempo (da aprile 2020)
  #  - Iscritti cumulati (da aprile 2020)
  #  - Entrate standardizzate (z-score) nel tempo (dal primo mese con entrate)
  # =============================================================================

pacchetti <- c("readxl", "dplyr", "ggplot2", "scales")
nuovi <- setdiff(pacchetti, rownames(installed.packages()))
if (length(nuovi) > 0) install.packages(nuovi)
invisible(lapply(pacchetti, library, character.only = TRUE))

setwd("C:/Users/Bruno/Desktop/Analisi_Ste")

# ---- 1. Importazione --------------------------------------------------------
percorso <- "C:/Users/Bruno/Desktop/Analisi_Ste/storico_iscritti.xlsx"

# "Totali": iscritti per mese, serie completa (89 mesi, da lug 2019)
totali <- read_excel(percorso, sheet = "Totali") %>%
  rename(data = Data, iscritti = Iscritti)

# "Dati della tabella": mensili con le entrate (82 mesi, non ordinati; le
# entrate sono disponibili solo da ott 2022 in poi)
tabella <- read_excel(percorso, sheet = "Dati della tabella") %>%
  select(data = Data, entrate_eur = `Entrate stimate (EUR)`)

# Unisco: parto dalla serie completa degli iscritti e aggiungo le entrate
dati <- totali %>%
  left_join(tabella, by = "data") %>%
  mutate(data = as.Date(paste0(data, "-01"))) %>%   # "2023-12" -> 2023-12-01
  arrange(data)

# Novembre 2026 è un mese ancora in corso (parziale): i suoi valori sono più
# bassi per questo motivo. Metti TRUE per escluderlo dai grafici.
escludi_ultimo_mese <- FALSE
if (escludi_ultimo_mese) dati <- dati %>% filter(data < max(data))

cat("\nPeriodo:", format(min(dati$data)), "->", format(max(dati$data)),
    "| mesi:", nrow(dati), "\n")
cat("Mesi con entrate disponibili:", sum(!is.na(dati$entrate_eur)),
    "(dal", format(min(dati$data[!is.na(dati$entrate_eur)])), ")\n")

# ---- 2. Standardizzazione ---------------------------------------------------
# z = (x - media) / deviazione standard, calcolata sui soli mesi con entrate,
# Il periodo parte dal primo mese con entrate disponibili
dati_entrate <- dati %>%
  filter(!is.na(entrate_eur)) %>%
  mutate(entrate_z = as.numeric(scale(entrate_eur)))

cat("\nEntrate: media =", round(mean(dati_entrate$entrate_eur), 2),
    "EUR, dev. standard =", round(sd(dati_entrate$entrate_eur), 2), "\n")

# ---- 3. Iscritti nel tempo (da aprile 2020) ----------------------------------
data_inizio <- as.Date("2020-04-01")
dati_iscritti <- dati %>%
  filter(data >= data_inizio) %>%
  mutate(iscritti_cum = cumsum(iscritti))   # cumulati a partire da aprile 2020

p_iscritti <- ggplot(dati_iscritti, aes(x = data, y = iscritti)) +
  geom_line(colour = "#1b9e77", linewidth = 0.8) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(title = "Iscritti per mese",
       x = "Mese", y = "Iscritti") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

x11()
print(p_iscritti)
ggsave("storico_iscritti_mensili.png", p_iscritti,
       width = 12, height = 6, dpi = 300, bg = "white")

# ---- 3b. Iscritti cumulati (da aprile 2020) ----------------------------------
p_cum <- ggplot(dati_iscritti, aes(x = data, y = iscritti_cum)) +
  geom_line(colour = "#d95f02", linewidth = 0.9) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  scale_y_continuous(labels = label_number(big.mark = ".")) +
  labs(title = "Iscritti cumulati",
       subtitle = "Somma degli iscritti ottenuti mese per mese, a partire da aprile 2020",
       x = "Mese", y = "Iscritti cumulati") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

x11()
print(p_cum)
ggsave("storico_iscritti_cumulati.png", p_cum,
       width = 12, height = 6, dpi = 300, bg = "white")

# ---- 4. Entrate standardizzate nel tempo -------------------------------------
p_entrate <- ggplot(dati_entrate, aes(x = data, y = entrate_z)) +
  geom_hline(yintercept = 0, colour = "grey60", linetype = "dashed") +
  geom_line(colour = "#7570b3", linewidth = 0.8) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y") +
  labs(title = "Entrate stimate standardizzate",
       #subtitle = "z-score: 0 = entrata media mensile; +1 = una deviazione standard sopra la media",
       x = "Mese", y = "Entrate (z-score)") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

x11()
print(p_entrate)
ggsave("storico_entrate_standardizzate.png", p_entrate,
       width = 12, height = 6, dpi = 300, bg = "white")

