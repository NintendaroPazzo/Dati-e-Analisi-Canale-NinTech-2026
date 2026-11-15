# =============================================================================
# Analisi dati YouTube - NinTech (Dati_canale.xlsx fuso con il file etichettato)
# Parte 1: Word Cloud dal "Titolo video"
# Parte 2: Scatterplot degli iscritti (scala log-log)
# Parte 3: Matrice di correlazione con tutte le variabili numeriche
# Parte 4: Boxplot degli iscritti per categoria
# =============================================================================

pacchetti <- c("readxl", "dplyr", "stringr", "tidytext", "stopwords",
               "wordcloud", "RColorBrewer", "ggplot2", "scales", "writexl")
nuovi <- setdiff(pacchetti, rownames(installed.packages()))
if (length(nuovi) > 0) install.packages(nuovi)
invisible(lapply(pacchetti, library, character.only = TRUE))

setwd("")

# ---- 1. Importazione e fusione dei dati -------------------------------------
# Modifica i percorsi se i file si trovano altrove
percorso_nuovo   <- ".../Dati_canale.xlsx"
percorso_vecchio <- ".../Analitici_Intera_durata_NinTech_Etichettati.xlsx"

# Dataset aggiornato (386 video). Il file ha 3 fogli: quello con un video per
# riga è "Dati della tabella"
nuovo <- read_excel(percorso_nuovo, sheet = "Dati della tabella") %>%
  rename(
    id_video        = Contenuti,
    titolo          = `Titolo video`,
    data_pubbl      = `Ora pubblicazione video`,
    durata_sec      = Durata,
    commenti        = `Commenti aggiunti`,
    like            = `Mi piace`,
    dislike         = `N. di Non mi piace`,
    visualizzazioni = Visualizzazioni,
    ore_visione     = `Tempo di visualizzazione (ore)`,
    iscritti        = Iscritti,
    entrate_eur     = `Entrate stimate (EUR)`,
    impressioni     = `Impressioni miniatura`,
    ctr             = `Percentuale di clic sulla miniatura (%)`
  )

# Dataset etichettato (376 video): da qui servono solo la categoria (ultima
# colonna, senza nome nel file) e la durata di visione media
vecchio <- read_excel(percorso_vecchio)
names(vecchio)[ncol(vecchio)] <- "Categoria"
vecchio <- vecchio %>%
  select(id_video = Contenuti,
         durata_media = `Durata di visualizzazione media`,
         Categoria)

# Fusione: tengo solo i video presenti nel dataset da 376 (inner join sull'id).
# Le metriche (visualizzazioni, iscritti, ...) sono quelle aggiornate del
# dataset nuovo; i 10 video in più del nuovo file (post, teaser, video non
# del canale) vengono esclusi.
dati <- inner_join(nuovo, vecchio, by = "id_video")

cat("\nVideo nel dataset nuovo:   ", nrow(nuovo),
    "\nVideo nel dataset vecchio: ", nrow(vecchio),
    "\nVideo nel dataset fuso:    ", nrow(dati), "\n")
stopifnot(nrow(dati) == nrow(vecchio))   # tutti gli id del vecchio ritrovati

# "Durata di visualizzazione media" è testo (h:mm:ss): la converto in secondi
dati <- dati %>%
  mutate(durata_media_sec = sapply(strsplit(as.character(durata_media), ":"),
                                   function(p) sum(as.numeric(p) * c(3600, 60, 1)[seq_along(p) + (3 - length(p))])))

# Salvo il dataset fuso
write_xlsx(dati, "Dati_canale_fusi.xlsx")

glimpse(dati)


# ---- 2. Pulizia dei titoli --------------------------------------------------
# Parole da escludere oltre alle stopword italiane/inglesi
# (aggiungi qui altre parole "rumorose" dopo aver visto il risultato)
stop_custom <- c("ita", "vs", "video", "nel", "con", "ho", "ep", "ep1",
                 "ep2", "ep3", "ep4", "ep5", "oggi", "come", "più", "piu",
                 "cosa", "fa", "può", "puo", "è", "e'", "sono", "pc")
# NB: "pc" è stata messa tra le esclusioni perché dominerebbe la nuvola;
#     rimuovila da stop_custom se vuoi vederla.

stop_words <- unique(c(
  stopwords("it"),
  stopwords("en"),
  stop_custom
))

# La word cloud esclude i video della categoria "Gameplay/Walkthrough"
# (vale solo per questa sezione: gli altri grafici usano tutti i video)
dati_cloud <- dati %>%
  filter(!str_detect(Categoria, regex("gameplay", ignore_case = TRUE)) | is.na(Categoria))
cat("\nVideo usati per la word cloud:", nrow(dati_cloud), "su", nrow(dati),
    "(esclusi", nrow(dati) - nrow(dati_cloud), "video Gameplay)\n")

parole <- dati_cloud %>%
  select(id_video, titolo) %>%
  mutate(
    titolo = str_to_lower(titolo),
    titolo = str_replace_all(titolo, "https?://\\S+", " "),   # URL
    titolo = str_replace_all(titolo, "#", " "),                # hashtag -> parola
    titolo = str_replace_all(titolo, "[[:punct:]]+", " "),     # punteggiatura
    titolo = str_replace_all(titolo, "[0-9]+", " "),           # numeri
    titolo = str_squish(titolo)
  ) %>%
  unnest_tokens(parola, titolo, token = "words") %>%
  filter(
    !parola %in% stop_words,
    nchar(parola) > 2
  )

# ---- 3. Frequenze -----------------------------------------------------------
freq <- parole %>%
  count(parola, sort = TRUE)

cat("\nParole uniche:", nrow(freq), "\n")
cat("\nTop 30 parole:\n")
print(head(freq, 30))

# ---- 4. Word Cloud ----------------------------------------------------------
set.seed(123)  # riproducibilità della disposizione

disegna_cloud <- function() {
  wordcloud(
    words        = freq$parola,
    freq         = freq$n,
    min.freq     = 3,            # scarta parole troppo rare
    max.words    = 150,
    random.order = FALSE,
    rot.per      = 0.25,
    scale        = c(5, 0.7),
    colors       = brewer.pal(8, "Dark2")
  )
}

# Visualizzazione a schermo
x11()
disegna_cloud()

# Salvataggio in PNG ad alta risoluzione
png("wordcloud_titoli_video.png", width = 2400, height = 1800, res = 300)
disegna_cloud()
dev.off()

cat("\nWord cloud salvata in: wordcloud_titoli_video.png\n")

# =============================================================================
# Parte 2: Scatterplot degli iscritti (scala log-log)
# =============================================================================
# Funzione che disegna iscritti (asse y) contro una variabile x, entrambe in
# scala logaritmica. Poiché commenti, like e dislike contengono molti zeri, si
# usa log(valore + 0.5). Gli iscritti negativi (video che hanno fatto perdere
# iscritti, minimo = -1) sono portati a 0 per poter calcolare il logaritmo.
# I punti sono leggermente "sparpagliati" (jitter) perché i conteggi bassi
# altrimenti si sovrappongono sugli stessi valori.
scatter_iscritti <- function(dati, var_x, etichetta_x, nome_file, offset_x = 0.5) {
  d <- dati %>%
    transmute(x = log(.data[[var_x]] + offset_x),
              y = log(pmax(iscritti, 0) + 0.5)) %>%
    filter(is.finite(x), is.finite(y))

  # Correlazione di Pearson sui valori trasformati (quelli che si vedono nel grafico)
  r <- cor(d$x, d$y)

  p <- ggplot(d, aes(x = x, y = y)) +
    geom_point(colour = "#1b9e77", alpha = 0.6, size = 2,
               position = position_jitter(width = 0.05, height = 0.05, seed = 1)) +
    geom_smooth(method = "lm", se = TRUE,
                colour = "#d95f02", fill = "#d95f02", alpha = 0.15) +
    labs(title = paste("Iscritti vs", tolower(etichetta_x)),
         subtitle = sprintf("Un punto per video  |  correlazione di Pearson (sui log) r = %.2f", r),
         x = paste0("log(", etichetta_x, ")"), y = "log(Iscritti)") +
    theme_minimal(base_size = 12)

  x11()
  print(p)
  ggsave(nome_file, p, width = 8, height = 6, dpi = 300, bg = "white")
  invisible(p)
}

# Le ore di visione sono sempre > 0: log() semplice, come nello script precedente
p_ore     <- scatter_iscritti(dati, "ore_visione", "Tempo di visualizzazione (ore)",
                              "scatter_iscritti_ore_visione.png", offset_x = 0)
p_commenti <- scatter_iscritti(dati, "commenti", "Commenti",
                               "scatter_iscritti_commenti.png")
p_like     <- scatter_iscritti(dati, "like", "Mi piace",
                               "scatter_iscritti_like.png")
p_dislike  <- scatter_iscritti(dati, "dislike", "Non mi piace",
                               "scatter_iscritti_dislike.png")


# =============================================================================
# Parte 3: Matrice di correlazione con tutte le variabili numeriche
# =============================================================================
etichette <- c(durata_sec = "Durata video", durata_media_sec = "Durata media vis.",
               commenti = "Commenti",
               like = "Mi piace", dislike = "Non mi piace",
               visualizzazioni = "Visualizzazioni", ore_visione = "Ore visione",
               iscritti = "Iscritti", entrate_eur = "Entrate (EUR)",
               impressioni = "Impressioni", ctr = "CTR miniatura")

vars <- dati %>% select(all_of(names(etichette)))
colnames(vars) <- etichette

# Metodo: "pearson" (lineare) oppure "spearman" (a ranghi, meno sensibile ai
# video con valori molto alti)
metodo_cor <- "pearson"

# Alcune entrate e alcuni CTR sono mancanti: uso le coppie di dati complete
mat_cor <- cor(vars, use = "pairwise.complete.obs", method = metodo_cor)
print(round(mat_cor, 2))

cor_long <- as.data.frame(as.table(mat_cor))
names(cor_long) <- c("var1", "var2", "r")

p_cor <- ggplot(cor_long, aes(x = var1, y = var2, fill = r)) +
  geom_tile(colour = "white") +
  geom_text(aes(label = sprintf("%.2f", r)), size = 3) +
  scale_fill_gradient2(low = "#2166ac", mid = "white", high = "#b2182b",
                       midpoint = 0, limits = c(-1, 1), name = "r") +
  scale_y_discrete(limits = rev) +
  coord_fixed() +
  labs(title = paste0("Matrice di correlazione (", metodo_cor, ")"),
       x = NULL, y = NULL) +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

x11()
print(p_cor)
ggsave("matrice_correlazioni.png", p_cor,
       width = 9, height = 8, dpi = 300, bg = "white")


# =============================================================================
# Parte 4: Boxplot degli iscritti per categoria
# =============================================================================
# Categorie ordinate per mediana degli iscritti; tra parentesi il numero di video
dati_box <- dati %>%
  group_by(Categoria) %>%
  mutate(cat_label = sprintf("%s (n=%d)", Categoria, n())) %>%
  ungroup() %>%
  mutate(cat_label = reorder(cat_label, iscritti, FUN = median))

p_box <- ggplot(dati_box, aes(x = cat_label, y = iscritti, fill = cat_label)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(width = 0.15, alpha = 0.35, size = 1.2, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  labs(title = "Iscritti per categoria di video",
       subtitle = "Categorie ordinate per mediana; i punti sono i singoli video",
       x = NULL, y = "Iscritti") +
  theme_minimal(base_size = 12)

x11()
print(p_box)
ggsave("boxplot_iscritti_categoria.png", p_box,
       width = 10, height = 6, dpi = 300, bg = "white")

# Riepilogo numerico per categoria
riepilogo <- dati %>%
  group_by(Categoria) %>%
  summarise(n = n(), media = mean(iscritti), mediana = median(iscritti),
            sd = sd(iscritti), max = max(iscritti), .groups = "drop") %>%
  arrange(desc(mediana))
print(riepilogo)
# ---- Boxplot per categoria, solo video con durata >= 180 secondi -------------
soglia_sec <- 180   # durata in secondi (3 minuti)

dati_box180 <- dati %>%
  filter(durata_sec >= soglia_sec) %>%
  group_by(Categoria) %>%
  mutate(cat_label = sprintf("%s (n=%d)", Categoria, n())) %>%
  ungroup() %>%
  mutate(cat_label = reorder(cat_label, iscritti, FUN = median))

cat("\nVideo con durata >=", soglia_sec, "s:", nrow(dati_box180),
    "| esclusi:", nrow(dati) - nrow(dati_box180), "\n")

p_box180 <- ggplot(dati_box180, aes(x = cat_label, y = iscritti, fill = cat_label)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(width = 0.15, alpha = 0.35, size = 1.2, show.legend = FALSE) +
  scale_fill_brewer(palette = "Set3") +
  coord_flip() +
  labs(title = "Iscritti per categoria di video",
       subtitle = paste0("Solo video con durata >= ", soglia_sec,
                         " secondi; categorie ordinate per mediana"),
       x = NULL, y = "Iscritti") +
  theme_minimal(base_size = 12)

x11()
print(p_box180)
ggsave("boxplot_iscritti_categoria_durata180.png", p_box180,
       width = 10, height = 6, dpi = 300, bg = "white")



# ---- Boxplot: video brevi (< 180 s) vs lunghi (>= 180 s) ---------------------
dati_durata <- dati %>%
  mutate(gruppo_durata = factor(ifelse(durata_sec < soglia_sec,
                                       paste0("Durata < ", soglia_sec, " s"),
                                       paste0("Durata >= ", soglia_sec, " s")),
                                levels = c(paste0("Durata < ", soglia_sec, " s"),
                                           paste0("Durata >= ", soglia_sec, " s")))) %>%
  group_by(gruppo_durata) %>%
  mutate(gruppo_label = sprintf("%s\n(n=%d)", gruppo_durata, n())) %>%
  ungroup() %>%
  mutate(gruppo_label = factor(gruppo_label, levels = unique(gruppo_label[order(gruppo_durata)])))

p_box_durata <- ggplot(dati_durata, aes(x = gruppo_label, y = iscritti, fill = gruppo_label)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA, show.legend = FALSE) +
  geom_jitter(width = 0.15, alpha = 0.35, size = 1.5, show.legend = FALSE) +
  scale_fill_manual(values = c("#d95f02", "#1b9e77")) +
  # Scala pseudo-logaritmica: si comporta come un log per valori grandi ma è
  # definita anche per 0 e per i valori negativi (qui il minimo è -1).
  # Gli assi mostrano i valori originali degli iscritti.
  scale_y_continuous(trans = pseudo_log_trans(sigma = 1, base = 10),
                     breaks = c(0, 1, 2, 5, 10, 20, 50, 100, 200)) +
  labs(title = "Iscritti: video brevi vs video lunghi (scala pseudo-log)",
       subtitle = "I punti sono i singoli video",
       x = NULL, y = "Iscritti (scala pseudo-log)") +
  theme_minimal(base_size = 12)

x11()
print(p_box_durata)
ggsave("boxplot_iscritti_durata180.png", p_box_durata,
       width = 7, height = 6, dpi = 300, bg = "white")

# Riepilogo numerico per gruppo di durata
riepilogo_durata <- dati_durata %>%
  group_by(gruppo_durata) %>%
  summarise(n = n(), media = mean(iscritti), mediana = median(iscritti),
            sd = sd(iscritti), max = max(iscritti), .groups = "drop")
print(riepilogo_durata)


# =============================================================================
# Parte 6: Boxplot per categoria con misure "normalizzate" (solo video >= 180 s)
# =============================================================================
# I video più vecchi hanno avuto più tempo per portare iscritti: invece degli
# iscritti grezzi uso (a) iscritti per tempo online e (b) iscritti per
# visualizzazione.

# ---- Data di pubblicazione e giorni online -----------------------------------
# Le date sono testo in inglese (es. "Apr 10, 2022"): converto i mesi a mano
# per non dipendere dal locale italiano del computer.
data_estrazione <- as.Date("2026-11-11")   # giorno in cui è stato estratto il dataset
mesi_en <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")

converti_data <- function(x) {
  if (inherits(x, "Date"))   return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  m <- str_match(as.character(x), "([A-Za-z]{3})\\w*\\s+(\\d{1,2}),?\\s*(\\d{4})")
  mese   <- match(str_to_title(m[, 2]), mesi_en)
  giorno <- as.integer(m[, 3])
  anno   <- as.integer(m[, 4])
  out <- as.Date(rep(NA_character_, length(x)))
  ok  <- !is.na(mese) & !is.na(giorno) & !is.na(anno)
  out[ok] <- as.Date(sprintf("%04d-%02d-%02d", anno[ok], mese[ok], giorno[ok]))
  out
}

dati_norm <- dati %>%
  filter(durata_sec >= soglia_sec) %>%
  mutate(
    data_pubbl_d   = converti_data(data_pubbl),
    giorni_online  = as.numeric(data_estrazione - data_pubbl_d),
    # iscritti ottenuti in media ogni 30 giorni di permanenza online
    iscritti_30gg  = iscritti / giorni_online * 30,
    # iscritti ottenuti ogni 1000 visualizzazioni
    iscritti_1000v = iscritti / visualizzazioni * 1000
  )

if (any(is.na(dati_norm$giorni_online)) || any(dati_norm$giorni_online <= 0)) {
  stop("Ci sono video con data di pubblicazione mancante o successiva all'estrazione")
}
cat("\nVideo usati (durata >=", soglia_sec, "s):", nrow(dati_norm),
    "| giorni online: min", min(dati_norm$giorni_online),
    "- max", max(dati_norm$giorni_online), "\n")

# Funzione: boxplot per categoria di una misura qualsiasi
boxplot_categoria <- function(d, var_y, titolo, etichetta_y, nome_file) {
  d <- d %>%
    group_by(Categoria) %>%
    mutate(cat_label = sprintf("%s (n=%d)", Categoria, n())) %>%
    ungroup() %>%
    mutate(cat_label = reorder(cat_label, .data[[var_y]], FUN = median))
  
  p <- ggplot(d, aes(x = cat_label, y = .data[[var_y]], fill = cat_label)) +
    geom_boxplot(alpha = 0.6, outlier.shape = NA, show.legend = FALSE) +
    geom_jitter(width = 0.15, alpha = 0.35, size = 1.2, show.legend = FALSE) +
    scale_fill_brewer(palette = "Set3") +
    coord_flip() +
    labs(title = titolo,
         subtitle = paste0("Solo video con durata >= ", soglia_sec,
                           " secondi; categorie ordinate per mediana"),
         x = NULL, y = etichetta_y) +
    theme_minimal(base_size = 12)
  
  x11()
  print(p)
  ggsave(nome_file, p, width = 10, height = 6, dpi = 300, bg = "white")
  invisible(p)
}

# ---- (a) Iscritti per tempo online -------------------------------------------
p_box_tempo <- boxplot_categoria(
  dati_norm, "iscritti_30gg",
  "Iscritti per tempo online, per categoria",
  "Iscritti ogni 30 giorni online",
  "boxplot_iscritti_per_tempo_categoria.png")

# ---- (b) Iscritti per visualizzazione ----------------------------------------
p_box_views <- boxplot_categoria(
  dati_norm, "iscritti_1000v",
  "Iscritti per visualizzazione, per categoria",
  "Iscritti ogni 1000 visualizzazioni",
  "boxplot_iscritti_per_visualizzazioni_categoria.png")

# Se pochi video molto alti schiacciano le scatole, si può usare la scala
# pseudo-logaritmica (come nel boxplot brevi/lunghi), ad esempio:
# x11(); print(p_box_tempo + scale_y_continuous(trans = pseudo_log_trans(sigma = 0.1, base = 10),
#                                               breaks = c(0, 0.1, 0.5, 1, 2, 5, 10, 30)))

# Riepilogo numerico delle due misure per categoria
riepilogo_norm <- dati_norm %>%
  group_by(Categoria) %>%
  summarise(n = n(),
            mediana_30gg  = median(iscritti_30gg),
            media_30gg    = mean(iscritti_30gg),
            mediana_1000v = median(iscritti_1000v),
            media_1000v   = mean(iscritti_1000v),
            .groups = "drop") %>%
  arrange(desc(mediana_1000v))
print(riepilogo_norm)


