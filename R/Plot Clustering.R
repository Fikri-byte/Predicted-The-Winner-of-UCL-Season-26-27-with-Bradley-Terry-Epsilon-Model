library(tidyverse)
library(readxl)
library(ggimage)

df <- read_excel("Prediksi Simulasi UCL.xlsx") %>%
  select(Nomor, Tim, Poin, Peluang) %>%
  filter(!is.na(Tim), !is.na(Peluang), Peluang > 0) %>%
  mutate(log_peluang = log10(Peluang))

logo_tim <- read_excel("C:/Users/Asus/OneDrive/Documents/Kuliah Statistika Unpad/My Lomba/Mention/logo_tim.xlsx") %>%
  rename(Tim = `Nama Tim`, Logo = Link)
peta <- c(
  "Bayern"="FC Bayern München",
  "PSG"="Paris Saint-Germain",
  "Barcelona"="FC Barcelona",
  "Bodo/Glimt"="Bodø/Glimt",
  "PSV"="PSV Eindhoven",
  "Sporting"="Sporting CP",
  "Atletico Madrid" ="Atlético Madrid",
  "Dortmund" =  "Borussia Dortmund",
  "Fenerbahce"="Fenerbahçe",
  "Lille" ="LOSC Lille"
)
samakan <- function(x) { i <- x %in% names(peta); x[i] <- peta[x[i]]; x }
df$Tim     <- samakan(df$Tim)
logo_tim$Tim <- samakan(logo_tim$Tim)
df <- df %>% left_join(logo_tim, by = "Tim")

library(tidyverse)
library(ggimage)
library(ggrepel)
df <- df %>%
  mutate(
    grup = case_when(
      Nomor <= 12 ~ "1 – 12",     # rank 1 s/d 12
      Nomor <= 24 ~ "13 – 24",    # rank 13 s/d 24
      TRUE        ~ "25 – 36"     # rank 25 s/d 36
    ),
    grup = factor(grup, levels = c("1 – 12", "13 – 24", "25 – 36")),
    baris = ave(Nomor, grup, FUN = function(x) rank(x))
  )
df_plot <- df %>% filter(!is.na(Logo))   # tanpa filter outlier



df_hull <- df_plot %>%
  group_by(grup) %>%
  slice(chull(Poin, log_peluang)) %>%
  ungroup()

library(ggplot2)
library(ggimage)
library(ggrepel)
library(ggpubr)   # untuk background_image()
library(png)      # pakai jpeg::readJPEG() kalau file-nya .jpg
library(jpeg)

# 1. Baca gambar latar UCL (bintang UCL / pola biru tua, simpan di folder kerja)
bg_ucl <- jpeg::readJPEG("C:/Users/Asus/OneDrive/Documents/Kuliah Statistika Unpad/My Lomba/Mention/Background1.jpg")

# Fungsi untuk mengatur transparansi gambar (0 = hilang, 1 = penuh)
set_alpha <- function(img, alpha = 0.3) {
  if (dim(img)[3] == 3) {
    # Tambah channel alpha jika belum ada
    img <- abind::abind(img, array(alpha, dim = dim(img)[1:2]), along = 3)
  } else {
    img[, , 4] <- img[, , 4] * alpha
  }
  img
}

bg_ucl <- set_alpha(bg_ucl, alpha = 0.3)   # coba 0.2 – 0.5

# 2. Warna blok dibuat lebih terang agar kontras dengan latar gelap
warna_grup <- c("1 – 12"  = "#4ADE80",
                "13 – 24" = "#FBBF24",
                "25 – 36" = "#F87171")
# sesuaikan nama elemen dengan level `grup` di datamu

p <- ggplot(df_plot, aes(x = Poin, y = log_peluang)) +
  background_image(bg_ucl) +                      # harus layer pertama
  geom_polygon(data = df_hull,
               aes(x = Poin, y = log_peluang, fill = grup, group = grup),
               alpha = 0.18, color = NA) +
  geom_path(data = df_hull,
            aes(x = Poin, y = log_peluang, color = grup, group = grup),
            linewidth = 0.8, linetype = "dashed", alpha = 0.9) +
  geom_image(aes(image = Logo), size = 0.032, asp = 1.3) +
  geom_text_repel(
    aes(label = Tim, color = grup),
    size = 2.5, fontface = "bold",
    box.padding = 0.5, point.padding = 0.3,
    segment.size = 0.25, segment.color = "grey80",
    bg.color = "#0a1e50", bg.r = 0.12,            # outline tipis agar label jelas
    max.overlaps = 40, seed = 123,
    show.legend = FALSE                            # menghilangkan huruf "a" di legenda
  ) +
  scale_color_manual(values = warna_grup, name = "Blok Rank") +
  scale_fill_manual(values  = warna_grup, guide = "none") +
  scale_x_continuous(expand = expansion(mult = 0.12)) +
  scale_y_continuous(expand = expansion(mult = 0.12)) +
  labs(
    title = "Poin Klasemen vs Peluang Juara",
    subtitle = "Area berwarna = batas wilayah tiap blok rank",
    x = "Poin", y = "log10(Peluang Juara)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title      = element_text(face = "bold", color = "white"),
    plot.subtitle   = element_text(color = "grey85"),
    axis.title      = element_text(color = "white"),
    axis.text       = element_text(color = "grey90"),
    legend.title    = element_text(color = "white"),
    legend.text     = element_text(color = "white"),
    panel.grid.major = element_line(color = scales::alpha("white", 0.12)),
    panel.grid.minor = element_blank(),
    plot.background  = element_rect(fill = "#0a1e50", color = NA),
    panel.background = element_rect(fill = NA, color = NA)
  )
p
ggsave("poin_vs_peluang_ucl.png", p, width = 10, height = 7, dpi = 300)
