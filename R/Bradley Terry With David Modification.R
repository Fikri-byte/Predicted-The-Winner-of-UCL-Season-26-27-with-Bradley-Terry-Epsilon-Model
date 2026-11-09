library(dplyr)
df <- read.csv("Kuliah Statistika Unpad/My Lomba/Mention/Before Hope.csv")
df <- df[, c("Team", "Comp", "Venue", "GF", "GA", "Opponent")]
nrow(df)
unique(df$Team)

# Nambah Pertandingan LASK musim 26/27
nambah <- data.frame(
  Team = c("Lask", "Lask"),
  Comp = c("Europa Lg", "Europa Lg"),
  Venue = c("Home", "Away"),
  GF = c(1, 0),
  GA = c(3, 4),
  Opponent = c("Liverpool", "Liverpool")
)
nambah
df <- rbind(nambah,df)
nrow(df)
unique(df$Comp)
unique(df$Team)

# ---- 1. Samakan nama tim (kolom Team dan Opponent memakai ejaan berbeda) ----
peta <- c(
  "Atletico Madrid"   = "Atl\u00e9tico Madrid",
  "Bod\u00f8 Glimt"   = "Bod\u00f8/Glimt",
  "Borussia Dortmund" = "Dortmund",
  "Fenerbahce"        = "Fenerbah\u00e7e",
  "Inter Milan"       = "Inter",
  "Lask"              = "LASK",
  "Machester United"  = "Manchester Utd",
  "PSG"               = "Paris SG",
  "Paris Saint-Germain" = "Paris SG",
  "RC Lens"           = "Lens",
  "Sabah"             = "Sabah FK",
  "Shakhtar Donetskk" = "Shakhtar D.",
  "Slovan"            = "Slovan Bratislava",
  "Sporting"          = "Sporting CP",
  "VfB Stuttgrat"     = "Stuttgart",
  "Viking FK"         = "Viking",
  "Villareal"         = "Villarreal",
  "AS Roma"           = "Roma",
  "Bod\xf8/Glimt"  = "Bod\u00f8/Glimt",
  "Atl\xe9tico Madrid" = "Atlético Madrid",
  "FC Shakhtar Donetsk"= "Shakhtar D.",
  "FC Porto" = "Porto",
  "Manchester United"="Manchester Utd",
  "Como 1907" = "Como",
  "FC Barcelona" = "Barcelona",
  "FC Bayern M\xfcnchen" = "Bayern Munich",
  "LOSC Lille" = "Lille",
  "Fenerbah\xe7e" = "Fenerbah\u00e7e",
  "PSV Eindhoven" = "PSV",
  "VfB Stuttgart" = "Stuttgart",
  "SK Slavia Praha" = "Slavia Prague",
  "SSC Napoli"   = "Napoli"
  
)
samakan <- function(x) { i <- x %in% names(peta); x[i] <- peta[x[i]]; x }
df$Team     <- samakan(df$Team)
df$Opponent <- samakan(df$Opponent)
sum(df$Team=="Bodø/Glimt")
unique(df$Team)
unique(df$Opponent)
setdiff(unique(df$Team), unique(df$Opponent))   # harus character(0)
setdiff(unique(df$Opponent), unique(df$Team))   # harus character(0)
# ---- 2. Bersihkan skor: "1 (4)" -> 1 (skor adu penalti dibuang) ----
skor <- function(x) as.integer(sub("\\s*\\(.*\\)", "", as.character(x)))
df$GF <- skor(df$GF); df$GA <- skor(df$GA)

# perbaiki encoding nama kompetisi yang rusak (TrophÃ©e -> Trophée)
df$Comp <- sub("^Troph.*", "Trophee des Champions", df$Comp)

# ---- 3. Buang laga di tempat netral (tidak punya tuan rumah) ----
cat("Laga netral dibuang:", sum(df$Venue == "Neutral"), "baris\n")
df <- df[df$Venue %in% c("Home", "Away"), ]

# ---- 4. Ubah ke format home / away ----
# Satu laga bisa muncul dua kali (dari sisi kedua tim) bila kedua tim ada di kolom Team.
h <- df$Venue == "Home"
laga <- data.frame(
  home = ifelse(h, df$Team, df$Opponent),
  away = ifelse(h, df$Opponent, df$Team),
  comp = df$Comp,
  gh   = ifelse(h, df$GF, df$GA),
  ga   = ifelse(h, df$GA, df$GF),
  sisi = ifelse(h, "H", "A"),        # dicatat dari sisi tuan rumah (H) atau tamu (A)
  stringsAsFactors = FALSE
)
laga$key <- paste(laga$home, laga$away, laga$comp, laga$gh, laga$ga, sep = "|")

# ---- 5. Hilangkan duplikasi ----
# Untuk tiap laga unik: jumlah sebenarnya = max(jumlah dari sisi H, jumlah dari sisi A).
# (Aman jika laga yang sama, skor sama, memang terjadi dua kali.)
nH <- table(factor(laga$key[laga$sisi == "H"], levels = unique(laga$key)))
nA <- table(factor(laga$key[laga$sisi == "A"], levels = unique(laga$key)))
n_asli <- pmax(as.integer(nH), as.integer(nA))
names(n_asli) <- names(nH)

unik <- laga[!duplicated(laga$key), c("home", "away", "comp", "gh", "ga", "key")]
unik <- unik[rep(seq_len(nrow(unik)), n_asli[unik$key]), ]
unik$key <- NULL
unik$hasil <- ifelse(unik$gh > unik$ga, "W", ifelse(unik$gh < unik$ga, "L", "T"))

library(dplyr)
unik <- unik %>% distinct()
unik

cat("Laga (tuan rumah vs tamu) setelah dibersihkan:", nrow(unik), "\n")
unique(unik$home)
unique(unik$away)
unik

# ---- 6. Bentuk matriks W, L, T (m xm) ----
tim <- sort(unique(c(unik$home, unik$away)))
unik$home <- factor(unik$home, levels = tim)
unik$away <- factor(unik$away, levels = tim)

buat_mat <- function(kode) {
  s <- unik[unik$hasil == kode, ]
  unclass(table(s$home, s$away, dnn = NULL))
}
W <- buat_mat("W")   # tuan rumah menang
L <- buat_mat("L")   # tamu menang
T <- buat_mat("T")   # seri
dimnames(W) <- dimnames(L) <- dimnames(T) <- list(tim, tim)

# ---- 7. Pemeriksaan ----
stopifnot(all(dim(W) == dim(L)), all(dim(W) == dim(T)), all(diag(W + L + T) == 0))
N <- W + L + T

sum(W)

cat("Jumlah tim:", length(tim), "| Total laga:", sum(N),
    "| W:", sum(W), "L:", sum(L), "T:", sum(T), "\n")
cat("Tim tanpa laga:", sum(rowSums(N) + colSums(N) == 0), "\n")

# Cek kondisi koneksi sederhana: tim yang tidak pernah menang / tidak pernah kalah
menang <- rowSums(W) + colSums(L)
kalah  <- rowSums(L) + colSums(W)
cat("Tim tanpa kemenangan:", paste(tim[menang == 0], collapse = ", "), "\n")
cat("Tim tanpa kekalahan :", paste(tim[kalah  == 0], collapse = ", "), "\n")

# ---- 9. Penyesuaian epsilon (strong connection gagal: ada tim tanpa kemenangan) ----
eps <- sqrt(log(36)/36);eps
W_adj <- W; L_adj <- L
W_adj[N > 0] <- W_adj[N > 0] + eps
L_adj[N > 0] <- L_adj[N > 0] + eps

# ==========================================================================
# Model David-epsilon: memaksimumkan penalized likelihood L5 (persamaan 7)
#
#   L5 = prod_{i<j} [ (g u_i)^{a~_ij.i} u_j^{a~_ji.i} (th sqrt(u_i u_j))^{t_ij.i}
#                       / (g u_i + u_j + th sqrt(u_i u_j))^{n~_ij.i} ]
#                   [ u_i^{a~_ij.j} (g u_j)^{a~_ji.j} (th sqrt(u_i u_j))^{t_ij.j}
#                       / (u_i + g u_j + th sqrt(u_i u_j))^{n~_ij.j} ]
#
#   a~ = a + eps * I(n > 0),   n~ = a~_ij + a~_ji + t
#
# Input (baris = tuan rumah, kolom = tamu), sama seperti sebelumnya:
#   W[i,j] = a_ij.i : kemenangan tuan rumah i atas tamu j
#   L[i,j] = a_ji.i : kemenangan tamu j atas tuan rumah i
#   T[i,j] = t_ij.i : seri
# Batasan identifikasi (sesuai paper): sum(u) = 1
# ==========================================================================

# Pembentukan a~ dan n~ dari data mentah
tambah_eps <- function(W, L, T, eps) {
  N0 <- W + L + T                          # n_ij.i (tanpa epsilon)
  Wt <- W + eps * (N0 > 0)                 # a~_ij.i
  Lt <- L + eps * (N0 > 0)                 # a~_ji.i
  list(Wt = Wt, Lt = Lt, T = T, Nt = Wt + Lt + T)   # n~_ij.i
}

# Log L5 ditulis persis mengikuti persamaan (7): produk i<j dengan dua faktor
loglik_L5 <- function(u, g, th, Wt, Lt, T, Nt) {
  m <- length(u); ll <- 0
  for (i in 1:(m - 1)) for (j in (i + 1):m) {
    s <- sqrt(u[i] * u[j])
    # faktor 1: i tuan rumah  -> a~_ij.i = Wt[i,j], a~_ji.i = Lt[i,j], t = T[i,j]
    if (Nt[i, j] > 0)
      ll <- ll + Wt[i, j] * log(g * u[i]) + Lt[i, j] * log(u[j]) +
        T[i, j] * log(th * s) - Nt[i, j] * log(g * u[i] + u[j] + th * s)
    # faktor 2: j tuan rumah  -> a~_ij.j = Lt[j,i], a~_ji.j = Wt[j,i], t = T[j,i]
    if (Nt[j, i] > 0)
      ll <- ll + Lt[j, i] * log(u[i]) + Wt[j, i] * log(g * u[j]) +
        T[j, i] * log(th * s) - Nt[j, i] * log(u[i] + g * u[j] + th * s)
  }
  ll
}

# Algoritma MM untuk memaksimumkan L5
david_eps <- function(W, L, T, eps, maxit = 20000, tol = 1e-10) {
  a <- tambah_eps(W, L, T, eps); Wt <- a$Wt; Lt <- a$Lt; Nt <- a$Nt
  m <- nrow(W); u <- rep(1 / m, m); g <- 1; th <- 1
  
  Dmat <- function(u, g, th)
    g * matrix(u, m, m) + matrix(u, m, m, byrow = TRUE) + th * sqrt(outer(u, u))
  ll_vec <- function(u, g, th) {       # versi vektor dari log L5 (untuk iterasi)
    lu <- log(u); Li <- matrix(lu, m, m); Lj <- matrix(lu, m, m, byrow = TRUE)
    sum(Wt * (log(g) + Li) + Lt * Lj + T * (log(th) + .5 * Li + .5 * Lj)) -
      sum(Nt * log(Dmat(u, g, th)))
  }
  
  ll_old <- ll_vec(u, g, th); trace <- ll_old
  for (it in 1:maxit) {
    D <- Dmat(u, g, th); S <- sqrt(outer(1 / u, u))    # S[i,j] = sqrt(u_j/u_i)
    num <- rowSums(Wt + T / 2) + colSums(Lt + T / 2)
    den <- rowSums(Nt * (g + th / 2 * S) / D) +
      colSums(Nt * (1 + th / 2 * t(S)) / D)
    g_new  <- sum(Wt) / sum(Nt * matrix(u, m, m) / D)
    th_new <- sum(T)  / sum(Nt * sqrt(outer(u, u)) / D)
    u_new  <- num / den
    u <- u_new / sum(u_new)                            # sum(u) = 1
    g <- g_new; th <- th_new
    ll <- ll_vec(u, g, th); trace <- c(trace, ll)
    if (abs(ll - ll_old) < tol) break
    ll_old <- ll
  }
  names(u) <- rownames(W)
  list(u = u, gamma = g, theta = th, loglik = ll, iter = it, eps = eps,
       loglik_L5 = loglik_L5(u, g, th, Wt, Lt, T, Nt), trace = trace)
}

# Komponen terhubung dari graf pertandingan (dua tim terhubung jika pernah bertanding).
# Skala u antar-komponen TIDAK teridentifikasi: likelihood tidak berubah jika u satu
# komponen dikalikan konstanta. Jadi u hanya bisa dibandingkan dalam satu komponen.
komponen <- function(W, L, T) {
  N <- W + L + T; A <- (N + t(N)) > 0; m <- nrow(N); comp <- rep(0L, m); k <- 0L
  for (s in 1:m) if (comp[s] == 0L) {
    k <- k + 1L; st <- s
    while (length(st)) {
      v <- st[1]; st <- st[-1]
      if (comp[v] == 0L) { comp[v] <- k; st <- c(st, which(A[v, ] & comp == 0L)) }
    }
  }
  names(comp) <- rownames(W); comp
}

# Ambil subset tim dalam satu komponen (default: komponen terbesar)
pilih_komponen <- function(W, L, T, k = NULL) {
  comp <- komponen(W, L, T)
  if (is.null(k)) k <- as.integer(names(which.max(table(comp))))
  id <- which(comp == k)
  list(W = W[id, id], L = L[id, id], T = T[id, id], tim = rownames(W)[id], comp = comp)
}

#  Contoh pemakaian
d <- pilih_komponen(W, L, T)             # buang tim yang terpisah dari jaringan utama
lit <- david_eps(d$W, d$L, d$T,eps = eps)
hasil <- data.frame(tim = names(lit$u), u = lit$u)
hasil <- hasil[order(-hasil$u), ]
hasil
c(gamma = lit$gamma, theta = lit$theta, sum_u = sum(lit$u))
sum(hasil$u)

library(readxl)
komp <- read_excel("Kuliah Statistika Unpad/My Lomba/Mention/UCL.xlsx")
df_koefisien <- data.frame(
  tim_asli = komp$`Nama Klub`,
  poin_koefisien = komp$`Coefficient Club Points`
)
unique(komp$`Nama Klub`)
# Pemetaan nama tim agar presisi dengan objek 'hasil' dari skrip sebelumnya
peta_koef <- c(
  "Paris Saint-Germain" = "Paris SG",
  "Atletico Madrid"     = "Atlético Madrid",
  "Manchester United"   = "Manchester Utd",
  "Shakhtar Donetskk"   = "Shakhtar D.",
  "Sabah"               = "Sabah FK",
  "Slovan"              = "Slovan Bratislava",
  "RB Leipziq"          = "RB Leipzig",
  "Atletico Madrid"   = "Atl\u00e9tico Madrid",
  "Bod\u00f8 Glimt"   = "Bod\u00f8/Glimt",
  "Borussia Dortmund" = "Dortmund",
  "Fenerbahce"        = "Fenerbah\u00e7e",
  "Inter Milan"       = "Inter",
  "Lask"              = "LASK",
  "Machester United"  = "Manchester Utd",
  "PSG"               = "Paris SG",
  "RC Lens"           = "Lens",
  "Sabah"             = "Sabah FK",
  "Shakhtar Donetskk" = "Shakhtar D.",
  "Slovan"            = "Slovan Bratislava",
  "Sporting"          = "Sporting CP",
  "VfB Stuttgart"      = "Stuttgart",
  "Viking FK"         = "Viking",
  "Villarreal FC"         = "Villarreal",
  "FC Barcelona" =  "Barcelona",
  "Lille OSC" = "Lille",
  "FC Porto"   = "Porto",
  "PSV Eindhoven"="PSV"
  # Mengoreksi typo pada sumber data
)

df_koefisien$tim <- ifelse(
  df_koefisien$tim_asli %in% names(peta_koef),
  peta_koef[df_koefisien$tim_asli],
  df_koefisien$tim_asli
)

# Menggabungkan (join) poin koefisien ke dalam dataframe hasil
library(dplyr)
hasil_akhir <- left_join(hasil, df_koefisien %>% select(tim, poin_koefisien), by = "tim")
hasil_akhir

# Imputasi nilai NA untuk robustness (jika ada string yang luput, diberi nilai minimum koefisien)
min_koef <- min(hasil_akhir$poin_koefisien, na.rm = TRUE)
hasil_akhir$poin_koefisien[is.na(hasil_akhir$poin_koefisien)] <- min_koef

# Operasi pembobotan: u * poin_koefisien
# Poin koefisien di sini berfungsi sebagai scaling factor yang proporsional
hasil
hasil_akhir$u_berbobot <- hasil_akhir$u * hasil_akhir$poin_koefisien

# Normalisasi ulang: membagi u_berbobot dengan total sum(u_berbobot)
# Hal ini wajib dilakukan agar total parameter kembali bernilai 1 (syarat model Bradley-Terry)
hasil_akhir$u_final_norm <- hasil_akhir$u_berbobot / sum(hasil_akhir$u_berbobot, na.rm = TRUE)

# Menata ulang klasemen berdasarkan nilai probabilitas yang baru
hasil_akhir <- hasil_akhir %>%
  arrange(desc(u_final_norm)) %>%
  mutate(peringkat_baru = row_number()) %>%
  select(peringkat_baru, tim, poin_koefisien, u_asli = u, u_final_norm)

# Menampilkan dataframe akhir
print(hasil_akhir)