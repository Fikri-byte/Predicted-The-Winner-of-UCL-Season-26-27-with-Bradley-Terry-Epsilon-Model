library(readxl)
# Mengubah Hope.csv (format per-tim) menjadi matriks W, L, T
# untuk fungsi david_mm(W, L, T)
# Baris = tuan rumah, kolom = tamu
library(dplyr)
df <- read.csv("Kuliah Statistika Unpad/My Lomba/Mention/Before Hope.csv")
df <- df[, c("Team", "Comp", "Venue", "GF", "GA", "Opponent")]
unique(df$Team)
table(df$Team)
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

buang <- c("Feyenoord","RB Leipzig","Villarreal","Stuttgart",
           "Lens","Como","Viking", "Slovan Bratislava","AEK Athens",
           "Sabah FK", "Slavia Prague", "LASK")
df <- df[!(df$Team %in% buang) & !(df$Opponent %in% buang), ]
unique(df$Team)
table(df$Team)

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

cat("Laga (tuan rumah vs tamu) setelah dibersihkan:", nrow(unik), "\n")

# ---- 6. Bentuk matriks W, L, T (m xm) ----
tim <- sort(unique(c(unik$home, unik$away)))
unik$home <- factor(unik$home, levels = tim)
unik$away <- factor(unik$away, levels = tim)
table(unik$home)
table(unik$away)

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
cat("Jumlah tim:", length(tim), "| Total laga:", sum(N),
    "| W:", sum(W), "L:", sum(L), "T:", sum(T), "\n")
cat("Tim tanpa laga:", sum(rowSums(N) + colSums(N) == 0), "\n")

# Cek kondisi koneksi sederhana: tim yang tidak pernah menang / tidak pernah kalah
menang <- rowSums(W) + colSums(L)
kalah  <- rowSums(L) + colSums(W)
cat("Tim tanpa kemenangan:", paste(tim[menang == 0], collapse = ", "), "\n")
cat("Tim tanpa kekalahan :", paste(tim[kalah  == 0], collapse = ", "), "\n")

# ---- 9. Penyesuaian epsilon (strong connection gagal: ada tim tanpa kemenangan) ----
eps <- sqrt(log(24)/24);eps
W_adj <- W; L_adj <- L
W_adj[N > 0] <- W_adj[N > 0] + eps
L_adj[N > 0] <- L_adj[N > 0] + eps
unique(df$Team)
unique(unik$home)
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
m_tim <- 24
eps_val <- sqrt(log(m_tim) / m_tim)
cat("Nilai epsilon:", eps_val, "\n")

# Pembentukan a~ dan n~ dari data mentah
tambah_eps_hfa <- function(W, L, eps) {
  N0 <- W + L                              # n_ij.i (total pertandingan tanpa seri)
  I_N <- (N0 > 0)                          # Indikator pernah bertanding
  Wt <- W + eps * I_N                      # a~_ij.i
  Lt <- L + eps * I_N                      # a~_ji.i
  list(Wt = Wt, Lt = Lt, Nt = Wt + Lt)     # n~_ij.i = a~_ij.i + a~_ji.i
}

# Fungsi log-likelihood L4 persis mengikuti persamaan (6)
loglik_L4 <- function(u, g, Wt, Lt, Nt) {
  m <- length(u)
  ll <- 0
  for (i in 1:(m - 1)) {
    for (j in (i + 1):m) {
      # Faktor 1: i sebagai tuan rumah -> a~_ij.i = Wt[i,j], a~_ji.i = Lt[i,j]
      if (Nt[i, j] > 0) {
        ll <- ll + Wt[i, j] * log(g * u[i]) + Lt[i, j] * log(u[j]) - 
          Nt[i, j] * log(g * u[i] + u[j])
      }
      # Faktor 2: j sebagai tuan rumah -> a~_ij.j = Lt[j,i], a~_ji.j = Wt[j,i]
      if (Nt[j, i] > 0) {
        ll <- ll + Lt[j, i] * log(u[i]) + Wt[j, i] * log(g * u[j]) - 
          Nt[j, i] * log(u[i] + g * u[j])
      }
    }
  }
  return(ll)
}

# Algoritma MM untuk memaksimumkan L4
agresti_eps <- function(W, L, eps = eps_val, maxit = 20000, tol = 1e-10) {
  a <- tambah_eps_hfa(W, L, eps)
  Wt <- a$Wt; Lt <- a$Lt; Nt <- a$Nt
  m <- nrow(W)
  
  # Inisialisasi
  u <- rep(1 / m, m) 
  g <- 1  # gamma awal
  
  # Matriks penyebut (Denominator): D_ij = gamma * u_i + u_j
  Dmat <- function(u, g) {
    g * matrix(u, m, m) + matrix(u, m, m, byrow = TRUE)
  }
  
  # Versi vektor dari log L4 untuk komputasi cepat dalam iterasi
  ll_vec <- function(u, g) {
    lu <- log(u)
    Li <- matrix(lu, m, m)
    Lj <- matrix(lu, m, m, byrow = TRUE)
    sum(Wt * (log(g) + Li) + Lt * Lj) - sum(Nt * log(Dmat(u, g)))
  }
  
  ll_old <- ll_vec(u, g)
  trace <- ll_old
  
  for (it in 1:maxit) {
    D <- Dmat(u, g)
    
    # --- Update parameter kekuatan (u) ---
    # Numerator untuk u_i: total kemenangan tim i (kandang dan tandang)
    num_u <- rowSums(Wt) + colSums(Lt)
    # Denominator untuk u_i
    den_u <- rowSums(Nt * g / D) + colSums(Nt / t(D))
    
    u_new <- num_u / den_u
    u <- u_new / sum(u_new)  # Normalisasi sum(u) = 1
    
    # --- Update parameter home advantage (gamma) ---
    num_g <- sum(Wt)
    den_g <- sum(Nt * matrix(u, m, m) / D)
    g_new <- num_g / den_g
    g <- g_new
    
    # --- Evaluasi konvergensi ---
    ll <- ll_vec(u, g)
    trace <- c(trace, ll)
    if (abs(ll - ll_old) < tol) break
    ll_old <- ll
  }
  
  names(u) <- rownames(W)
  list(u = u, gamma = g, loglik = ll, iter = it, eps = eps,
       loglik_L4 = loglik_L4(u, g, Wt, Lt, Nt), trace = trace)
}

# --- Contoh Penggunaan (Bila menggunakan matriks W dan L dari skrip Anda sebelumnya) ---
res_agresti <- agresti_eps(W, L, eps = eps_val)
hasil_agresti <- data.frame(tim = names(res_agresti$u), u = res_agresti$u)
hasil_agresti <- hasil_agresti[order(-hasil_agresti$u), ]
colnames(hasil_agresti) <- c("Tim","u")
cat("Estimasi gamma:", res_agresti$gamma, "\n")
print((hasil_agresti))
nrow(hasil_agresti)

library(readxl)
dat = read_excel("Kuliah Statistika Unpad/My Lomba/Mention/UCL.xlsx")
club_coef = dat$`Coefficient Club Points`
dat <- data.frame(
  Tim = dat$`Nama Klub`,
  Club_coef=club_coef
)
unique(dat$Tim)
unique(hasil_agresti$Tim)

# Menyelaraskan nama tim yang berbeda di hasil_agresti agar sama dengan data awal Anda
hasil_agresti$Tim[hasil_agresti$Tim == "Paris SG"]         <- "Paris Saint-Germain"
hasil_agresti$Tim[hasil_agresti$Tim == "Manchester Utd"]   <- "Manchester United"
hasil_agresti$Tim[hasil_agresti$Tim == "Atlético Madrid"]  <- "Atletico Madrid"
hasil_agresti$Tim[hasil_agresti$Tim == "Barcelona"]        <- "FC Barcelona"
hasil_agresti$Tim[hasil_agresti$Tim == "Porto"]            <- "FC Porto"
hasil_agresti$Tim[hasil_agresti$Tim == "Dortmund"]         <- "Borussia Dortmund"
hasil_agresti$Tim[hasil_agresti$Tim == "Stuttgart"]        <- "VfB Stuttgart"
hasil_agresti$Tim[hasil_agresti$Tim == "Shakhtar D."]      <- "Shakhtar Donetskk"
hasil_agresti$Tim[hasil_agresti$Tim == "Villarreal"]       <- "Villarreal FC"
hasil_agresti$Tim[hasil_agresti$Tim == "Slovan Bratislava"]<- "Slovan"
hasil_agresti$Tim[hasil_agresti$Tim == "Sabah FK"]         <- "Sabah"
hasil_agresti$Tim[hasil_agresti$Tim == "Inter"]            <- "Inter Milan"
hasil_agresti$Tim[hasil_agresti$Tim == "PSV"]              <- "PSV Eindhoven"
hasil_agresti$Tim[hasil_agresti$Tim == "Lens"]             <- "RC Lens"

dat <- dat %>%
  mutate(Tim = recode(Tim,
                      "RB Leipziq"       = "RB Leipzig",
                      "Shakhtar Donetskk" = "Shakhtar Donetsk",
                      "Lille OSC"        = "Lille",
                       "Shakhtar Donetsk" = "FC Shakhtar Donetsk"
  ))

hasil_agresti <- hasil_agresti %>%
  mutate(Tim = recode(Tim,
                      "Shakhtar Donetsk" = "FC Shakhtar Donetsk",
                      "FC Bayern München"= "Bayern Munich"
  ))

u2 <- merge(dat,hasil_agresti,by="Tim")
u2$ubobot <- u2$u*u2$Club_coef
u2$bobotfin <- u2$ubobot/sum(u2$ubobot)

u2 <- data.frame(
  Tim = u2$Tim,
  u_david = u2$bobotfin
)
u2 <- u2[order(u2$u_david,decreasing = TRUE),]
u2
nrow(u2)
unique(u2$Tim)
u2 <- u2 %>%
  mutate(Tim = recode(Tim,
                      "Roma" = "AS Roma",
                      "Atletico Madrid"="Atlético Madrid",
                      "Lille"="LOSC Lille",
                      "Napoli"="SSC Napoli",
                     "Bayern Munich"= "FC Bayern München",
  ))


#Simulasi
SimulasiK1 <- read_excel("Kuliah Statistika Unpad/My Lomba/Mention/Prediksi Simulasi UCL.xlsx")
SimulasiK1

# JANGAN DIENTER
T1 <- sample(SimulasiK1$Tim[15:16],size=1);T1 #Porto
T2 <- sample(SimulasiK1$Tim[17:18],size=1);T2 #Club Brugge

T3 <- sample(SimulasiK1$Tim[13:14],size=1);T3 #Sporting CP
T4 <- sample(SimulasiK1$Tim[19:20],size=1);T4 #Fenerbahce

T5 <- sample(SimulasiK1$Tim[11:12],size=1);T5 #PSV
T6 <- sample(SimulasiK1$Tim[21:22],size=1);T6 #Shaktar

T7 <- sample(SimulasiK1$Tim[9:10],size=1);T7 #MU
T8 <- sample(SimulasiK1$Tim[23:24],size=1);T8 #Galatasaray

T9 <- setdiff(SimulasiK1$Tim[15:16],T1);T9 #ATM
T10 <- setdiff(SimulasiK1$Tim[17:18],T2);T10 #DORTMUND

T11 <- setdiff(SimulasiK1$Tim[13:14],T3);T11 #REAL BETIS 
T12 <- setdiff(SimulasiK1$Tim[19:20],T4);T12 #Roma

T13 <- setdiff(SimulasiK1$Tim[11:12],T5);T13 #BODO/GLIMT
T14 <- setdiff(SimulasiK1$Tim[21:22],T6);T14 #Napoli

T15 <- setdiff(SimulasiK1$Tim[9:10],T7);T15 #City
T16 <- setdiff(SimulasiK1$Tim[23:24],T8);T16 #LILLE

#Validasi 
Klub <- c(T1,T2,T3,T4,
          T5,T6,T7,T8,
          T9,T10,T11,T12,
          T13,T14,T15,T16)
unique(Klub)

#Nyobain Simulasi
Simulasi1 <- NULL
Simulasi1 <- data.frame(
  Tim1=rbind(T1,T3,T5,T7,T9,T11,T13,T15),
  Tim2=rbind(T2,T4,T6,T8,T10,T12,T14,T16))
Simulasi1$u1 <- u2$u_david[match(Simulasi1$Tim1,u2$Tim)]
Simulasi1$u2 <- u2$u_david[match(Simulasi1$Tim2,u2$Tim)]
View(Simulasi1)

# Catatan 
# Menang1 = Peluang Tim 1 Menang di Kandang
# Kalah1 = Peluang Tim 1 Kalah di Kandang
# Menang2 = Peluang Tim 2 Menang di Kandang
# Kalah 2 = Peluang Tim 2 Kalah di Kandang
gamma <- res_agresti$gamma
Simulasi1$Menang1 <- gamma*Simulasi1$u1/(gamma*Simulasi1$u1+Simulasi1$u2)
Simulasi1$Kalah1 <- 1-Simulasi1$Menang1

Simulasi1$Menang2 <- gamma*Simulasi1$u2/(gamma*Simulasi1$u2+Simulasi1$u1)
Simulasi1$Kalah2 <- 1-Simulasi1$Menang2

Simulasi1$WW1 <- Simulasi1$Menang1*Simulasi1$Kalah2
Simulasi1$WW2 <- Simulasi1$Menang2*Simulasi1$Kalah1
Simulasi1$Pemenang <- ifelse(
  Simulasi1$WW1>Simulasi1$WW2,
  Simulasi1$Tim1,
  Simulasi1$Tim2
)
View(Simulasi1)


# Simulasi Babak 16 besar (JANGAN DIRUN)
K1 <- Simulasi1$Pemenang[1];K1 #Brugge
K2 <- sample(SimulasiK1$Tim[1:2],size=1);K2 #PSG

K3 <- Simulasi1$Pemenang[2];K3 #Sporting
K4 <- sample(SimulasiK1$Tim[3:4],size=1);K4 #Liverpool

K5 <- Simulasi1$Pemenang[3];K5 #PSV
K6 <- sample(SimulasiK1$Tim[5:6],size=1);K6 #Barcelona

K7 <- Simulasi1$Pemenang[4];K7 #MU
K8 <- sample(SimulasiK1$Tim[7:8],size=1);K8 #Aston Villa

K9 <- Simulasi1$Pemenang[5];K9 #ATM
K10 <- setdiff(SimulasiK1$Tim[1:2],K2);K10 #Bayern

K11 <- Simulasi1$Pemenang[6];K11 #Real Betis
K12 <- setdiff(SimulasiK1$Tim[3:4],K4);K12 #Arsenal

K13 <- Simulasi1$Pemenang[7];K13 #BODO/GLIMT
K14 <- setdiff(SimulasiK1$Tim[5:6],K6);K14 #Real Madrid

K15 <- Simulasi1$Pemenang[8];K15 #City
K16 <- setdiff(SimulasiK1$Tim[7:8],K8);K16 #Inter Milan

Simulasi2 <- NULL
Simulasi2 <- data.frame(
  Tim1=rbind(K1,K3,K5,K7,K9,K11,K13,K15),
  Tim2=rbind(K2,K4,K6,K8,K10,K12,K14,K16))
Simulasi2$u1 <- u2$u_david[match(Simulasi2$Tim1,u2$Tim)]
Simulasi2$u2 <- u2$u_david[match(Simulasi2$Tim2,u2$Tim)]
View(Simulasi2)

# Catatan 
# Menang1 = Peluang Tim 1 Menang di Kandang
# Kalah1 = Peluang Tim 1 Kalah di Kandang
# Menang2 = Peluang Tim 2 Menang di Kandang
# Kalah 2 = Peluang Tim 2 Kalah di Kandang
Simulasi2$Menang1 <- gamma*Simulasi2$u1/(gamma*Simulasi2$u1+Simulasi2$u2)
Simulasi2$Kalah1 <- 1-Simulasi2$Menang1

Simulasi2$Menang2 <- gamma*Simulasi2$u2/(gamma*Simulasi2$u2+Simulasi2$u1)
Simulasi2$Kalah2 <- 1-Simulasi2$Menang2
View(Simulasi2)

Simulasi2$WW1 <- Simulasi2$Menang1*Simulasi2$Kalah2
Simulasi2$WW2 <- Simulasi2$Menang2*Simulasi2$Kalah1
Simulasi2$Pemenang <- ifelse(
  Simulasi2$WW1>Simulasi2$WW2,
  Simulasi2$Tim1,
  Simulasi2$Tim2
)
View(Simulasi2)

# Simulasi Babak 8 besar
A1 <- Simulasi2$Pemenang[1];A1 #PSG
A2 <- Simulasi2$Pemenang[2];A2 #Liverpool

A3 <- Simulasi2$Pemenang[3];A3 #Barcelona
A4 <- Simulasi2$Pemenang[4];A4 #Aston Villa

A5 <- Simulasi2$Pemenang[5];A5 #Bayern
A6 <- Simulasi2$Pemenang[6];A6 #Arsenal

A7 <- Simulasi2$Pemenang[7];A7 #Real Madrid
A8 <- Simulasi2$Pemenang[8];A8 #City

Simulasi3 <- NULL
Simulasi3 <- data.frame(
  Tim1=rbind(A1,A3,A5,A7),
  Tim2=rbind(A2,A4,A6,A8))
Simulasi3$u1 <- u2$u_david[match(Simulasi3$Tim1,u2$Tim)]
Simulasi3$u2 <- u2$u_david[match(Simulasi3$Tim2,u2$Tim)]
View(Simulasi3)

# Catatan 
# Menang1 = Peluang Tim 1 Menang di Kandang
# Kalah1 = Peluang Tim 1 Kalah di Kandang
# Menang2 = Peluang Tim 2 Menang di Kandang
# Kalah 2 = Peluang Tim 2 Kalah di Kandang
Simulasi3$Menang1 <- gamma*Simulasi3$u1/(gamma*Simulasi3$u1+Simulasi3$u2)
Simulasi3$Kalah1 <- 1-Simulasi3$Menang1

Simulasi3$Menang2 <- gamma*Simulasi3$u2/(gamma*Simulasi3$u2+Simulasi3$u1)
Simulasi3$Kalah2 <- 1-Simulasi3$Menang2
View(Simulasi3)

Simulasi3$WW1 <- Simulasi3$Menang1*Simulasi3$Kalah2
Simulasi3$WW2 <- Simulasi3$Menang2*Simulasi3$Kalah1
Simulasi3$Pemenang <- ifelse(
  Simulasi3$WW1>Simulasi3$WW2,
  Simulasi3$Tim1,
  Simulasi3$Tim2
)
View(Simulasi3)

# Simulasi babak 4 besar
B1 <- Simulasi3$Pemenang[1];B1 #PSG
B2 <- Simulasi3$Pemenang[2];B2 #Barcelona

B3 <- Simulasi3$Pemenang[3];B3 #Arsenal
B4 <- Simulasi3$Pemenang[4];B4 #Real Madrid

Simulasi4 <- NULL
Simulasi4 <- data.frame(
  Tim1=rbind(B1,B3),
  Tim2=rbind(B2,B4))
Simulasi4$u1 <- u2$u_david[match(Simulasi4$Tim1,u2$Tim)]
Simulasi4$u2 <- u2$u_david[match(Simulasi4$Tim2,u2$Tim)]
View(Simulasi4)

# Catatan 
# Menang1 = Peluang Tim 1 Menang di Kandang
# Kalah1 = Peluang Tim 1 Kalah di Kandang
# Menang2 = Peluang Tim 2 Menang di Kandang
# Kalah 2 = Peluang Tim 2 Kalah di Kandang
Simulasi4$Menang1 <- gamma*Simulasi4$u1/(gamma*Simulasi4$u1+Simulasi4$u2)
Simulasi4$Kalah1 <- 1-Simulasi4$Menang1

Simulasi4$Menang2 <- gamma*Simulasi4$u2/(gamma*Simulasi4$u2+Simulasi4$u1)
Simulasi4$Kalah2 <- 1-Simulasi4$Menang2
View(Simulasi4)

Simulasi4$WW1 <- Simulasi4$Menang1*Simulasi4$Kalah2
Simulasi4$WW2 <- Simulasi4$Menang2*Simulasi4$Kalah1
Simulasi4$Pemenang <- ifelse(
  Simulasi4$WW1>Simulasi4$WW2,
  Simulasi4$Tim1,
  Simulasi4$Tim2
)
View(Simulasi4)
