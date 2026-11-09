# Data
Akhir <- read.csv("Kuliah Statistika Unpad/My Lomba/Mention/Before Hope.csv")
Akhir <- Akhir[, c("Team", "Comp", "Venue", "GF", "GA", "Opponent")]
unique(Akhir$Team)

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
Akhir$Team     <- samakan(Akhir$Team)
Akhir$Opponent <- samakan(Akhir$Opponent)
unique(Akhir$Team)
unique(Akhir$Opponent)

buang <- c("Paris SG","Arsenal")
Akhir <- Akhir[(Akhir$Team %in% buang) & (Akhir$Opponent %in% buang), ]
unique(Akhir$Team)

# ==========================================================================
# Membentuk input A untuk bt_mm dari data hasil pertandingan
#
# Data: Team, Comp, Venue, GF, GA, Opponent (satu baris per tim per laga)
# A[i,j] = jumlah kemenangan i atas j (diagonal = 0)
# Seri (GF == GA) diabaikan karena BT biasa tidak memodelkan seri
# ==========================================================================

buat_A <- function(data, comp = NULL) {
  if (!is.null(comp)) data <- data[data$Comp == comp, ]
  
  # Satu pertandingan = satu baris Home (menghindari hitung ganda)
  d <- data
  
  tim <- sort(unique(c(data$Team, data$Opponent)))
  m <- length(tim)
  A <- matrix(0, m, m, dimnames = list(tim, tim))
  
  for (r in seq_len(nrow(d))) {
    h <- d$Team[r]; a <- d$Opponent[r]
    if (d$GF[r] > d$GA[r]) A[h, a] <- A[h, a] + 1      # tuan rumah menang
    if (d$GF[r] < d$GA[r]) A[a, h] <- A[a, h] + 1      # tamu menang
  }
  A
}

A <- buat_A(Akhir)
A

# ==========================================================================
# Model Bradley-Terry biasa: memaksimumkan likelihood L (persamaan 1)
#
#   L = prod_{i != j} ( u_i / (u_i + u_j) )^{a_ij}
#
#   p_ij = u_i / (u_i + u_j)
#
# Input:
#   A[i,j] = a_ij : jumlah kemenangan i atas j (diagonal = 0)
#   (tanpa seri, tanpa home advantage, tanpa penalti epsilon)
# Batasan identifikasi: sum(u) = 1
#
# Algoritma MM (Hunter, 2004):
#   Minorisasi: -ln(u_i + u_j) >= -ln(u_i^(k) + u_j^(k))
#                                 - (u_i + u_j)/(u_i^(k) + u_j^(k)) + 1
#   Update tertutup per parameter:
#     u_i^(k+1) = W_i / sum_{j != i} [ N_ij / (u_i^(k) + u_j^(k)) ]
#   dengan W_i = sum_j a_ij (total menang) dan N_ij = a_ij + a_ji
#
# Syarat konvergensi (Ford, 1957): setiap objek harus punya minimal satu
# kemenangan, dan graf kemenangan harus terhubung kuat.
# ==========================================================================

# Fungsi log-likelihood persis mengikuti persamaan (1)
loglik_BT <- function(u, A) {
  m <- length(u)
  ll <- 0
  for (i in 1:m) {
    for (j in 1:m) {
      if (i != j && A[i, j] > 0) {
        ll <- ll + A[i, j] * (log(u[i]) - log(u[i] + u[j]))
      }
    }
  }
  return(ll)
}

# Algoritma MM untuk memaksimumkan L
bt_mm <- function(A, maxit = 20000, tol = 1e-10) {
  m <- nrow(A)
  diag(A) <- 0
  
  Wi <- rowSums(A)       # total kemenangan tiap objek
  N  <- A + t(A)         # total pertandingan tiap pasangan (diagonal = 0)
  
  if (any(Wi == 0)) {
    warning("Ada objek tanpa kemenangan: u_i -> 0 dan MLE tidak ada (syarat Ford dilanggar).")
  }
  
  # Inisialisasi
  u <- rep(1 / m, m)
  
  # Versi vektor dari log L untuk komputasi cepat dalam iterasi
  ll_vec <- function(u) {
    lu <- log(u)
    sum(A * (matrix(lu, m, m) - log(outer(u, u, "+"))))
  }
  
  ll_old <- ll_vec(u)
  trace <- ll_old
  
  for (it in 1:maxit) {
    # --- Update parameter kekuatan (u) ---
    D <- outer(u, u, "+")             # D_ij = u_i + u_j
    den_u <- rowSums(N / D)           # diagonal N = 0, jadi tidak ikut
    u_new <- Wi / den_u
    u <- u_new / sum(u_new)           # Normalisasi sum(u) = 1
    
    # --- Evaluasi konvergensi ---
    ll <- ll_vec(u)
    trace <- c(trace, ll)
    if (abs(ll - ll_old) < tol) break
    ll_old <- ll
  }
  
  names(u) <- rownames(A)
  list(u = u, loglik = ll, iter = it, converged = it < maxit,
       loglik_BT = loglik_BT(u, A), trace = trace)
}

res <- bt_mm(A)
u_final <- res$u;u_final

# Simulasi Terakhir
Tim1="Arsenal"
Tim2="PSG"
Kemenangan_Tim1=u_final[1]/(sum(u_final))
Kekalahan_Tim1=1-Simulasi5$Kemenangan_Tim1
Pemenang=ifelse(Kemenangan_Tim1>Kekalahan_Tim1,
                Tim1,
                Tim2)

Simulasi5 <- data.frame(
  Tim1="Arsenal",
  Tim2="PSG",
  Kemenangan_Tim1=u_final[1]/(sum(u_final)),
  Kekalahan_Tim1=1-Simulasi5$Kemenangan_Tim1,
  Pemenang=ifelse(Kemenangan_Tim1>Kekalahan_Tim1,
                  Tim1,
                  Tim2)
)
