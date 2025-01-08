library(readxl)
library(dplyr)
library(stringr)
library(tidyr)
library(writexl)
library(GA)

set.seed(1)
# Memuat data dari file
year <- '2015'
target_month <- 'Jul.2015'
data <- read_xlsx('Jul 2015.xlsx', sheet = 1, skip = 1)


previous_year <- as.character(as.numeric(year) - 1)  # Mengubah year menjadi numerik, lalu dikurangi 1, kemudian dikembalikan menjadi string
Profitability <- read_xlsx('PROFITABILITY.xlsx', sheet = 1, skip = 1)
Growth <- read_xlsx('Growth.xlsx', sheet = 1, skip = 1)
Aset <- read_xlsx('total_aset.xlsx')
Rf <- read_xlsx('Market Risk Premium.xlsx')

# Convert the first column to Date format and then format it to "Jul.2015"
Rf[[1]] <- format(as.Date(Rf[[1]]), "%b.%Y")

risk_free_rate_value <- Rf %>%
  filter(`...1` == target_month) %>%  # Replace `...1` with the actual column name if it's different
  pull(`Risk free rate`)    


data['profitability'] <- Profitability[year]
data['growth'] <- Growth[year]
data['aset'] <- Aset[previous_year]


rows_to_remove <- c(96 ,431,580)
data <- data %>%
  slice(-rows_to_remove)

esg <- read_xlsx('esg dataset.xlsx', sheet = 1)
# Menghapus baris di mana 'nama perusahaan' hanya angka
esg <- esg %>%
  filter(!grepl("^[0-9]+$", `nama perusahaan`)) %>%
  select(company_name = `nama perusahaan`, ESG)

# Membersihkan kolom 'Emiten' dan 'nama perusahaan' dengan menghapus spasi dan mengubah ke huruf besar
data$Emiten <- str_trim(data$Emiten) %>% toupper()
esg$company_name <- str_trim(esg$company_name) %>% toupper()

# Melakukan left join seperti VLOOKUP untuk menambahkan kolom ESG ke dalam dataset 'data'
data <- data %>%
  left_join(esg, by = c("Emiten" = "company_name"))

# Mengisi nilai NA di kolom ESG dengan "N" jika tidak ditemukan hasil VLOOKUP
data <- data %>%
  replace_na(list(ESG = "N"))


# Membersihkan kolom 'Kode perusahaan': Menghapus spasi di depan/belakang dan mengubah ke huruf besar
data$`Kode perusahaan` <- str_trim(data$`Kode perusahaan`) %>% toupper()

# Memuat data harga penutupan dari file adjusted closing price
closing_prices <- read_xlsx('closingprice.xlsx')
closing_prices$Ticker <- str_trim(closing_prices[[1]]) %>% toupper()
date_cols <- as.Date(as.numeric(names(closing_prices)[-1]), origin = "1899-12-30")
formatted_dates <- format(date_cols, "%b %Y")
names(closing_prices)[-1] <- formatted_dates

# Memeriksa nama kolom
colnames(closing_prices)

# Mengganti nama kolom kosong atau NA dengan nama yang valid
valid_names <- make.names(colnames(closing_prices), unique = TRUE)
colnames(closing_prices) <- valid_names

# Memastikan tidak ada kolom dengan nama kosong atau NA setelah ini
colnames(closing_prices)

# Misalkan target_month sudah didefinisikan sebagai "Sep.2015"

target_index <- match(target_month, colnames(closing_prices))

# Identifikasi kolom untuk 12 bulan sebelumnya (Sep 2014 hingga Aug 2015)
twelve_months_cols <- colnames(closing_prices)[(target_index - 12):(target_index - 1)]

# Inisialisasi data frame untuk menyimpan hasil log return dengan header bulannya
log_returns_df <- data.frame(matrix(nrow = nrow(closing_prices), ncol = length(twelve_months_cols)))
colnames(log_returns_df) <- twelve_months_cols


# Loop untuk menghitung log return setiap bulan
for (i in 1:length(twelve_months_cols)) {
  # Indeks kolom bulan sekarang dan bulan sebelumnya
  current_col <- twelve_months_cols[i]
  previous_col <- colnames(closing_prices)[match(current_col, colnames(closing_prices)) - 1]
  
  # Menghitung log return dan menyimpannya ke dalam data frame
  log_returns_df[, current_col] <- log(closing_prices[, current_col] / closing_prices[, previous_col])
}

# Menampilkan data frame hasil log return dengan header bulannya
log_returns_df


# Menghitung rata-rata log return untuk setiap baris
average_return_12m <- rowMeans(log_returns_df, na.rm = TRUE)

# Menambahkan rata-rata log return ke dalam data frame hasil sebagai kolom baru
log_returns_df$average_return_12m <- average_return_12m 
log_returns_df$Emiten <- data$Emiten


# Menambahkan kolom rata-rata return 12 bulan sebelumnya ke data utama
data$average_return_12m <- log_returns_df$average_return_12m
previous_month <- colnames(closing_prices)[target_index - 1]

# Menghitung return berdasarkan bulan sebelumnya dan target month
data$bulan_sebelumnya <- closing_prices[[previous_month]]
data$bulan_setelahnya <- closing_prices[[target_month]]
data$Return <- log(data$bulan_setelahnya / data$bulan_sebelumnya)

###############################################################################################
# Menampilkan perusahaan yang memiliki NA di kolom Total equity atau AVERAGE MARKET CAP
perusahaan_teks_na <- data %>%
  filter(`Total equty` == "NA" | `AVERAGE MARKET CAP` == "NA")

# Menyimpan daftar perusahaan yang memiliki NA di kolom Total equity atau AVERAGE MARKET CAP
perusahaan_real_na <- data %>%
  filter(is.na(`Total equty`) | is.na(`AVERAGE MARKET CAP`)) %>%
  select(Emiten, `Total equty`, `AVERAGE MARKET CAP`)


# Menghapus baris dengan NA di kolom Total equity atau AVERAGE MARKET CAP dari dataframe asli
data <- data %>%
  filter(!is.na(`Total equty`) & !is.na(`AVERAGE MARKET CAP`) & `Total equty` != "NA" & `AVERAGE MARKET CAP` != "NA")

# Menampilkan data setelah baris dengan NA dihapus
print(data)

###############################################################################################
data$`AVERAGE MARKET CAP` <- as.numeric(data$`AVERAGE MARKET CAP`)
# Menambahkan kolom log dari market cap
data <- data %>%
  mutate(Log_Market_Cap = log(`AVERAGE MARKET CAP`))

# Identifikasi emiten yang memiliki NA pada kolom tertentu dan simpan dalam variabel
emiten_na <- data %>%
  filter(is.na(profitability) | is.na(growth) | is.na(`BE/ME`) | is.na(Log_Market_Cap) | is.na(average_return_12m)) %>%
  select(Emiten)

# Menghapus baris dengan NA pada kolom profitability, growth, BE/ME, Log_Market_Cap, dan average_return_12m langsung di variabel data
data <- data %>%
  filter(!is.na(profitability) & !is.na(growth) & !is.na(`BE/ME`) & !is.na(Log_Market_Cap) & !is.na(average_return_12m))


# Buat subset dari data yang memiliki nilai Inf pada average_return_12m
perusahaan_inf <- data %>%
  filter(!is.finite(average_return_12m)) %>%
  select(Emiten)  # Ganti 'Emiten' dengan nama kolom yang berisi nama perusahaan

data <- data %>%
  filter(is.finite(average_return_12m))

# Memastikan tidak ada lagi nilai Inf di data
summary(data$average_return_12m)

# Gabungkan semua dataset yang berisi emiten yang ingin dihapus
daftar_emiten_dihapus <- bind_rows(
  emiten_na %>% select(Emiten),
  perusahaan_inf %>% select(Emiten),
  perusahaan_real_na %>% select(Emiten),
  perusahaan_teks_na %>% select(Emiten)
) %>%
  distinct()  # Hapus emiten duplikat jika ada
# Menghapus emiten dari log_returns_df berdasarkan daftar emiten yang akan dihapus
log_returns_df <- log_returns_df %>%
  anti_join(daftar_emiten_dihapus, by = "Emiten")


# Gabungkan log_returns_df dengan data berdasarkan kolom Emiten
# Pastikan kolom Emiten di kedua dataset sudah dalam format yang sama (uppercase, trim whitespace)
log_returns_df$Emiten <- str_trim(toupper(log_returns_df$Emiten))
data$Emiten <- str_trim(toupper(data$Emiten))

# Lakukan left join untuk mendapatkan informasi ESG dari dataset 'data'
log_returns_df$ESG <- data$ESG


# Iterasi melalui setiap kolom log_returns_df yang berisi log return bulanan
for (col in twelve_months_cols) {
  # Ganti NA pada kolom log_return dengan nilai dari average_return_12m di baris yang sama
  log_returns_df[[col]][is.na(log_returns_df[[col]])] <- log_returns_df$average_return_12m[is.na(log_returns_df[[col]])]
}

###########################################
# Filter data berdasarkan kategori ESG
esg_groups <- list(
  H = data %>% filter(ESG == "H"),
  M = data %>% filter(ESG == "M"),
  L = data %>% filter(ESG == "L"),
  N = data %>% filter(ESG == "N")
)

# Fungsi untuk menghitung Sharpe Ratio dan menghasilkan bobot optimal
calculate_sharpe_ratio <- function(group_data) {
  num_assets <- nrow(group_data)
  
  # Ambil log return yang relevan berdasarkan emiten di grup ini
  relevant_rows <- which(log_returns_df$Emiten %in% group_data$Emiten)
  returns_matrix <- as.matrix(log_returns_df[relevant_rows, twelve_months_cols, drop = FALSE])
  
  # Jika perusahaan kurang dari 2, tidak bisa menghitung matriks kovarian
  if (nrow(returns_matrix) < 2) return(NULL)
  
  # Hitung matriks kovarian untuk return perusahaan di grup ini
  cov_matrix <- cov(returns_matrix, use = "pairwise.complete.obs")
  
  # Periksa dimensi matriks kovarian dan vektor bobot
  cat("Dimensi weights:", num_assets, "\n")
  cat("Dimensi cov_matrix:", dim(cov_matrix), "\n")
  
  # Fungsi fitness untuk GA (berdasarkan Sharpe Ratio)
  sharpe_ratio_fitness <- function(weights) {
    weights <- weights / sum(weights)  # Normalisasi bobot
    portfolio_return <- sum(weights * group_data$average_return_12m)
    portfolio_variance <- t(weights) %*% cov_matrix %*% weights
    portfolio_sd <- sqrt(portfolio_variance)
    
    # Hindari deviasi standar 0
    if (is.na(portfolio_sd) || portfolio_sd == 0) return(Inf)
    
    # Hitung Sharpe Ratio
    sharpe_ratio <- (portfolio_return - risk_free_rate_value) / portfolio_sd
    
    return(-sharpe_ratio)  # GA akan meminimasi nilai
  }
  
  # Setup Genetic Algorithm (GA) untuk mencari bobot optimal
  ga_optim <- ga(
    type = "real-valued",
    fitness = sharpe_ratio_fitness,
    lower = rep(0, num_assets),  # Bobot terendah 0
    upper = rep(1, num_assets),  # Bobot tertinggi 1
    popSize = 50,                # Ukuran populasi
    maxiter = 100,               # Iterasi maksimum
    run = 50,                    # Stop jika tidak ada perbaikan
    seed = 123                   # Seed untuk reprodusibilitas
  )
  
  # Dapatkan bobot optimal dari GA dan normalisasi
  optimal_weights <- ga_optim@solution / sum(ga_optim@solution)
  
  # Hitung Sharpe Ratio optimal menggunakan bobot optimal
  optimal_sharpe_ratio <- -sharpe_ratio_fitness(optimal_weights)
  
  # Kembalikan bobot optimal dan Sharpe Ratio optimal
  return(list(weights = optimal_weights, sharpe_ratio = optimal_sharpe_ratio))
}

# Terapkan fungsi ke masing-masing grup ESG dan hitung return portofolio
results <- lapply(esg_groups, function(group) {
  if (nrow(group) > 1) {  # Hanya hitung jika ada lebih dari 1 perusahaan
    sharpe_result <- calculate_sharpe_ratio(group)
    
    # Jika ada hasil Sharpe Ratio, hitung return portofolio
    if (!is.null(sharpe_result)) {
      # Hitung return portofolio (Bobot * Return aktual)
      portfolio_return <- sum(sharpe_result['weights'] * group$Return)
      return(list(
        #weights = sharpe_result$weights,
        weights = sharpe_result['weights'],
        sharpe_ratio = sharpe_result$sharpe_ratio,
        portfolio_return = portfolio_return
      ))
    } else {
      return(NULL)
    }
  } else {
    warning(paste("Group", unique(group$ESG), "has too few companies to calculate Sharpe Ratio."))
    return(NULL)
  }
})

# Tampilkan hasil
results
