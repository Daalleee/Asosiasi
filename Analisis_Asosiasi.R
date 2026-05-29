# ==============================================================================
# TUGAS AKHIR PENAMBANGAN DATA: ANALISIS ASOSIASI ONLINE RETAIL
# Algoritma: Apriori
# Deskripsi: Mengidentifikasi pola pembelian konsumen menggunakan Market Basket Analysis
# ==============================================================================

# ------------------------------------------------------------------------------
# TAHAP 1: PERSIAPAN LINGKUNGAN & LIBRARY
# ------------------------------------------------------------------------------
library(tidyverse)
library(arules)
library(arulesViz)
library(lubridate)

# Membuat folder output jika belum ada
output_dir <- "output"
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
  cat("Folder 'output' berhasil dibuat.\n")
}

# ------------------------------------------------------------------------------
# TAHAP 2: LOAD DATA & PREPROCESSING
# ------------------------------------------------------------------------------
cat("\n[TAHAP 2] Memuat dan Membersihkan Data...\n")

# Membaca dataset
raw_data <- read.csv("data/Assignment-1_Data.csv", sep = ";", dec = ",", stringsAsFactors = FALSE)

# Pembersihan Data:
# 1. Menghapus transaksi tanpa Itemname atau BillNo
# 2. Menghapus Quantity <= 0 (transaksi batal/return)
# 3. Filter produk non-retail (seperti POSTAGE, Manual)
clean_data <- raw_data %>%
  filter(!is.na(Itemname), Itemname != "", Quantity > 0, !is.na(BillNo)) %>%
  filter(!str_detect(Itemname, "POSTAGE|DOTCOM POSTAGE|Manual|Adjust bad debt"))

# Konversi Tanggal
clean_data$Date <- dmy_hm(clean_data$Date)

# ------------------------------------------------------------------------------
# TAHAP 3: EXPLORATORY DATA ANALYSIS (EDA) & VISUALISASI
# ------------------------------------------------------------------------------
cat("\n[TAHAP 3] Melakukan EDA...\n")

# Visualisasi 1: Top 10 Most Sold Items
top_10_plot <- clean_data %>%
  group_by(Itemname) %>%
  summarise(Count = n()) %>%
  arrange(desc(Count)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(Itemname, Count), y = Count, fill = Count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_gradient(low = "skyblue", high = "darkblue") +
  labs(title = "Top 10 Most Sold Items", 
       subtitle = "Online Retail Dataset",
       x = "Product Name", 
       y = "Frequency") +
  theme_minimal() +
  theme(legend.position = "none")

# Simpan ke folder output
ggsave(file.path(output_dir, "01_plot_top_items.png"), top_10_plot, width = 10, height = 6)

# ------------------------------------------------------------------------------
# TAHAP 4: TRANSFORMASI DATA KE FORMAT TRANSAKSI
# ------------------------------------------------------------------------------
cat("\n[TAHAP 4] Transformasi Data ke Format Market Basket...\n")

transaction_list <- split(clean_data$Itemname, clean_data$BillNo)
trans <- as(transaction_list, "transactions")

# Tampilkan ringkasan di konsol
print(summary(trans))

# Visualisasi 2: Item Frequency Plot
png(file.path(output_dir, "02_plot_item_frequency.png"), width = 800, height = 600)
itemFrequencyPlot(trans, topN = 20, type = "relative", 
                  col = "steelblue", main = "Relative Item Frequency (Top 20)")
dev.off()

# ------------------------------------------------------------------------------
# TAHAP 5: IMPLEMENTASI ALGORITMA APRIORI
# ------------------------------------------------------------------------------
cat("\n[TAHAP 5] Menjalankan Algoritma Apriori...\n")

# Menjalankan Apriori (Support 1%, Confidence 80%)
rules <- apriori(trans, parameter = list(supp = 0.01, conf = 0.8, target = "rules"))

# Menghapus aturan redundant
rules_pruned <- rules[!is.redundant(rules)]

cat("Jumlah aturan akhir setelah pembersihan:", length(rules_pruned), "\n")

# ------------------------------------------------------------------------------
# TAHAP 6: ANALISIS & VISUALISASI ATURAN ASOSIASI
# ------------------------------------------------------------------------------
cat("\n[TAHAP 6] Analisis Hasil Rules...\n")

top_10_rules <- sort(rules_pruned, by = "lift", decreasing = TRUE)
inspect(head(top_10_rules, 10))

# Visualisasi 3: Scatter Plot
plot_scatter <- plot(rules_pruned, method = "scatterplot", engine = "ggplot2") +
  labs(title = "Scatter Plot Aturan Asosiasi")
ggsave(file.path(output_dir, "03_plot_rules_scatter.png"), plot_scatter, width = 8, height = 6)

# Visualisasi 4: Graph Plot (Top 10 Rules)
png(file.path(output_dir, "04_plot_rules_graph.png"), width = 1000, height = 800)
plot(head(top_10_rules, 10), method = "graph", 
     main = "Graph Visualization of Top 10 Rules")
dev.off()

# ------------------------------------------------------------------------------
# TAHAP 7: KESIMPULAN & OUTPUT AKHIR
# ------------------------------------------------------------------------------
cat("\n[TAHAP 7] Menyimpan Hasil ke CSV...\n")

# Menyimpan hasil rules ke CSV di folder output
write(rules_pruned, file = file.path(output_dir, "hasil_asosiasi_rules.csv"), 
      sep = ";", quote = TRUE, row.names = FALSE)

cat("========================================================\n")
cat(" ANALISIS SELESAI\n")
cat(" Semua hasil tersimpan di folder: ", output_dir, "\n")
cat("========================================================\n")
