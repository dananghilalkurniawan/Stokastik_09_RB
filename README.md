# Code_09_RB
Analisis Rantai Markov ini mengkaji pola perpindahan merek smartphone pada 130 mahasiswa Sains Data ITERA. Data survei digunakan untuk membentuk matriks transisi, probabilitas langkah ke-2, serta distribusi stasioner. Hasil menunjukkan Samsung, iPhone, dan Xiaomi sebagai merek dominan dan paling stabil dalam preferensi jangka panjang.

# 📊 Pemodelan Stokastik Rantai Markov
### Pergantian Merek HP Mahasiswa Sains Data ITERA (130 Responden)

---

## 1. Persiapan Lingkungan & Import Data
```r
library(readxl)
library(dplyr)
library(stringr)
library(igraph)
library(ggraph)
library(ggplot2)

# Import dataset
df <- read_excel("C:/Users/HP/Downloads/Studi_Rantai_Markov_130_Responden_SetelahPenambahan.xlsx")
```

---

## 2. Pembersihan Data & Standarisasi Merek HP
```r
df <- df %>%
  mutate(
    `Merk HP Saat Ini`   = str_to_title(str_trim(`Merk HP Saat Ini`)),
    `Merk HP Sebelumnya` = str_to_title(str_trim(`Merk HP Sebelumnya`))
  )

df <- df %>%
  mutate(
    `Merk HP Sebelumnya` = ifelse(
      str_detect(`Merk HP Sebelumnya`, "Belum"),
      NA,
      `Merk HP Sebelumnya`
    )
  )

brands <- c("Samsung", "Xiaomi", "Oppo", "Vivo",
            "Realme", "Iphone", "Infinix", "Huawei")

df <- df %>%
  mutate(
    `Merk HP Saat Ini`   = ifelse(`Merk HP Saat Ini` %in% brands, `Merk HP Saat Ini`, "Others"),
    `Merk HP Sebelumnya` = ifelse(`Merk HP Sebelumnya` %in% brands, `Merk HP Sebelumnya`, "Others")
  )

df_non_na <- df %>% filter(!is.na(`Merk HP Sebelumnya`))
```

---

## 3. Penyusunan Matriks Transisi Rantai Markov
```r
contingency <- table(df_non_na$`Merk HP Sebelumnya`,
                     df_non_na$`Merk HP Saat Ini`) %>%
  as.matrix()

contingency <- contingency[brands, brands, drop = FALSE]
contingency[is.na(contingency)] <- 0

row_sums <- rowSums(contingency)
row_sums[row_sums == 0] <- NA

P <- contingency / row_sums
P[is.na(P)] <- 0

cat("Counts:\n")
print(contingency)

cat("\nTransition matrix P (rows sum to 1):\n")
print(round(P, 4))
```

---

## 4. Visualisasi Diagram Transisi Rantai Markov
```r
edges <- data.frame(
  from   = rep(brands, each = length(brands)),
  to     = rep(brands, times = length(brands)),
  weight = as.vector(P)
) %>%
  filter(weight > 0)

g <- graph_from_data_frame(edges, directed = TRUE,
                           vertices = data.frame(name = brands))

ggraph(g, layout = "fr") +
  geom_edge_link(aes(width = weight),
                 arrow = arrow(type = "closed", length = unit(3, "mm")),
                 end_cap = circle(3, "mm"),
                 alpha = 0.7) +
  geom_node_point(size = 12, color = "lightblue") +
  geom_node_text(aes(label = name), size = 4, vjust = 0.5) +
  scale_edge_width(range = c(0.2, 2)) +
  ggtitle("Diagram Transisi Rantai Markov Pergantian Merek HP") +
  theme_void()
```

---

## 5. Probabilitas Langkah Ke-2 & Distribusi Stasioner
```r
n <- 2
P_n <- P
for (i in 1:(n - 1)) {
  P_n <- P_n %*% P
}

cat(paste0("Probabilitas langkah ke-", n, ":\n"))
print(round(P_n, 4))

eig <- eigen(t(P))
idx <- which(abs(eig$values - 1) < 1e-8)

stationary_dist <- Re(eig$vectors[, idx])
stationary_dist <- stationary_dist / sum(stationary_dist)

cat("Distribusi Stasioner:\n")
print(round(stationary_dist, 4))

sorted_states <- sort(setNames(as.numeric(stationary_dist), brands), decreasing = TRUE)

cat("Klasifikasi ruang keadaan berdasarkan distribusi stasioner:\n")
for (i in seq_along(sorted_states)) {
  cat(names(sorted_states)[i], ": ",
      sprintf("%.4f", sorted_states[i]), "\n", sep = "")
}
```

---

## 6. Visualisasi Distribusi Merek HP
```r
ggplot(df, aes(x = `Merk HP Sebelumnya`)) +
  geom_bar(fill = "steelblue") +
  labs(title = "Distribusi Merek HP Sebelum",
       x = "Merek HP", y = "Jumlah Mahasiswa") +
  theme_minimal()

ggplot(df, aes(x = `Merk HP Saat Ini`)) +
  geom_bar(fill = "darkorange") +
  labs(title = "Distribusi Merek HP Saat Ini",
       x = "Merek HP", y = "Jumlah Mahasiswa") +
  theme_minimal()
```

---

## 7. Ringkasan Jumlah & Persentase
```r
df %>% count(`Merk HP Sebelumnya`, name = "Jumlah")

df %>% 
  count(`Merk HP Sebelumnya`, name = "Jumlah") %>%
  mutate(Persentase = round(Jumlah / sum(Jumlah) * 100, 2))
```

---

## 8. Statistik Deskriptif
```r
summary(df_non_na)
```

---

## 9. Statistik Frekuensi Merek
```r
df_non_na %>% count(`Merk HP Saat Ini`) %>% arrange(desc(n))
df_non_na %>% count(`Merk HP Sebelumnya`) %>% arrange(desc(n))
```
