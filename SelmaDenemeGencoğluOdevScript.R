 VERİ MADENCİLİGİ - BİRLESTİRİLMİS UYGULAMA ODEVİ 
# Hazırlayan: Selma Deneme Gencoglu
# Not: EDA Sureci 5 Temel Adim Uzerinden Yapilandirilmistir.
# ==============================================================================

# 0. HAZİRLİK: KUTUPHANE YUKLENMESİ
if(!require(zoo)) install.packages("zoo")
if(!require(ggplot2)) install.packages("ggplot2")
library(zoo)
library(ggplot2)

# ==============================================================================
# BÖLÜM 1: SİMULASYON VERİSİ ANALİZİ (ODEV I)
# ==============================================================================

# --- ADİM 1: Veri Ozetleme (Summary Statistics) ---
# Simulasyon verisi uretimi ve merkezi egilim ölcülerinin analizi.
set.seed(123)
n <- 100
calisma_saati    <- rnorm(n, 10, 2)
materyal_destegi <- rnorm(n, 5, 1)
motivasyon       <- rnorm(n, 7, 1.5)
# Bagimli degisken uretimi
basari_puani <- 5 + (1.5 * calisma_saati) + (0.8 * motivasyon) + rnorm(n, 0, 2)

df1 <- data.frame(Calisma=calisma_saati, Motivasyon=motivasyon, Basari=basari_puani)
print("--- ODEV 1: VERİ OZETİ ---")
print(summary(df1)) # Ortalama, medyan ve ceyreklikler

# --- ADİM 2: Veri Görsellestirme ---
# Tek ve iki degişkenli analizler (Histogram, Boxplot ve Saçılım).
par(mfrow=c(2,2))
hist(df1$Basari, main="Histogram: Başarı Dağılımı", col="skyblue")
boxplot(df1$Basari, main="Boxplot: Aykırı Değer", col="skyblue", horizontal=TRUE)
plot(df1$Calisma, df1$Basari, main="Saçılım: Çalışma vs Başarı", col="blue", pch=19)
qqnorm(df1$Basari, main="Q-Q Plot: Normallik Testi")
qqline(df1$Basari, col="red")
par(mfrow=c(1,1))

# --- ADİM 3: Korelasyon ve İliski Analizi ---
# Pearson korelasyon katsayisi ile degiskenler arasi iliskinin gucu.
korelasyon_df1 <- cor(df1$Calisma, df1$Basari)
print(paste("Calisma ve Basari Arasindaki Korelasyon:", round(korelasyon_df1, 4)))

# --- ADİM 4: Aykiri ve Eksik Veri Analizi ---
# Eksik verilerin (NA) tespiti ve LOCF yontemiyle yonetimi.
df1$Basari[c(10, 25, 50)] <- NA # Yapay eksik veri olusturma
df1$Basari <- na.locf(df1$Basari) # Son gozlemi ileri tasima
print("Eksik veriler LOCF yontemiyle tamamlanmistir.")

# --- ADİM 5: Hipotez Olusturma ve Test Etme ---
# H1: Calisma saati basari puanini anlamli duzeyde yordar.
model_sim <- lm(Basari ~ Calisma, data=df1)
print(summary(model_sim)) # p-value < 0.05 ise hipotez dogrulanir

# ==============================================================================
# BOLUM 2: GERCEK VERİ SETİ ANALİZİ (ODEV II - SWISS)
# ==============================================================================

# --- ADİM 1: Veri Ozetleme (Summary Statistics) ---
data("swiss")
df2 <- swiss[, c("Education", "Examination")]
colnames(df2) <- c("Egitim", "Sinav")
print("--- ODEV 2: GERCEK VERİ OZETİ ---")
print(summary(df2))

# --- ADİM 2: Veri Görsellestirme ---
# İleri veri on isleme: Standardizasyon (Z-Score)
df2_scaled <- as.data.frame(scale(df2)) # Veri donusturme

par(mfrow=c(2,2))
hist(df2$Egitim, main="Egitim Dagilimi", col="orange")
boxplot(df2$Egitim, main="Eğitim Aykiri Degerler", col="orange", horizontal=TRUE)
plot(df2$Egitim, df2$Sinav, main="Eğitim vs Sınav İliskisi", col="darkgreen", pch=18)
qqnorm(df2$Egitim, main="Q-Q Plot: Gercek Veri")
qqline(df2$Egitim, col="red")
par(mfrow=c(1,1))

# --- ADİM 3: Korelasyon ve İliski Analizi ---
korelasyon_swiss <- cor.test(df2$Egitim, df2$Sinav)
print(korelasyon_swiss)

# --- ADİM 4: Aykiri ve Eksik Veri Analizi (IQR Yontemi) ---
# Matematiksel sınırların (Üst Sınır: 21) hesaplanması.
Q1 <- quantile(df2$Egitim, 0.25)
Q3 <- quantile(df2$Egitim, 0.75)
IQR_degeri <- Q3 - Q1
ust_sinir <- Q3 + 1.5 * IQR_degeri
print(paste("Egitim Degiskeni İçin Ust Sinir Esigi:", ust_sinir))

# --- ADİM 5: Hipotez Olusturma ve Test Etme ---
# H1: Kantonlardaki egitim seviyesi sinav basarisini istatistiksel olarak etkiler.
model_swiss <- lm(Sinav ~ Egitim, data=df2)
print(summary(model_swiss)) # Anlamlilik duzeyi kontrolu



