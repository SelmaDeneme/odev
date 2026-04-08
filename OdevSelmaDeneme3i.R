ODEV VERI MADENCILIGI - UYGULAMA ODEVI 
# Hazırlayan: Selma Deneme Gencoglu
# Not: EDA Sureci 5 Temel Adim Uzerinden Yapilandirilmistir.
# ==============================================================================

# 0. HAZİRLİK: KUTUPHANE YUKLENMESİ
if(!require(zoo)) install.packages("zoo")
if(!require(ggplot2)) install.packages("ggplot2")
library(zoo)
library(ggplot2)
library(car)

# BOLUM 1: VERI URETIMI VE TEMEL FONKSIYONLAR (ODEV-3i)
# -------------------------------------------------------------

# 1.1. Rastgele Veri Uretimi (Simulation)
set.seed(2026)

# Bagimsiz Degiskenler: Calisma Saati ve Aylik Okunan Kitap Sayisi
calisma_saati <- rnorm(n = 100, mean = 15, sd = 4)   # Haftalik calisma
kitap_sayisi <- rpois(n = 100, lambda = 4)          # Aylik okunan kitap (Ortalama 4)

# Bagimli Degisken: Basari Puani
# (Her iki degisken de basariyi pozitif etkiler)
basari_puani <- 40 + (1.4 * calisma_saati) + (2.1 * kitap_sayisi) + rnorm(100, sd = 5)

# 1.2. Veri Cercevesi (Data Frame) Olusturma
veriseti_1 <- data.frame(ID = 1:100, Calisma = calisma_saati, Kitap = kitap_sayisi, Basari = basari_puani)

print("--- BOLUM 1: SENTETIK VERI OZETI ---")
print(summary(veriseti_1))

# 1.3. Eksik ve Aykiri Veri Analizi
print(paste("Toplam Eksik Veri Sayisi:", sum(is.na(veriseti_1))))

# Aykiri Deger Analizi - IQR Yontemi
Q1_s <- quantile(veriseti_1$Basari, 0.25)
Q3_s <- quantile(veriseti_1$Basari, 0.75)
IQR_s <- Q3_s - Q1_s
alt_sinir_s <- Q1_s - 1.5 * IQR_s
ust_sinir_s <- Q3_s + 1.5 * IQR_s

print(paste("Basari Degiskeni Icin Alt Sinir:", round(alt_sinir_s, 2)))
print(paste("Basari Degiskeni Icin Ust Sinir:", round(ust_sinir_s, 2)))

# 1.4. Gorsellestirme (EDA)
par(mfrow=c(2,2))
hist(veriseti_1$Basari, main="Basari Puani Dagilimi", col="lightblue", border="white", xlab="Puan")
boxplot(veriseti_1$Basari, main="Basari Aykiri Deger Kontrolu", col="lightblue", horizontal=TRUE)
plot(veriseti_1$Calisma, veriseti_1$Basari, main="Calisma vs Basari", pch=16, col="red", xlab="Calisma Saati", ylab="Basari")
plot(veriseti_1$Kitap, veriseti_1$Basari, main="Kitap Sayisi vs Basari", pch=18, col="darkgreen", xlab="Kitap Sayisi", ylab="Basari")
par(mfrow=c(1,1))

# 1.5. Hipotez Testi ve Collinearity (Dogrusallik) Kontrolu
# H1: Calisma saati ve kitap okuma sayisi basariyi anlamli etkiler.
model_odev1 <- lm(Basari ~ Calisma + Kitap, data = veriseti_1)
print("--- BOLUM 1: HIPOTEZ TESTI SONUCLARI ---")
print(summary(model_odev1))

print("--- BOLUM 1: COKLU DOGRUSALLIK (VIF) ANALIZI ---")
print(vif(model_odev1))
