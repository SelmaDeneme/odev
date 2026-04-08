=============================================================
  # ODEV VERI MADENCILIGI - UYGULAMA ODEVI
  # Hazirlayan: Selma Deneme Gencoglu
  # Not: Temel fonksiyonlar ve veri yapilari uzerinden kurgulanmistir.
  # =============================================================

# --- 0. HAZIRLIK: KUTUPHANE YUKLENMESI ---
# Sistemde paket kontrolü ve yükleme islemleri
if(!require(zoo)) install.packages("zoo")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(car)) install.packages("car")

library(zoo)
library(ggplot2)
library(car)

# --- 1. TEMEL MATEMATIKSEL FONKSIYONLAR ---
# Degisken tanimlama ve aritmetik operasyonlar
vize_notu <- 70
final_notu <- 85
basari_ortalamasi <- (vize_notu * 0.4) + (final_notu * 0.6)

print(paste("Hesaplanan Donem Sonu Basari Notu:", basari_ortalamasi))

# --- 2. VEKTOR YAPILARI VE ISTATISTIKSEL ANALIZ ---
# Veri dizileri uzerinde merkezi egilim olculeri
puan_listesi <- c(65, 80, 45, 95, 100, 75, 88)

print(paste("Grup Ortalamasi:", mean(puan_listesi)))
print(paste("En Yuksek Deger:", max(puan_listesi)))
print(paste("Standart Sapma:", sd(puan_listesi)))

# --- 3. VERI CERCEVESI (DATA FRAME) OLUSTURMA ---
# Farkli veri tiplerini (Karakter, Sayisal, Mantiksal) birlestirme
ogrenci_isimleri <- c("Ogrenci_1", "Ogrenci_2", "Ogrenci_3", "Ogrenci_4")
gunluk_calisma <- c(5, 8, 3, 10)
yeterli_mi <- gunluk_calisma >= 6  # Mantiksal sorgulama

df_odev2 <- data.frame(
  Isim = ogrenci_isimleri,
  Calisma_Saati = gunluk_calisma,
  Durum = yeterli_mi
)

# --- 4. VERI YAPISI INCELEME FONKSIYONLARI ---
# Verinin icerigini ve tipini sorgulama
print("--- Veri Seti Genel Ozeti ---")
summary(df_odev2)

print("--- Veri Seti Teknik Yapisi ---")
str(df_odev2)

# --- 5. TEMEL GORSELLESTIRME ---
# Verinin dagilimini gosteren bar grafigi
barplot(df_odev2$Calisma_Saati, 
        names.arg = df_odev2$Isim, 
        col = "steelblue", 
        main = "Ogrencilerin Gunluk Calisma Saatleri",
        ylab = "Saat",
        xlab = "Ogrenciler")

