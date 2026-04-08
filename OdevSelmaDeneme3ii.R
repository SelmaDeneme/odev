VERI MADENCILIGI - ODEV-3ii: KESIFSEL VERI ANALIZI (EDA)
# Hazirlayan: Selma Deneme Gencoglu
# Konu: Gercek Veri Seti (Swiss) Uzerinde Cok Yonlu Analiz
# =============================================================

# --- 0. HAZIRLIK ASAMASI ---
# Analiz icin gerekli olan istatistiksel paketlerin yuklenmesi ve cagrilmasi.
# 'car' paketi ozellikle VIF (Coklu Dogrusallik) analizi icin gereklidir.
if(!require(car)) install.packages("car") 
library(car)

# -------------------------------------------------------------
# BOLUM2: GERCEK VERI SETI ANALIZI (SWISS DATASET)
# -------------------------------------------------------------

# --- ADIM 1: Veri Kaynagi ve Isimlendirme ---
# R sisteminde yerlesik olan 'swiss' (Isvicre Sosyo-Ekonomik Verileri) kullanilmistir.
# Analiz icin 'Education' (Egitim) ve 'Examination' (Sinav) degiskenleri secilmistir.
data("swiss")
df_gercek <- swiss[, c("Education", "Examination")]
colnames(df_gercek) <- c("Egitim", "Sinav")

# Veri setinin ilk bakista anlasilmasi icin merkezi egilim olculeri raporlanmistir.
print("--- ADIM 1: VERI SETI BETIMSEL OZETI ---")
print(summary(df_gercek))

# --- ADIM 2: Veri Standartlastirma (Z-Score Transformation) ---
# Farkli birimlerdeki verileri ayni olcege getirmek icin Z-Skoru donusumu yapilmistir.
# Boylece verilerin ortalamasi 0, standart sapmasi 1 yapilarak karsilastirma kolaylasmistir.
df_gercek_scaled <- as.data.frame(scale(df_gercek)) 
print("--- ADIM 2: Z-SKORU ILE STANDARTLASTIRILMIS VERI (ILK 5 SATIR) ---")
print(head(df_gercek_scaled, 5))

# --- ADIM 3: Veri Gorsellestirme (Gorsel EDA) ---
# Dortlu panel duzeni ile verinin dagilimi, aykiri degerleri ve normalligi incelenmistir.
par(mfrow=c(2,2))

# 1. Histogram: Verinin hangi araliklarda yogunlastigini gosterir.
hist(df_gercek$Egitim, main="Egitim Dagilim Grafigi", col="skyblue", xlab="Egitim")

# 2. Boxplot: Verideki yayilimi ve merkezi egilimi gosterir.
boxplot(df_gercek$Egitim, main="Egitim Aykiri Deger Analizi", col="tomato", horizontal=TRUE)

# 3. Scatter Plot: Egitim ve Sinav degiskenleri arasindaki iliskinin yonunu gosterir.
plot(df_gercek$Egitim, df_gercek$Sinav, main="Egitim vs Sinav Iliskisi", 
     col="darkblue", pch=19, xlab="Egitim Duzeyi", ylab="Sinav Basarisi")

# 4. Q-Q Plot: Verinin normal dagilima ne kadar yakin oldugunu test eder.
qqnorm(df_gercek$Egitim, main="Normal Dagilim Kontrolu (Q-Q)")
qqline(df_gercek$Egitim, col="red", lwd=2)

par(mfrow=c(1,1)) # Panel duzenini sifirlama

# --- ADIM 4: Korelasyon ve Iliski Analizi ---
# Degiskenler arasindaki dogrusal baglantinin guce (r degeri) bakilmistir.
# P-degerinin 0.05'ten kucuk olmasi iliskinin tesadufi olmadigini kanitlar.
korelasyon_testi <- cor.test(df_gercek$Egitim, df_gercek$Sinav)
print("--- ADIM 4: KORELASYON ANALIZI SONUCLARI ---")
print(korelasyon_testi)

# --- ADIM 5: Aykiri Deger Analizi (IQR Yontemi) ---
# Veri setindeki uc (aykiri) degerleri saptamak icin Ceyrekler Arasi Aralik (IQR) kullanilmistir.
# Bu yontemle verinin 'normal' kabul edilen alt ve ust sinirlari hesaplanmistir.
Q1_v <- quantile(df_gercek$Egitim, 0.25)
Q3_v <- quantile(df_gercek$Egitim, 0.75)
IQR_v <- Q3_v - Q1_v
ust_limit <- Q3_v + 1.5 * IQR_v
alt_limit <- Q1_v - 1.5 * IQR_v

print(paste("Egitim Degiskeni Icin Ust Limit:", round(ust_limit, 2)))
print(paste("Egitim Degiskeni Icin Alt Limit:", round(alt_limit, 2)))

# --- ADIM 6: Hipotez Testi ve Regresyon Modelleme ---
# H0: Egitim seviyesinin sinav basarisi uzerinde etkisi yoktur.
# H1: Egitim seviyesi arttikca sinav basarisi anlamli sekilde artar.
model_final <- lm(Sinav ~ Egitim, data=df_gercek)
print("--- ADIM 6: REGRESYON MODELI VE HIPOTEZ TESTI ---")
print(summary(model_final))

# --- ADIM 7: Coklu Dogrusallik (Collinearity - VIF) Analizi ---
# Bagimsiz degiskenlerin birbirini yuksek oranda aciklamasi (multicollinearity) riskine bakilmistir.
# VIF degerinin 5 veya 10'dan kucuk olmasi modelin guvenilir oldugunu gosterir.
# Modelde Fertility degiskeni; Education, Examination ve Agriculture ile test edilmistir.
vif_degerleri <- vif(lm(Fertility ~ Education + Examination + Agriculture, data=swiss))
print("--- ADIM 7: COKLU DOGRUSALLIK (VIF) ANALIZI ---")
print(vif_degerleri)

