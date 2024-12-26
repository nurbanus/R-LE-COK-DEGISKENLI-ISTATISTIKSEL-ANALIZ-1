cokdegiskenli2
handenurbanuş
2024-11-27
Sys.setlocale("LC_CTYPE", "tr_TR.UTF-8")
## [1] "tr_TR.UTF-8"
Türkçe karakterlere izin vermesi için öncelikle bu ayarlamayı yapalım.

##soru1:Aşağıda verilen matrislerin pozitif tanımlı olup olmadıklarını R programında belirleyiniz.
A=⎡⎣⎢543−432322⎤⎦⎥,B=⎡⎣⎢4−31−342123⎤⎦⎥,C=⎡⎣⎢321263131⎤⎦⎥,D=⎡⎣⎢331342122⎤⎦⎥

# Matrislerin tanımlanması
A <- matrix(c(5, 4, 3, -4, 3, 2, 3, 2, 2), nrow = 3, byrow = TRUE)
B <- matrix(c(4, -3, 1, -3, 4, 2, 1, 2, 3), nrow = 3, byrow = TRUE)
C <- matrix(c(3, 2, 1, 2, 6, 3, 1, 3, 1), nrow = 3, byrow = TRUE)
D <- matrix(c(3, 3, 1, 3, 4, 2, 1, 2, 2), nrow = 3, byrow = TRUE)
# Pozitif tanımlılığı kontrol eden fonksiyon
pozitif_func <- function(matrix) {
  # Simetrik mi kontrolü
  if (!all(matrix == t(matrix))) {
    return(FALSE)
  }
  # Özdeğerlerin pozitif olup olmadığını kontrol et
  eigenvalues <- eigen(matrix)$values
  return(all(eigenvalues > 0))
}
# Kontrollerin yapılması
sonuc_A <- pozitif_func(A)
sonuc_B <- pozitif_func(B)
sonuc_C <- pozitif_func(C)
sonuc_D <- pozitif_func(D)
print(sonuc_A)
## [1] FALSE
print(sonuc_B)
## [1] FALSE
print(sonuc_C)
## [1] FALSE
print(sonuc_D)
## [1] TRUE
-Pozitif Tanımlı (TRUE): Matrisin tüm özdeğerleri pozitif ve matris simetriktir. Bu tür matrisler genellikle enerji, varyans-kovaryans gibi fiziksel veya istatistiksel uygulamalarda kararlılığı temsil eder.

-Pozitif Tanımlı Değil (FALSE): Matris pozitif tanımlı değilse:

-Özdeğerlerinden biri sıfırsa, matris pozitif yarı tanımlı olabilir. Negatif özdeğer varsa, matris tanımlı değil olabilir. Karmaşık özdeğerler varsa, matris pozitif tanımlı olamaz.

-Sonuçlara baktığımızda D matrisinin pozitif tanımlı olduğunu görüyoruz.

2.soru:
f(x)=ke−12Q
 eşitliğinde Q=x21+5x22+2x23+6x2x3−4x1−8x2−6x3+9
 olarak verilmiştir. f(x) yoğunluk fonksiyonunun yayılım parametresini ve ortalama vektörünü R programında bulunuz. En yüksek yayılıma ve en düşük ortalamaya sahip olan değişkenleri belirtiniz.

 # Kovaryans matrisi Q'nun tanımlanması
# Kovaryans matrisi Q
Q <- matrix(c(1, 0, 0,
              0, 5, 6,
              0, 6, 2), nrow = 3, byrow = TRUE)

Q
##      [,1] [,2] [,3]
## [1,]    1    0    0
## [2,]    0    5    6
## [3,]    0    6    2
# Ortalama vektörü (x1, x2, x3 için sırasıyla)
ortalama_vektor <- c(2, 4, 3)

# Değişken isimleri
degisken_isimleri <- c("x1", "x2", "x3")

 # Varyansların hesaplanması (Kovaryans matrisinin diyagonal elemanları)
varyanslar <- diag(Q)

# En yüksek varyansa sahip değişkenin bulunması
maks_varyans_degiskeni <- degisken_isimleri[which.max(varyanslar)]

# Ortalama vektörü zaten sıfır olduğu için, en düşük ortalamalı değişken
# (hepsi eşit, birinciyi seçiyoruz)
min_ortalama_degiskeni <- degisken_isimleri[which.min(ortalama_vektor)]

# Sonuçların gösterilmesi
list(
  Varyanslar = varyanslar,
  Ortalama_Vektoru = ortalama_vektor,
  Maks_Varyans_Degiskeni = maks_varyans_degiskeni,
  Min_Ortalama_Degiskeni = min_ortalama_degiskeni
)
## $Varyanslar
## [1] 1 5 2
## 
## $Ortalama_Vektoru
## [1] 2 4 3
## 
## $Maks_Varyans_Degiskeni
## [1] "x2"
## 
## $Min_Ortalama_Degiskeni
## [1] "x1"
X1’in varyansı: 1

X2’nin varyansı: 5

X3’ün varyansı: 2

Köşegen dışındaki elemanlar değişkenler arasındaki kovaryansları gösterir:

X2 ve X3 arasındaki kovaryans: 6

Diğer kovaryanslar sıfırdır, yani değişkenler arasındaki ilişkiler yoktur veya bağımsızdır.

ORTALAMA VEKTÖRÜ:

X1’in ortalaması: 2

X2’nin ortalaması: 4

X3’ün ortalaması: 3

-diag(Q) ile varyansları alınır.

Varyanslar: X1 = 1, X2 = 5, X3 = 2

En yüksek varyans: X2 (5)

En yüksek yayılıma sahip değişken: x2

x2 değişkeni, diğerlerine kıyasla daha fazla yayılım göstermekte. Bu, X2’nin değerlerinin daha geniş bir aralıkta dağıldığını ifade eder.

Ortalama Değerler: X1 = 2, X2 = 4, X3 =3

En düşük ortalama: x1

En düşük ortalamaya sahip değişken: x1

x1 değişkeni, diğerlerine kıyasla en düşük ortalamaya sahiptir. Bu, X2’nin yoğunluk fonksiyonunda merkezi eğilim değerinin diğer değişkenlerden daha küçük olduğunu gösterir.

soru3:
Bir biyoloji araştırmasında, üç farklı gübre türünün (Organik, Kimyasal, Kontrol) bitki büyümesi üzerindeki etkisi incelenmektedir. Araştırmacı, Bitki Boyu, Yaprak Alanı ve Biyokütle ölçümlerini kullanarak her bir gruptaki 30 bitkiyi incelemiştir. Gübre türleri arasında etkinlik açısından fark olup olmadığını sistemde kayıtlı gübre veri setini kullanarak R programında %95 güven düzeyinde inceleyerek yorumlayınız.

library(readxl)
# Excel dosyasını yükleme
veri <- read_excel("C:/Users/HANDENUR/Downloads/gubre.xlsx")
head(veri)
## # A tibble: 6 × 4
##   Grup    `Bitki Boyu (cm)` `Yaprak Alanı (cm²)` `Biyokütle (g)`
##   <chr>               <dbl>                <dbl>           <dbl>
## 1 Organik              45.3                 200.            15.8
## 2 Organik              44.9                 199.            15.5
## 3 Organik              46                   202.            16  
## 4 Organik              45.7                 201.            15.9
## 5 Organik              44.8                 198             15.6
## 6 Organik              46.2                 202.            16.1
Veriler üç bağımlı değişkene (Bitki Boyu, Yaprak Alanı, Biyokütle) ve bir bağımsız değişkene (Gübre Türü: Organik, Kimyasal, Kontrol) sahiptir.MANOVA, bu tip verilerde grupların etkisini çoklu bağımlı değişken üzerinde aynı anda test etmek için uygundur.

#normallik varsayımı
library(readxl)
library(car)
## Zorunlu paket yükleniyor: carData
library(heplots)
## Warning: package 'heplots' was built under R version 4.3.3
## Zorunlu paket yükleniyor: broom
colnames(veri) <- c("Grup", "Bitki_Boyu", "Yaprak_Alani", "Biyokutle")
normallik_testi <- by(veri[, c("Bitki_Boyu", "Yaprak_Alani", "Biyokutle")], veri$Grup, function(group) {
  lapply(group, shapiro.test)
})
print(normallik_testi)
## veri$Grup: Kimyasal
## $Bitki_Boyu
## 
##  Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.95318, p-value = 0.5758
## 
## 
## $Yaprak_Alani
## 
##  Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.95808, p-value = 0.659
## 
## 
## $Biyokutle
## 
##  Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.94347, p-value = 0.428
## 
## 
## ------------------------------------------------------------ 
## veri$Grup: Kontrol
## $Bitki_Boyu
## 
##  Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.96103, p-value = 0.7102
## 
## 
## $Yaprak_Alani
## 
##  Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.95088, p-value = 0.5384
## 
## 
## $Biyokutle
## 
##  Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.92876, p-value = 0.2615
## 
## 
## ------------------------------------------------------------ 
## veri$Grup: Organik
## $Bitki_Boyu
## 
##  Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.91019, p-value = 0.1363
## 
## 
## $Yaprak_Alani
## 
##  Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.92071, p-value = 0.1975
## 
## 
## $Biyokutle
## 
##  Shapiro-Wilk normality test
## 
## data:  X[[i]]
## W = 0.96242, p-value = 0.7343
H0: Veriler normal dağılmıyor. Hs:Veriler normal dağılıyor.

Bütün p değerlerine baktığımızda p >0.05 olduğundan H0 reddedilir. Veriler normallik varsayımını sağlıyor.

#install.packages("heplots")  
library(heplots)
box_m_test <- boxM(
  as.matrix(veri[, c("Bitki_Boyu", "Yaprak_Alani", "Biyokutle")]),
  veri$Grup
)
print(box_m_test)
## 
##  Box's M-test for Homogeneity of Covariance Matrices
## 
## data:  as.matrix(veri[, c("Bitki_Boyu", "Yaprak_Alani", "Biyokutle")])
## Chi-Sq (approx.) = 40.375, df = 12, p-value = 6.229e-05
H0:kovaryans matrislerinin gruplar arasında eşittir.(homojendir) Hs:kovaryans matrislerinin gruplar arasında eşit değildir.(homojen değildir)

p değeri < 0.05 olduğu için H0 red.Homojenlik sağlanmıyor.Wilk in lambda değeri yerine Pillai test istatistiğini kullanırız.

manova_model <- manova(cbind(Bitki_Boyu, Yaprak_Alani, Biyokutle) ~ Grup, data = veri)
summary(manova_model) 
##           Df Pillai approx F num Df den Df    Pr(>F)    
## Grup       2 1.9946   5002.4      6     82 < 2.2e-16 ***
## Residuals 42                                            
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
MANOVA Modeli:

H0: Gübre türlerinin bitki büyümesi üzerindeki çoklu ölçümler üzerinde etkisi yoktur Hs: En az bir bağımlı değişken açısından gruplar arasında fark vardır.

Gruplar arasında istatistiksel olarak anlamlı bir fark var.

p-değeri < 0.001 olduğu için, gübre türlerinin (Organik, Kimyasal, Kontrol) bitki büyümesi üzerindeki etkileri arasında anlamlı bir fark olduğu söylenebilir.

Pillai’s Trace: Bağımlı değişkenlerin (Bitki_Boyu, Yaprak_Alani, Biyokutle) varyansının yaklaşık %199.46’sı grup farklılıklarından kaynaklanmaktadır.

H0 red ise post-hoc testleri yapılır.Tukey testini deneyelim:

anova_summary <- summary.aov(manova_model)
print(anova_summary)
##  Response Bitki_Boyu :
##             Df  Sum Sq Mean Sq F value    Pr(>F)    
## Grup         2 3155.24 1577.62  9846.5 < 2.2e-16 ***
## Residuals   42    6.73    0.16                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##  Response Yaprak_Alani :
##             Df Sum Sq Mean Sq F value    Pr(>F)    
## Grup         2 138430   69215   36221 < 2.2e-16 ***
## Residuals   42     80       2                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
##  Response Biyokutle :
##             Df  Sum Sq Mean Sq F value    Pr(>F)    
## Grup         2 1145.55  572.77   12778 < 2.2e-16 ***
## Residuals   42    1.88    0.04                      
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
p-value (< 2.2e-16): p < 0.001 olduğundan, gübre türleri arasında Bitki Boyu açısından anlamlı bir fark var. Gübre türleri, Bitki Boyu üzerinde anlamlı şekilde farklı etkiler yaratmaktadır.

p-value (< 2.2e-16): p < 0.001 olduğundan, gübre türleri arasında Yaprak Alanı açısından anlamlı bir fark var. Gübre türleri, Yaprak Alanı üzerinde de anlamlı şekilde farklı etkiler yaratmaktadır.

p-value (< 2.2e-16): p < 0.001 olduğundan, gübre türleri arasında Biyokütle açısından anlamlı bir fark var. Gübre türleri, Biyokütle üzerinde de anlamlı şekilde farklı etkiler yaratmaktadır.

Tüm bağımlı değişkenler (Bitki_Boyu, Yaprak_Alani, Biyokutle) için gübre türleri arasında istatistiksel olarak anlamlı farklar bulunmuştur (p < 0.001).

Bu sonuç, gübre türlerinin bitki büyümesi üzerindeki etkilerinin anlamlı olduğunu kanıtlar.

#install.packages("emmeans")
library(emmeans)
## Warning: package 'emmeans' was built under R version 4.3.3
## Welcome to emmeans.
## Caution: You lose important information if you filter this package's results.
## See '? untidy'
posthoc <- emmeans(manova_model, pairwise ~ Grup)
print(posthoc)
## $emmeans
##  Grup     emmean   SE df lower.CL upper.CL
##  Kimyasal 100.25 0.17 42    99.91   100.59
##  Kontrol   46.39 0.17 42    46.05    46.73
##  Organik   87.14 0.17 42    86.79    87.48
## 
## Results are averaged over the levels of: rep.meas 
## Confidence level used: 0.95 
## 
## $contrasts
##  contrast           estimate   SE df  t.ratio p.value
##  Kimyasal - Kontrol     53.9 0.24 42  224.038  <.0001
##  Kimyasal - Organik     13.1 0.24 42   54.538  <.0001
##  Kontrol - Organik     -40.7 0.24 42 -169.501  <.0001
## 
## Results are averaged over the levels of: rep.meas 
## P value adjustment: tukey method for comparing a family of 3 estimates
Kimyasal - Kontrol: p-value < 0.0001: Fark anlamlı .Kimyasal gübre, Kontrol grubundan anlamlı şekilde daha yüksek bir etkiye sahiptir.

Kimyasal - Organik:p-value < 0.0001: Fark anlamlı. Kimyasal gübre, Organik gübreden anlamlı şekilde daha yüksek bir etkiye sahiptir.

Kontrol - Organik:p-value < 0.0001: Fark anlamlı. Organik gübre, Kontrol grubundan anlamlı şekilde daha yüksek bir etkiye sahiptir.

Kimyasal gübre, her üç bağımlı değişken üzerinde en yüksek pozitif etkiye sahiptir.

Organik gübre, Kimyasal gübreye göre daha az etkilidir, ancak Kontrol grubuna göre önemli ölçüde daha etkilidir.

Kontrol grubu, diğer gübre türlerine göre anlamlı şekilde daha düşük bir etkiye sahiptir.

Kimyasal gübre, bitki büyümesi üzerinde en etkili yöntemdir.

SORU-4
Bir tekstil mağazasının yöneticisi, 10 farklı dönemde “klasik mavi” kazak satışlarını incelemiş ve satılan kazak sayısını (X1), fiyat değişimini (X2, Euro cinsinden), yerel gazetelerdeki reklam maliyetlerini X3, Euro cinsinden) ve satış asistanının varlığını (X4, dönem başına saat olarak) gözlemlemiştir. Dönemler boyunca aşağıdaki veri matrisini kaydetmiş ve fiyat değişimlerinin, satılan kazak sayısı üzerinde büyük bir etkisi olması gerektiğine ikna olmuştur. Sizce mağaza yöneticisi bu görüşünde haklı mıdır? Haklıysa ya da haklı değilse neden? Cevabınızı R programındaki bulgularınızla birlikte destekleyiniz. (3p)

X=⎡⎣⎢⎢⎢23012520010918199551071659710598150115857197120082192100150103181908511118995120931721251108617012513078⎤⎦⎥⎥⎥

# Veriyi düzenleme
veri <- data.frame(
  X1 = c(230, 181, 165, 150, 97, 192, 181, 189, 172, 170), # Satış (bağımlı değişken)
  X2 = c(125, 99, 97, 115, 120, 100, 90, 95, 125, 125),    # Fiyat değişimi
  X3 = c(200, 55, 105, 85, 0, 150, 85, 120, 110, 130),     # Reklam maliyeti
  X4 = c(109, 107, 98, 71, 82, 103, 111, 93, 86, 78)       # Satış asistanının varlığı
)

# Çoklu regresyon modeli
model <- lm(X1 ~ X2 + X3 + X4, data = veri)

# Model özetini inceleme
summary(model)
## 
## Call:
## lm(formula = X1 ~ X2 + X3 + X4, data = veri)
## 
## Residuals:
##      Min       1Q   Median       3Q      Max 
## -13.7271  -9.3177   0.6364   6.8077  18.7494 
## 
## Coefficients:
##             Estimate Std. Error t value Pr(>|t|)   
## (Intercept) 56.81125   70.72736   0.803  0.45247   
## X2          -0.15027    0.39821  -0.377  0.71889   
## X3           0.48059    0.09185   5.232  0.00195 **
## X4           0.87742    0.41811   2.099  0.08064 . 
## ---
## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
## 
## Residual standard error: 13.01 on 6 degrees of freedom
## Multiple R-squared:  0.9021, Adjusted R-squared:  0.8531 
## F-statistic: 18.42 on 3 and 6 DF,  p-value: 0.001979
H0:model anlamsızdır. p-value = 0.001979 Model genel olarak anlamlı (p-değeri < 0.05 H0 red).

Multiple R-squared = 0.9021:

Model, bağımlı değişken olan satılan kazak sayısındaki (X1) varyansın yaklaşık %90.21’ini açıklıyor. Bu, modelin bağımlı değişkeni iyi bir şekilde açıkladığını gösterir.

katsayıların yorumu:
Estimate = 56.81125, p = 0.45247:

Sabit terim (Intercept) istatistiksel olarak anlamlı değil (p > 0.05). Bu, diğer değişkenler sıfır olduğunda bağımlı değişkenin (satışların) başlangıç değeriyle ilgili bir sonuç çıkarmanın doğru olmadığını gösterir.Bu yüzden anlamlı bir katkı sağlamamaktadır.

X2 (Fiyat Değişimi):

H0: fiyat değişiminin satış üzerinde etkisi anlamlı değildir.

Katsayı = -0.15027, p = 0.71889:

Fiyat değişiminin etkisi istatistiksel olarak anlamlı değildir (p > 0.05 H0 reddedilemez). Bu, fiyat değişimlerinin (X2) satılan kazak sayısı (X1) üzerinde anlamlı bir etkisi olmadığını gösterir.Bu bulgu, fiyat değişimlerinin kazak satışları üzerinde anlamlı bir etkisi olduğu görüşünü desteklemez.

reklam maliyetleri:

H0: reklam maliyetinin satış üzerinde etkisi anlamlı değildir.

Katsayı = 0.48059, p = 0.00195: Reklam maliyetlerinin etkisi istatistiksel olarak anlamlıdır (p < 0.05 H0 red). Her 1 Euro’luk reklam maliyeti artışı, satılan kazak sayısını ortalama 0.48 artırmaktadır. Reklamlar, kazak satışları üzerinde önemli bir etkiye sahiptir.

satış asistanı varlığı(saat):

H0:satış asistanı varlığının satış üzerinde etkisi anlamlı değildir.

Katsayı = 0.87742, p = 0.08064:

Satış asistanının saat başına etkisi anlamlı değildir.(p =0.08>0.05 H0 reddedilemez.)

model denklemine anlamlı çıkan değişkenler yazılır.

SORU-5
Çok değişkenli normal dağıldığı bilinen bir veri setinde:

X1
: 2024 yılında bir tatil beldesinde günlük satılan dondurma sayısını,
X2
: Celcius cinsinden günlük hava sıcaklığını,
X3
: O beldeye gelen günlük turist sayısını göstermektedir.
Veri setinin kovaryans matrisi (Σ
) ve ortalama vektörü (μ
) aşağıdaki gibi verilmiştir:

Σ=⎡⎣⎢40006000808012010000600010000120⎤⎦⎥veμ=⎡⎣⎢30030500⎤⎦⎥

Hava sıcaklığının 25∘
 olduğu bir gün için, dondurma sayısı (X1
) ve günlük turist sayısı (X3
) değişkenlerinin bileşik olasılık yoğunluk fonksiyonunu R programındaki bulgularınıza dayanarak yazınız.

# Gerekli kütüphane
library(mvtnorm)

# Veriler
mu <- c(300, 30, 500)  # Ortalama vektörü: X1, X2, X3
sigma <- matrix(c(4000, 80, 6000,  # Kovaryans matrisi
                  6000, 120, 10000,
                  80, 4, 120),
                nrow = 3, byrow = TRUE)

# Şartlı hesaplama için parçalar
mu_x1_x3 <- mu[c(1, 3)]  # X1 ve X3'ün ortalaması
mu_x2 <- mu[2]  # X2'nin ortalaması

sigma_x1_x3 <- sigma[c(1, 3), c(1, 3)]  # X1 ve X3'ün kovaryans alt matrisi
sigma_x2 <- sigma[2, 2]  # X2'nin varyansı
sigma_x1_x3_x2 <- sigma[c(1, 3), 2]  # X1 ve X3 ile X2'nin kovaryansı

# X2 = 25 olduğunda Şartlı Ortalama
x2_verilen <- 25  # X2'nin verilen değeri
mu_cond <- mu_x1_x3 + sigma_x1_x3_x2 * (1 / sigma_x2) * (x2_verilen - mu_x2)

# X2 = 25 olduğunda Şartlı Kovaryans
sigma_cond <- sigma_x1_x3 - sigma_x1_x3_x2 %*% t(sigma_x1_x3_x2) / sigma_x2

# Sonuçları yazdır
cat("Koşullu Ortalama (X1 ve X3 | X2=25):\n")
## Koşullu Ortalama (X1 ve X3 | X2=25):
print(mu_cond)
## [1] 296.6667 499.8333
cat("\nKoşullu Kovaryans (X1 ve X3 | X2=25):\n")
## 
## Koşullu Kovaryans (X1 ve X3 | X2=25):
print(sigma_cond)
##            [,1]      [,2]
## [1,] 3946.66667 5997.3333
## [2,]   77.33333  119.8667
# Yoğunluk fonksiyonunu tanımlama
yogunluk_fonk <- function(x1, x3) {
  x <- c(x1, x3)
  dmvnorm(x, mean = mu_cond, sigma = sigma_cond)
}
X1 ve X3 değişkenlerinin birleşik olasılık yoğunluk fonksiyonu, çok değişkenli normal dağılım fonksiyonudur

Ortalama vektörü (μ) ve kovaryans matrisi (Σ) tanımlanır.

koşullu Ortalama (X1,X3|X2=25)
X1:296.67 X3:499.83

Bu değerler, hava sıcaklığının 25 derece olduğu bir günde, günlük satılan dondurma sayısı ve günlük turist sayısı değişkenlerinin beklenen (ortalama) değerleridir.

Ortalama 296.67 dondurma satışı, Ortalama 499.83 turist beklenmektedir.

koşullu kovaryans (X1,X3|X2=25)
Bu matris, X1 ve X3’ün birlikte varyans ve kovaryans değerlerini gösterir.

var(x1):3946.67- Dondurma satışının varyansı. Dondurma satışlarının değişkenliği oldukça yüksek.

var(X3):119.87- Turist sayısının varyansı. Turist sayısındaki değişkenlik daha düşüktür, yani daha istikrarlı bir değere sahiptir.

COV(X1,X3):5997.33 -Dondurma satışları ile turist sayısı arasında pozitif bir ilişki olduğunu ifade eder. Yani turist sayısı arttıkça, dondurma satışlarının da artması beklenir.

Hava sıcaklığı 25 derece iken dondurma satışları ile turist sayısı arasında anlamlı bir ilişki vardır.Bu ilişkinin yönü pozitiftir, yani turist sayısındaki artış, dondurma satışlarını da artırmaktadır.

SORU-6
Olasılık, Matematiksel İstatistik ve Regresyon Analizi derslerinden 100 üzerinden alınan notlar aşağıda verilmiştir. Herhangi bir öğrencinin bu üç dersin eşit ağırlıklandırmaları üzerinden hesaplanacak not ortalaması Y
 olarak tanımlanırsa, Y
’nin 70’ten az olma olasılığı kaçtır? Cevabınızı R programındaki bulgularınızla birlikte destekleyiniz.

Veriler:
Öğrenciler	Olasılık	Matematik	Örnekleme
Gizem	88	76	51
Tuncay	91	82	70
Can	64	72	98
Tuğçe	83	88	66
Görkem	77	80	94
Akın	45	60	55
Ebru	48	44	73
Burcu	51	48	64
Funda	63	61	79
Hande	65	63	44
# Verilerin tanımlanması
notlar <- data.frame(
  Ogrenci = c("Gizem", "Tuncay", "Can", "Tuğçe", "Görkem", "Akın", "Ebru", "Burcu", "Funda", "Hande"),
  Olasilik = c(88, 91, 64, 83, 77, 45, 48, 51, 63, 65),
  Matematik = c(76, 82, 72, 88, 80, 60, 44, 48, 61, 63),
  Ornekleme = c(51, 70, 98, 66, 94, 55, 73, 64, 79, 44)
)

# Ortalama hesaplama
notlar$Ortalama <- rowMeans(notlar[, 2:4])

# 70'ten küçük ortalamaların tespiti
ortalama_70ten_kucuk <- notlar$Ortalama < 70

# 70'ten küçük ortalama oranını hesaplama
olasilik <- mean(ortalama_70ten_kucuk)

# Sonuçları yazdırma
cat("Ortalama 70'ten küçük olma olasılığı:", olasilik, "\n")
## Ortalama 70'ten küçük olma olasılığı: 0.5
Öğrencilerin not ortalamasının 70’ten az olma olasılığı %50 imiş.
