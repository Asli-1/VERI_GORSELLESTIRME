#1. GRAF??K- AYLARA G??RE ORTALAMA TRAF??K YO??UNLU??U- ????ZG?? GRAF??????

library(ggplot2)
library(dplyr)
library(lubridate)
# 1. Veriyi y??kle
df <- read.csv("C:/Users/Acer/Downloads/Sehir_verileri.csv", fileEncoding = "UTF-8", stringsAsFactors = FALSE)
# 2. Tarih s??tununu Date format??na ??evir (g??n-ay-y??l format??nda)
df$analiz_baslangic_tarih <- as.Date(df$analiz_baslangic_tarih, format = "%d-%m-%Y")
# 3. Ayl??k ortalama yo??unlu??u hesapla
df_aylik <- df %>%
  filter(!is.na(analiz_baslangic_tarih)) %>%
  mutate(ay = month(analiz_baslangic_tarih, label = TRUE, abbr = FALSE)) %>%
  group_by(ay) %>%
  summarise(yogunluk = mean(sonuc, na.rm = TRUE))
# 4. Ayl??k ortalama yo??unlu??u g??steren ??izgi grafi??ini olu??tur
ggplot(df_aylik, aes(x = ay, y = yogunluk, group = 1)) +
  geom_line(color = "#6E7B8B", size = 1) +  # Gri ??izgi
  geom_point(color = "#87CEFA", size = 3) +  # A????k mavi noktalar
  labs(title = "Aylara G??re Ortalama Trafik Yo??unlu??u",
       x = "Ay", y = "Yo??unluk") +
  theme_minimal()
ggsave("harita_600dpi.png", plot = p, width = 10, height = 8, dpi = 600)

#-----------------------------------------------------------------------------

#2.GRAF??K- YED?? B??LGEYE G??RE ORTALAMA SONU?? DA??ILIMI- S??TUN GRAF??????

library(dplyr)
library(ggplot2)
library(stringi)

# 1. Veri setini UTF-8 olarak oku
Sehir_verileri <- read.csv(
  "C:/Users/Acer/Downloads/Sehir_verileri.csv",
  stringsAsFactors = FALSE,
  fileEncoding = "UTF-8"
)
# 2. sehir_adi s??tununu ASCII???ye ??evir (???????????????????????? ??? cCgGiIoOsSuU)
Sehir_verileri <- Sehir_verileri %>%
  mutate(
    sehir_ascii = stri_trans_general(sehir_adi, "Latin-ASCII")
  )
# 3. B??lge atamas??n?? ASCII ba??l??????na g??re yap ve Di??er???i filtrele
Sehir_verileri <- Sehir_verileri %>%
  mutate(
    Bolge = case_when(
      sehir_ascii %in% c("Istanbul","Tekirdag","Edirne","Kirklareli","Balikesir",
                         "Canakkale","Bursa","Bilecik","Sakarya","Kocaeli","Yalova") 
      ~ "Marmara",
      sehir_ascii %in% c("Antalya","Burdur","Isparta","Mersin","Adana","Hatay",
                         "Osmaniye","Kahramanmaras") 
      ~ "Akdeniz",
      sehir_ascii %in% c("Malatya","Erzincan","Elazig","Tunceli","Bingol",
                         "Erzurum","Mus","Bitlis","Sirnak","Kars","Agri",
                         "Ardahan","Van","Igdir","Hakkari") 
      ~ "Do??u Anadolu",
      sehir_ascii %in% c("Izmir","Aydin","Mugla","Manisa","Denizli",
                         "Usak","Kutahya","Afyonkarahisar") 
      ~ "Ege",
      sehir_ascii %in% c("Gaziantep","Kilis","Adiyaman","Sanliurfa",
                         "Diyarbakir","Mardin","Batman","Siirt") 
      ~ "G??neyDo??u Anadolu",
      sehir_ascii %in% c("Eskisehir","Konya","Ankara","Cankiri","Aksaray",
                         "Kirikale","Kirsehir","Yozgat","Nigde","Nevsehir",
                         "Kayseri","Karaman","Sivas") 
      ~ "I?? Anadolu",
      sehir_ascii %in% c("Bolu","Duzce","Zonguldak","Karabuk","Bartin",
                         "Kastamonu","Corum","Sinop","Samsun","Amasya",
                         "Tokat","Ordu","Giresun","Gumusghe","Trabzon",
                         "Bayburt","Rize","Artvin") 
      ~ "Karadeniz",
      TRUE 
      ~ NA_character_
    )
  ) %>%
  filter(!is.na(Bolge))
# 4. B??lge fakt??r seviyelerini istedi??imiz s??raya sok
bolge_duzeyleri <- c(
  "Marmara", "Ege", "Akdeniz",
  "???? Anadolu", "Karadeniz",
  "Do??u Anadolu", "G??neyDo??u Anadolu"
)
Sehir_verileri <- Sehir_verileri %>%
  mutate(
    Bolge = factor(Bolge, levels = bolge_duzeyleri)
  )
# 5. B??lgelere g??re ortalama sonucu hesapla
Bolge_avg <- Sehir_verileri %>%
  group_by(Bolge) %>%
  summarise(Ortalama_Sonuc = mean(sonuc, na.rm = TRUE)) %>%
  ungroup()
# 6. S??tun grafi??ini ??iz
ggplot(Bolge_avg, aes(x = Bolge, y = Ortalama_Sonuc, fill = Bolge)) + 
  geom_col() + 
  scale_fill_manual(values = setNames(
    rep(c("#A2C2E5", "#D3D3D3"), length.out = length(bolge_duzeyleri)),
    bolge_duzeyleri
  )) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title   = element_text(hjust = 0.5),
    panel.grid   = element_blank()
  ) +
  labs(
    x     = "Bolge",
    y     = "Ortalama Sonuc",
    title = "Yedi Bolgeye Gore Ortalama Sonuc Dagilimi"
  )
ggsave("harita_600dpi.png", plot = p, width = 10, height = 8, dpi = 600)

#---------------------------------------------------------------------------

#3.GRAF??K ??EH??RLERE G??RE SONU?? DA??ILIMI- S??TUN GRAF??????

library(ggplot2)
ggplot(Sehir_verileri, aes(x=sehir_adi, y=sonuc)) + 
  geom_bar(stat="identity", fill="#A2C2E5") + # Barlar i??in pastel mavi renk
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1), # X eksenindeki yaz??lar?? d??nd??r
    plot.title = element_text(hjust = 0.5), # Ba??l?????? ortala
    panel.grid = element_blank() # Izgaralar?? kald??r
  ) +
  labs(
    x = "Sehirler", 
    y = "Sonuc Degeri", 
    title = "Sehirlere Gore Sonuc Dagilimi"
  )
ggsave("harita_600dpi.png", plot = p, width = 10, height = 8, dpi = 600)


#---------------------------------------------------------------------------

#4.GRAF??K- MEVS??MLERE G??RE SONU?? DA??ILIMI-KUTU GRAF??????

library(dplyr)
library(ggplot2)
library(lubridate)
# 1. Tarihleri Date s??n??f??na ??evir
Sehir_verileri <- Sehir_verileri %>%
  mutate(
    baslangic = as.Date(analiz_baslangic_tarih, format = "%d-%m-%Y"),
    bitis     = as.Date(analiz_bitis_tarih,     format = "%d-%m-%Y")
  )
# 2. Ay?? ????kar ve mevsime e??le
Sehir_verileri <- Sehir_verileri %>%
  mutate(
    ay = month(baslangic),  # veya bitis
    Mevsim = case_when(
      ay %in% 3:5   ~ "??lkbahar",
      ay %in% 6:8   ~ "Yaz",
      ay %in% 9:11  ~ "Sonbahar",
      ay %in% c(12,1,2) ~ "K????"
    )
  )
# 3. Kutuphane fakt??r seviyelerini ayarla (okunur s??rayla)
Sehir_verileri <- Sehir_verileri %>%
  mutate(
    Mevsim = factor(Mevsim, levels = c("??lkbahar","Yaz","Sonbahar","K????"))
  )
# 4. Kutu grafi??i
ggplot(Sehir_verileri, aes(x = Mevsim, y = sonuc, fill = Mevsim)) +
  geom_boxplot() +
  scale_fill_manual(values = c(
    "??lkbahar" = "#A2C2E5",
    "Yaz"      = "#D3D3D3",
    "Sonbahar" = "#A2C2E5",
    "K????"      = "#D3D3D3"
  )) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 0, hjust = 0.5),
    plot.title  = element_text(hjust = 0.5),
    panel.grid  = element_blank()
  ) +
  labs(
    x     = "Mevsim",
    y     = "Sonuc",
    title = "Mevsimlere Gore Sonuc Dagilimi (Kutu Grafigi)"
  )
ggsave("harita_600dpi.png", plot = p, width = 10, height = 8, dpi = 600)


#--------------------------------------------------------------------------


#5.GRAF??K-AY VE B??LGEYE G??RE ORTALAMA SONU??-ISI HAR??TASI

library(tidyverse)
library(lubridate)
# T??rk??e ay adlar?? i??in yerel ayar
Sys.setlocale("LC_TIME", "Turkish")
# Veriyi oku
veri <- read_csv("C:/Users/Acer/Downloads/Sehir_verileri.csv")
# B??lge s??tununu ekle ve "Di??er" olanlar?? ????kar
veri <- veri %>%
  mutate(Bolge = case_when(
    sehir_adi %in% c("Malatya", "Erzincan", "Elaz??g", "Tunceli", "Bingol", "Erzurum") ~ "Do??u Anadolu",
    sehir_adi %in% c("??stanbul", "Kocaeli", "Edirne", "Bursa", "Tekirdag") ~ "Marmara",
    sehir_adi %in% c("Ankara", "Konya", "Eskisehir", "Karaman", "Kayseri") ~ "???? Anadolu",
    sehir_adi %in% c("Antalya", "Mersin", "Adana", "Hatay") ~ "Akdeniz",
    sehir_adi %in% c("??zmir", "Ayd??n", "Mugla", "Manisa") ~ "Ege",
    sehir_adi %in% c("Trabzon", "Ordu", "Samsun", "Rize", "Giresun") ~ "Karadeniz",
    sehir_adi %in% c("Diyarbak??r", "Sanl??urfa", "Mardin", "Batman", "Gaziantep") ~ "G??neydo??u Anadolu",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Bolge))
# Ay bilgisini ????kar (T??rk??e olarak)
veri <- veri %>%
  mutate(ay = month(dmy(analiz_baslangic_tarih), label = TRUE, abbr = FALSE))
# Is?? haritas?? i??in veriyi gruplama
heatmap_data <- veri %>%
  group_by(Bolge, ay) %>%
  summarise(ortalama_sonuc = mean(sonuc, na.rm = TRUE), .groups = "drop")
# Grafik nesnesini olu??tur
p <- ggplot(heatmap_data, aes(x = ay, y = Bolge, fill = ortalama_sonuc)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "#dce3f0", high = "#2b6cb0") +
  labs(
    title = "Ay ve B??lgeye G??re Ortalama Sonu?? Is?? Haritas??",
    x = "Ay",
    y = "B??lge",
    fill = "Ortalama\nSonu??"
  ) +
  scale_x_discrete(labels = c("January" = "Ocak", "February" = "subat", "March" = "Mart", 
                              "April" = "Nisan", "May" = "Mayis", "June" = "Haziran", 
                              "July" = "Temmuz", "August" = "Agustos", "September" = "Eylul", 
                              "October" = "Ekim", "November" = "Kasim", "December" = "Aralik")) +
  theme_minimal() +
  theme(text = element_text(size = 12))

# Grafi??i g??ster
print(p)
# Y??ksek ????z??n??rl??kle grafi??i kaydet
ggsave("harita_600dpi.png", plot = p, width = 10, height = 8, dpi = 600)

#-----------------------------------------------------------------------------

#6.GRAF??K- T??RK??YE B??LGELER??NE G??RE TRAF??K YO??UNLU??U-SA??ILIM GRAF??????

veri <- read.csv("C:\Users\Acer\Downloads\Sehir_verileri.csv", fileEncoding = "UTF-8")
View(veri)  # Veriyi tablo ??eklinde g??rmek i??in
install.packages("ggplot2")
install.packages("dplyr")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthdata")
install.packages("rgeos") # baz?? sf i??lemleri i??in gerekebilir
# Gerekli paketler
library(ggplot2)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
# T??rkiye haritas??n?? al
turkey <- ne_countries(scale = "medium", country = "Turkey", returnclass = "sf")
# ??rnek veri (senin elindeki ger??ek veriyle de??i??tirebilirsin)
# Bolgeler, enlem ve boylam de??erleri ile yo??unluk oran??
veri <- data.frame(
  bolge = c("Marmara", "Ege", "Akdeniz", "???? Anadolu", "Karadeniz", "Do??u Anadolu", "G??neydo??u Anadolu"),
  lon = c(28.9, 27.1, 30.7, 33.5, 37.0, 39.5, 38.5),     # yakla????k boylamlar
  lat = c(40.9, 38.4, 37.0, 39.0, 41.2, 39.5, 37.9),     # yakla????k enlemler
  yogunluk = c(75, 60, 50, 40, 35, 20, 45)               # ??rnek yo??unluk oranlar??
)
# Haritay?? ??iz
ggplot(data = turkey) +
  geom_sf(fill = "grey85", color = "white") +  # gri harita
  geom_point(data = veri, aes(x = lon, y = lat, size = yogunluk), 
             color = "#80C1FF", alpha = 0.7) + # pastel mavi noktalar
  scale_size_continuous(range = c(3, 10)) +    # nokta b??y??kl??k aral??????
  theme_minimal() +
  labs(title = "Turkiye Bolgelerine Gore Trafik Yo??unlu??u", size = "Yo??unluk")
+
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    legend.position = "right"
  )
ggsave("harita_600dpi.png", plot = p, width = 10, height = 8, dpi = 600)
