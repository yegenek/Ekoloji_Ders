
# Verilerin yüklenmesi
AB_data <- read.csv("Data_agac.csv",header = TRUE,col.names = c("A","B"))
AB_data

# Verileri A ve B alanı olmak üzere iki kısma ayırın ve boş verileri atın (NA)
alan_A <- AB_data$A
alan_B <- na.omit(AB_data$B)

# Tanımlayıcı istatistik
summary(alan_A)

summary(alan_B)

# standart sapma ve varyasyon

sd(alan_A)

sd(alan_B)

# varyans ve örneklem büyüklüklerini ayrı olarak belirleyin
var_A <- var(alan_A)

var_B <- var(alan_B)

n_A <- length(alan_A)

n_B <- length(alan_B)



# T değerinin bulunması, Cohen'in değerine göre (uzun yol)

d_value <-  (mean(alan_A)-mean(alan_B))/sqrt((var_A+var_B)^2)

d_value

t_value <- (mean(alan_A)-mean(alan_B))/sqrt((var_A/n_A+var_B/n_B))

t_value
# P değerinin bulunması, çift taraflı olduğu için ikiyle çarpılması gereklidir

pt(t_value,df=22,lower.tail = FALSE, log.p = FALSE)*2    


# Histogram grafikleri

hist(alan_A)

hist(alan_B)

# T testi ile alanların karşılaştırılması (kısayol)

t.test(alan_A,alan_B,alternative = "two.sided",paired = FALSE)

