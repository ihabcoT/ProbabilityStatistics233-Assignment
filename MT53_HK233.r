# LINK DỮ LIỆU: https://www.kaggle.com/datasets/iliassekkaf/computerparts/data

########################################################################################
# TIỀN XỬ LÝ DỮ LIỆU
########################################################################################
# Định nghĩa các giá trị NA
naStrings = c("\n- ", "", "N/A")

# Đọc dữ liệu từ tệp CSV
data <- read.csv("C:/Users/MACBOOK AIR/Documents/2023-2024/[HK232]/[MT2013] Possibilities & Satistics/BTL/Intel_CPUs.csv", header = TRUE, na.strings = naStrings)
#C:/Users/MACBOOK AIR/Documents/2023-2024/[HK232]/[MT2013] Possibilities & Satistics/BTL/Intel_CPUs.csv

# Hiển thị 6 dòng đầu tiên của dữ liệu
head(data)

# Thống kê biến và giá trị quan trắc sau tiền xử lý số liệu
str(data)

# Lựa chọn các cột chính từ dữ liệu
main_Factors <- data [, c("Vertical_Segment", "Lithography", "Recommended_Customer_Price", "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "Cache", "Max_Memory_Size", "Max_nb_of_Memory_Channels", "Max_Memory_Bandwidth")]

# Hiển thị 6 dòng đầu tiên của main_Factors
head(main_Factors)

#install.packages("dplyr")
library(dplyr)

# Chuyển đổi và xử lý định dạng dữ liệu
main_Factors <- mutate(main_Factors,
    Vertical_Segment = as.character(Vertical_Segment),
    Lithography = as.numeric(gsub("[^0-9.]", "", Lithography)),
    Recommended_Customer_Price = ifelse(
	  Recommended_Customer_Price == "N/A", NA, 
        sapply(strsplit(Recommended_Customer_Price, " - "), function(x) {
            price_val <- as.numeric(gsub("[^0-9.]" , "", x))
            if (!anyNA(price_val)) { 
                mean(price_val)
            } else{ NA }
    })),
    nb_of_Cores = as.numeric(gsub("[^0-9.]", "", nb_of_Cores)),
    nb_of_Threads = as.numeric(gsub("[^0-9.]", "", nb_of_Threads)),
    Processor_Base_Frequency = ifelse(
        grepl("MHz", Processor_Base_Frequency), 
        as.numeric(gsub("[^0-9.]", "", Processor_Base_Frequency))/1000, 
        as.numeric(gsub("[^0-9.]", "", Processor_Base_Frequency))),
    Cache = ifelse(
        grepl("KB", Cache),
        as.numeric(gsub("[^0-9.]", "", Cache))/1024,
        as.numeric(gsub("[^0-9.]", "", Cache))),
    Max_Memory_Size = ifelse(
        grepl("TB", Max_Memory_Size),
        as.numeric(gsub("[^0-9.]", "", Max_Memory_Size))*1024,
        as.numeric(gsub("[^0-9.]", "", Max_Memory_Size))),
    Max_nb_of_Memory_Channels = as.numeric(gsub("[^0-9.]", "", Max_nb_of_Memory_Channels)),
    Max_Memory_Bandwidth = as.numeric(gsub("[^0-9.]", "", Max_Memory_Bandwidth)),
)

# Hiển thị 12 dòng đầu tiên của main_Factors để kiểm tra
head(main_Factors,12)

# Loại bỏ các dòng bị khuyết ở các cột có tỷ lệ dữ liệu khuyết thấp
main_Factors <- main_Factors %>%
    filter(!is.na(Lithography), !is.na(Processor_Base_Frequency), !is.na(Cache))

# Thay thế giá trị khuyết bằng giá trị trung bình cho các cột có tỷ lệ dữ liệu khuyết cao
main_Factors <- mutate(main_Factors,
    Recommended_Customer_Price = ifelse(is.na(Recommended_Customer_Price), 
                                        mean(Recommended_Customer_Price, na.rm = TRUE),
                                        Recommended_Customer_Price),

    nb_of_Threads = ifelse(is.na(nb_of_Threads), 
                           median(nb_of_Threads, na.rm = TRUE), 
                           nb_of_Threads),

    Max_Memory_Size = ifelse(is.na(Max_Memory_Size), 
                             median(Max_Memory_Size, na.rm = TRUE), 
                             Max_Memory_Size),

    Max_nb_of_Memory_Channels = ifelse(is.na(Max_nb_of_Memory_Channels),
                                       median(Max_nb_of_Memory_Channels, na.rm = TRUE),
                                       Max_nb_of_Memory_Channels),

    Max_Memory_Bandwidth = ifelse(is.na(Max_Memory_Bandwidth), 
                                  mean(Max_Memory_Bandwidth, na.rm = TRUE), 
                                  Max_Memory_Bandwidth)
)

# Tạo bảng thống kê dữ liệu khuyết lần 2
missing_data_stats <- main_Factors %>%
    summarise_all(~sum(is.na(.))) %>%
    gather(key = "Column", value = "Missing_Count") %>%
    mutate(Total_Count = nrow(main_Factors),
    Missing_Percentage = (Missing_Count / Total_Count) * 100)

# Hiển thị bảng thống kê dữ liệu khuyết lần 2
print(missing_data_stats)

# Hiển thị 12 dòng đầu tiên của main_Factors để kiểm tra
head(main_Factors,12)

# Thống kê biến và giá trị quan trắc sau tiền xử lý số liệu
str(main_Factors)

########################################################################################
# THỐNG KÊ MÔ TẢ
########################################################################################

# Bảng thống kê cho biến liên tục
ThongKeLienTuc <- as.data.frame(
    cbind(
    apply(main_Factors[c("Recommended_Customer_Price", "Processor_Base_Frequency", "Cache", "Max_Memory_Size", 
    "Max_Memory_Bandwidth" )], MARGIN = 2, mean),
    apply(main_Factors[c("Recommended_Customer_Price", "Processor_Base_Frequency", "Cache", "Max_Memory_Size", 
    "Max_Memory_Bandwidth" )], MARGIN = 2, sd),
    apply(main_Factors[c("Recommended_Customer_Price", "Processor_Base_Frequency", "Cache", "Max_Memory_Size", 
    "Max_Memory_Bandwidth" )], MARGIN = 2, min),
    apply(main_Factors[c("Recommended_Customer_Price", "Processor_Base_Frequency", "Cache", "Max_Memory_Size", 
    "Max_Memory_Bandwidth" )], MARGIN = 2, max),
    apply(main_Factors[c("Recommended_Customer_Price", "Processor_Base_Frequency", "Cache", "Max_Memory_Size", 
    "Max_Memory_Bandwidth" )], MARGIN = 2, quantile, probs = c(0.25)),    
    apply(main_Factors[c("Recommended_Customer_Price", "Processor_Base_Frequency", "Cache", "Max_Memory_Size", 
    "Max_Memory_Bandwidth" )], MARGIN = 2, median),
    apply(main_Factors[c("Recommended_Customer_Price", "Processor_Base_Frequency", "Cache", "Max_Memory_Size", 
    "Max_Memory_Bandwidth" )], MARGIN = 2, quantile, probs = c(0.75))
    )
 )
colnames(ThongKeLienTuc) <- c("TrungBinh", "DoLechChuan", "GTNN", "GTLN", "Q1", "TrungVi", "Q3")
View(ThongKeLienTuc)

# Bảng thống kê cho biến rời rạc
summary_stats <- function(x) {
    freq_table <- table(x)
    max_freq <- max(freq_table)
    max_freq_value <- as.numeric(names(freq_table)[which(freq_table == max_freq)])

    data.frame(
    TrungBinh = mean(x),
    DoLechChuan = sd(x),
    GTNN = min(x),
    GTLN = max(x),
    TrungVi = median(x),
    GTriXuatHienNhieuNhat = max_freq_value
    )
}

summaryLithography <- summary_stats(main_Factors$Lithography)
summaryCores <- summary_stats(main_Factors$nb_of_Cores)
summaryThreads <- summary_stats(main_Factors$nb_of_Threads)
summaryMemoryChannels <- summary_stats(main_Factors$Max_nb_of_Memory_Channels)

ThongKeRoiRac <- rbind(summaryLithography, summaryCores, summaryThreads, summaryMemoryChannels)
row.names(ThongKeRoiRac) <- c("Lithography", "Cores", "Threads", "Maximum Memory Channels")
View(ThongKeRoiRac)

#install.packages("ggplot2")
library(ggplot2)   #Vẽ biểu đồ
#install.packages("patchwork")
library(patchwork) #Kết hợp các biểu đồ
#install.packages("nortest")
library(nortest)   #Kiểm tra phân phối chuẩn
#install.packages("tidyverse")
library(tidyverse) #Tính giá trị thống kê

# Biểu đồ Histogram
hist(main_Factors$Recommended_Customer_Price, main = "Histogram of Recommended Customer Price", xlab = "Recommended Customer Price ($)", col = "purple", border = "black")
hist(main_Factors$Lithography, main = "Histogram of Lithography", xlab = "Lithography (nm)", col = "yellow", border = "black")
hist(main_Factors$nb_of_Cores, main = "Histogram of Number of Cores", xlab = "Number of Cores", col = "lightcoral", border = "black")
hist(main_Factors$nb_of_Threads, main = "Histogram of Number of Threads", xlab = "Number of Threads", col = "blue", border = "black")
hist(main_Factors$Processor_Base_Frequency, main = "Histogram of Processor Base Frequency", xlab = "Processor Base Frequency (GHz)", col = "lightcyan", border = "black")
hist(main_Factors$Cache, main = "Histogram of Cache", xlab = "Cache (MB)", col = "lightgreen", border = "black")
hist(main_Factors$Max_Memory_Size, main = "Histogram of Max Memory Size", xlab = "Max Memory Size (GB)", col = "lightblue", border = "black")
hist(main_Factors$Max_nb_of_Memory_Channels, main = "Histogram of Max Number of Memory Channels", xlab = "Max Number of Memory Channels", col = "grey", border = "black")
hist(main_Factors$Max_Memory_Bandwidth, main = "Histogram of Max Memory Bandwidth", xlab = "Max Memory Bandwidth (GB/s)", col = "pink", border = "black")

# Biểu đồ Boxplot
boxplot(main_Factors$Recommended_Customer_Price, main = "Boxplot of Recommended Customer Price", ylab = "Recommended Customer Price ($)", col = "purple")
boxplot(main_Factors$Lithography, main = "Boxplot of Lithography", ylab = "Lithography (nm)", col = "yellow")
boxplot(main_Factors$nb_of_Cores, main = "Boxplot of Number of Cores", ylab = "Number of Cores", col = "lightcoral")
boxplot(main_Factors$nb_of_Threads, main = "Boxplot of Number of Threads", ylab = "Number of Threads", col = "blue")
boxplot(main_Factors$Processor_Base_Frequency, main = "Boxplot of Processor Base Frequency", ylab = "Processor Base Frequency (GHz)", col = "lightcyan")
boxplot(main_Factors$Cache, main = "Boxplot of Cache", ylab = "Cache (MB)", col = "lightgreen")
boxplot(main_Factors$Max_Memory_Size, main = "Boxplot of Max Memory Size", ylab = "Max Memory Size (GB)", col = "lightblue")
boxplot(main_Factors$Max_nb_of_Memory_Channels, main = "Boxplot of Max Number of Memory Channels", ylab = "Max Number of Memory Channels", col = "grey")
boxplot(main_Factors$Max_Memory_Bandwidth, main = "Boxplot of Max Memory Bandwidth", ylab = "Max Memory Bandwidth (GB/s)", col = "pink")

# Biểu đồ Scatterplot
plot(main_Factors$Lithography, main_Factors$Recommended_Customer_Price, 
    main = "Lithography vs Recommended Customer Price",
    xlab = "Lithography (nm)", ylab = "Recommended Customer Price ($)")  

plot(main_Factors$nb_of_Cores, main_Factors$Recommended_Customer_Price, 
    main = "Number of Cores vs Recommended Customer Price",
    xlab = "Cores", ylab = "Recommended Customer Price ($)") 

plot(main_Factors$nb_of_Threads, main_Factors$Recommended_Customer_Price, 
    main = "Number of Threads vs Recommended Customer Price",
    xlab = "Threads", ylab = "Recommended Customer Price ($)")  

plot(main_Factors$Processor_Base_Frequency, main_Factors$Recommended_Customer_Price, 
    main = "Processor Base Frequency vs Recommended Customer Price",
    xlab = "Processor Base Frequency (GHz)", ylab = "Recommended Customer Price ($)") 

plot(main_Factors$Cache, main_Factors$Recommended_Customer_Price, 
    main = "Cache vs Recommended Customer Price",
    xlab = "Cache (MB)", ylab = "Recommended Customer Price ($)")  

plot(main_Factors$Max_Memory_Size, main_Factors$Recommended_Customer_Price, 
    main = "Max Memory Size vs Recommended Customer Price",
    xlab = "Max Memory Size (GB)", ylab = "Recommended Customer Price ($)")  

plot(main_Factors$Max_Memory_Bandwidth, main_Factors$Recommended_Customer_Price, 
    main = "Max Memory Bandwidth vs Recommended Customer Price",
    xlab = "Max Nemory Bandwidth (GB/s)", ylab = "Recommended Customer Price ($)")  

########################################################################################
# KIỂM ĐỊNH TRUNG BÌNH 1 MẪU
# Bạn Ngọc Khánh cho rằng những thiết bị sở hữu tốc độ băng thông cao (từ 85 GB/s trở lên)
# có giá bán trung bình là $3000. 
# Với mức ý nghĩa 5%, kiểm tra xem phát biểu của bạn đúng hay sai? 
########################################################################################

# Lọc dữ liệu cho mẫu
HighBandwidthVertical <- main_Factors %>%
filter(Max_Memory_Bandwidth >= 85) %>%
select(Max_Memory_Bandwidth, Recommended_Customer_Price)

# Hiển thị bảng mẫu
print(HighBandwidthVertical)

# Kiểm định phân phối chuẩn Shapiro-Wilk
shapiro.test(HighBandwidthVertical$Recommended_Customer_Price)

# Kích thước mẫu
length(HighBandwidthVertical$Recommended_Customer_Price)

# Tính giá trị z cho kiểm định một mẫu
z <- qnorm(1-0.05/2)
cat("Miền bác bỏ: RR = (-∞;", -z,") ∪ (",z,"; +∞)\n")

# Thực hiện Welch's t-test
t.test(HighBandwidthVertical$Recommended_Customer_Price, mu = 3000)

########################################################################################
# KIỂM ĐỊNH TRUNG BÌNH 2 MẪU
# Bạn Minh Thái cho rằng giá bán lẻ đề xuất của CPU cho loại thiết bị Desktop
# sẽ đắt hơn các CPU cho loại thiết bị Mobile.
# Với mức ý nghĩa 5%, kiểm tra xem phát biểu của bạn đúng hay sai?
########################################################################################

#library(dplyr)
# Lọc dữ liệu cho mẫu
Desktop <- main_Factors$Recommended_Customer_Price[grep("Desktop", main_Factors$Vertical_Segment, ignore.case = TRUE)]
Mobile <- main_Factors$Recommended_Customer_Price[grep("Mobile", main_Factors$Vertical_Segment, ignore.case = TRUE)]

# Hiển thị bảng mẫu
print(Desktop)
print(Mobile)

# Kiểm định phân phối chuẩn Shapiro-Wilk
shapiro.test(Desktop)
shapiro.test(Mobile)

# Kích thước mẫu
length(Desktop)
length(Mobile)

# Tính giá trị z cho kiểm định hai mẫu
z <- qnorm(1-0.05)
cat("Miền bác bỏ: RR = (",z,"; +∞) \n")

# Thực hiện Welch's t-test với giả thuyết đối một phía (μ1 > μ2)
t.test(Desktop, Mobile, alternative = "greater")

########################################################################################
# PHÂN TÍCH PHƯƠNG SAI (ANOVA)
# Có ý kiến cho rằng giá bán trung bình của các loại thiết bị
# Mobile, Desktop, Embedded và Server là bằng nhau.
# Với mức ý nghĩa 5\%, kiểm tra xem ý kiến trên đúng hay không?
########################################################################################

# Lọc dữ liệu
anova_Factors <- main_Factors %>% select("Vertical_Segment", "Recommended_Customer_Price")
Mobile <- main_Factors$Recommended_Customer_Price[grep("Mobile", main_Factors$Vertical_Segment, ignore.case = TRUE)]
Desktop <- main_Factors$Recommended_Customer_Price[grep("Desktop", main_Factors$Vertical_Segment, ignore.case = TRUE)]
Embedded <- main_Factors$Recommended_Customer_Price[grep("Embedded", main_Factors$Vertical_Segment, ignore.case = TRUE)]
Server <- main_Factors$Recommended_Customer_Price[grep("Server", main_Factors$Vertical_Segment, ignore.case = TRUE)]

# Bảng thống kê
MauGop <- c(Mobile, Desktop, Embedded, Server)
ThongKe <- data.frame(Vertical_Segments = c("Mobile", "Desktop", "Embedded", "Server", "Mau gop"),
TrungBinh = c(mean(Mobile), mean(Desktop), mean(Embedded), mean(Server), mean(MauGop)),
PhuongSai = c(var(Mobile), var(Desktop), var(Embedded), var(Server), var(MauGop)),
KichThuocMau = c(length(Mobile), length(Desktop), length(Embedded), length(Server), length(MauGop)))
View(ThongKe)

# Tính giá trị F cho ANOVA
F_RR <- (qf(1 - 0.05, 4 - 1, length(Mobile) + length(Desktop) + length(Embedded) + length(Server) - 4))
cat("Miền bác bỏ: RR = (",F_RR,"; +∞) \n")

# Kiểm tra giả thiết 2: Các tổng thể có phân phối chuẩn?
by(anova_Factors$Recommended_Customer_Price, anova_Factors$Vertical_Segment, shapiro.test)

# Kiểm định ANOVA
resultANOVA <- aov(Recommended_Customer_Price ~ Vertical_Segment, data = anova_Factors)
print(resultANOVA)
print(summary(resultANOVA))

# So sánh bội LSD với alpha = 0.05
TukeyHSD(resultANOVA)

########################################################################################
# HỒI QUY TUYẾN TÍNH
# Các thông số của CPU ảnh hưởng tới giá cả của nó như thế nào?
########################################################################################

# Tính hệ số tương quan giữa biến giá cả và các biến còn lại
selected_data <- main_Factors[, c("Recommended_Customer_Price", "Lithography", "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "Cache", "Max_Memory_Size", "Max_nb_of_Memory_Channels", "Max_Memory_Bandwidth")]
correlation_matrix <- cor(selected_data, method = "spearman")
print(correlation_matrix)

# Chọn mô hình
summary(lm(Recommended_Customer_Price ~ Lithography + nb_of_Cores + nb_of_Threads + Processor_Base_Frequency + Cache + Max_Memory_Size + Max_nb_of_Memory_Channels + Max_Memory_Bandwidth, data = main_Factors))
summary(lm(Recommended_Customer_Price ~ Lithography + nb_of_Cores + nb_of_Threads + Processor_Base_Frequency + Cache + Max_Memory_Size + Max_nb_of_Memory_Channels, data = main_Factors))
summary(lm(Recommended_Customer_Price ~ Lithography + nb_of_Cores + nb_of_Threads + Cache + Max_Memory_Size + Max_nb_of_Memory_Channels, data = main_Factors))
summary(lm(Recommended_Customer_Price ~ Lithography + nb_of_Cores + nb_of_Threads + Cache + Max_Memory_Size, data = main_Factors))

# Kiểm tra giả thiết 1 của mô hình hồi quy tuyến tính bằng đồ thị hệ số tương quan: Có r > 0.8?
selected_model <- main_Factors[, c("Recommended_Customer_Price", "Lithography", "nb_of_Cores", "nb_of_Threads", "Processor_Base_Frequency", "Cache", "Max_Memory_Size", "Max_nb_of_Memory_Channels")]

#library(psych)
pairs.panels(
  selected_model,
  method = "spearman",
  hist.col = "lightblue",
  density = TRUE,
  ellipses = TRUE,
  lm = TRUE)

# Kiểm tra giả thiết 2 của mô hình hồi quy tuyến tính bằng đồ thị hệ số tương quan: εi có cùng phân phối chuẩn N(0, σ2) với σ không đổi?
model <- lm(Recommended_Customer_Price ~ Lithography + nb_of_Cores + nb_of_Threads + Processor_Base_Frequency + Cache + Max_Memory_Size + Max_nb_of_Memory_Channels, data = main_Factors)
fitted(model)
resid(model)
op <- par(mfrow=c(2,2))
plot(model) 