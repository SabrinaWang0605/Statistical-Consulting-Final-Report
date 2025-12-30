# ============================================================================
# 數據清理和預處理 - 優化版本
# 處理: 發生地點(縣市)、死亡、受傷、車種
# ============================================================================

cat("數據清理和預處理 - 優化版本\n")

library(tidyverse)
library(stringr)

# ============================================================================
# 步驟1：讀取原始數據
# ============================================================================

cat("步驟1: 讀取原始數據\n\n")

traffic_data <- read.csv("E:/碩一/consulation/final data/traffic_accident_102_110.csv")

cat("原始數據資訊:\n")
cat("  行數:", nrow(traffic_data), "\n")
cat("  列數:", ncol(traffic_data), "\n")
cat("  欄位名稱:\n")
print(names(traffic_data))

# 查看前幾筆數據
cat("前5筆數據:\n")
print(head(traffic_data, 5))
cat("\n")


# ✓ 只保留年份 2013-2019
traffic_data <- traffic_data %>%
  filter(ad_year >= 2013 & ad_year <= 2019)

# 移除經度和緯度
if ("經度" %in% names(traffic_data)) {
  traffic_data <- traffic_data %>% dplyr::select(-經度)
}
if ("緯度" %in% names(traffic_data)) {
  traffic_data <- traffic_data %>% dplyr::select(-緯度)
}

# ============================================================================
# 步驟2：處理發生地點 (只保留縣市)
# ============================================================================

cat("步驟2: 處理發生地點\n")

cat("原始發生地點範例:\n")
print(head(traffic_data$發生地點, 10))
cat("\n")

# 提取縣市 (第一個括號前的文字或前4-6個字)
traffic_data <- traffic_data %>%
  mutate(
    # 方法1: 提取括號前的內容
    county_city_raw = str_extract(發生地點, "^[^\\(]+"),
    # 清理空白和特殊符號
    county_city = str_trim(county_city_raw),
    # 標準化縣市名稱 (移除"市"、"縣"後的文字)
    county_city = case_when(
      str_detect(county_city, "台北|臺北") ~ "台北市",
      str_detect(county_city, "新北") ~ "新北市",
      str_detect(county_city, "桃園") ~ "桃園市",
      str_detect(county_city, "台中|臺中") ~ "台中市",
      str_detect(county_city, "台南|臺南") ~ "台南市",
      str_detect(county_city, "高雄") ~ "高雄市",
      str_detect(county_city, "基隆") ~ "基隆市",
      str_detect(county_city, "新竹") ~ "新竹市",
      str_detect(county_city, "苗栗") ~ "苗栗縣",
      str_detect(county_city, "彰化") ~ "彰化縣",
      str_detect(county_city, "南投") ~ "南投縣",
      str_detect(county_city, "雲林") ~ "雲林縣",
      str_detect(county_city, "嘉義") ~ "嘉義市/縣",
      str_detect(county_city, "屏東") ~ "屏東縣",
      str_detect(county_city, "宜蘭") ~ "宜蘭縣",
      str_detect(county_city, "花蓮") ~ "花蓮縣",
      str_detect(county_city, "台東|臺東") ~ "台東縣",
      str_detect(county_city, "澎湖") ~ "澎湖縣",
      str_detect(county_city, "金門") ~ "金門縣",
      str_detect(county_city, "連江") ~ "連江縣",
      TRUE ~ county_city
    )
  ) %>%
  dplyr::select(-county_city_raw)


cat("提取後的縣市:\n")
print(table(traffic_data$county_city))

# ============================================================================
# 步驟3：處理死亡和受傷人數
# ============================================================================

cat("步驟3: 處理死亡和受傷人數\n")

cat("原始死亡受傷人數範例:\n")
print(head(traffic_data$死亡受傷人數, 10))
cat("\n")

# 檢查數據類型
cat("死亡受傷人數的類型:", class(traffic_data$死亡受傷人數), "\n\n")

# 處理死亡和受傷人數
traffic_data <- traffic_data %>%
  mutate(
    # 如果是字符串，需要分離
    death_count = case_when(
      # 格式: "死亡1;受傷2" 或 "死亡1、受傷2" 等
      str_detect(死亡受傷人數, "死") & str_detect(死亡受傷人數, "受傷") ~ {
        # 提取數字
        death_part <- str_extract(死亡受傷人數, "死亡(\\d+)")
        as.numeric(str_extract(death_part, "\\d+"))
      },
      str_detect(死亡受傷人數, "死") ~ as.numeric(str_extract(死亡受傷人數, "\\d+")),
      TRUE ~ 0
    ),
    injured_count = case_when(
      # 提取受傷人數
      str_detect(死亡受傷人數, "受傷") ~ {
        injured_part <- str_extract(死亡受傷人數, "受傷(\\d+)")
        as.numeric(str_extract(injured_part, "\\d+"))
      },
      TRUE ~ 0
    ),
    # 總傷亡人數
    total_casualties = death_count + injured_count
  )

cat("死亡人數統計:\n")
print(table(traffic_data$death_count))

cat("受傷人數統計:\n")
print(table(traffic_data$injured_count))

cat("總傷亡人數統計:\n")
print(summary(traffic_data$total_casualties))

# ============================================================================
# 步驟4：處理車種
# ============================================================================

cat("步驟4: 處理車種\n")

cat("原始車種範例:\n")
print(head(traffic_data$車種, 20))

cat("車種統計 (原始):\n")
print(table(traffic_data$車種))

cat("方案 A: 創建車種標誌變數 (含其他)\n\n")

traffic_data <- traffic_data %>%
  mutate(
    # 檢查是否包含各種車型
    has_passenger_car = ifelse(str_detect(車種, "小客|轎車|轎跑|休旅|SUV"), 1, 0),
    has_motorcycle = ifelse(str_detect(車種, "機車|電動機|重機"), 1, 0),
    has_truck = ifelse(str_detect(車種, "大貨|貨車|卡車"), 1, 0),
    has_bus = ifelse(str_detect(車種, "公車|客車|遊覽|巴士"), 1, 0),
    has_small_truck = ifelse(str_detect(車種, "小貨"), 1, 0),
    has_taxi = ifelse(str_detect(車種, "計程|出租"), 1, 0),
    has_non_motor = ifelse(str_detect(車種, "人|自行車"), 1, 0),
    
    # 其他: 不屬於以上任何分類
    has_other = ifelse(!(has_passenger_car | has_motorcycle | has_truck | has_bus | 
                           has_small_truck | has_taxi | has_non_motor), 1, 0),
    
    # 計算涉及的車種數量
    vehicle_count = has_passenger_car + has_motorcycle + has_truck + 
      has_bus + has_small_truck + has_taxi + has_non_motor + has_other,
    
    # 判斷是否為多車種事故 (涉及 2 種或以上)
    is_multi_vehicle = ifelse(vehicle_count >= 2, 1, 0)
  )

cat("✓ 車種標誌變數已建立\n\n")


# ============================================================================
# 步驟5：新增額外衍生變數
# ============================================================================

cat("步驟5: 新增衍生變數\n")

traffic_data <- traffic_data %>%
  mutate(
    # 事故類型分類
    incident_type = case_when(
      death_count > 0 ~ "A1",
      injured_count > 0 ~ "A2"
    ),
    # 季節
    season = case_when(
      month %in% c(12, 1, 2) ~ "冬季",
      month %in% c(3, 4, 5) ~ "春季",
      month %in% c(6, 7, 8) ~ "夏季",
      month %in% c(9, 10, 11) ~ "秋季"
    ),
    # 月份名稱
    month_name = month.abb[month]
  )


# ============================================================================
# 步驟6：數據驗證和清理
# ============================================================================

cat("步驟6: 數據驗證和清理\n")

# 檢查缺失值
cat("缺失值檢查:\n")
missing_check <- data.frame(
  欄位 = names(traffic_data),
  缺失值數量 = sapply(traffic_data, function(x) sum(is.na(x))),
  缺失率 = round(sapply(traffic_data, function(x) sum(is.na(x))) / nrow(traffic_data) * 100, 2)
)
print(missing_check)

# 移除必要欄位的 NA 值
traffic_data_clean <- traffic_data %>%
  filter(
    !is.na(ad_year) &
      !is.na(month) &
      !is.na(county_city) 
  )

cat("清理前:", nrow(traffic_data), "筆\n")
cat("清理後:", nrow(traffic_data_clean), "筆\n")
cat("移除:", nrow(traffic_data) - nrow(traffic_data_clean), "筆\n\n")

# 更新為清理後的數據
traffic_data <- traffic_data_clean

# ============================================================================
# 步驟7：產生清理後的統計表
# ============================================================================

cat("步驟7: 清理後數據統計\n")

# 年度統計
cat("年度統計:\n\n")
yearly_stats <- traffic_data %>%
  group_by(ad_year) %>%
  summarise(
    事故件數 = n(),
    多車種事故 = sum(is_multi_vehicle, na.rm = TRUE),
    死亡人數 = sum(death_count, na.rm = TRUE),
    受傷人數 = sum(injured_count, na.rm = TRUE),
    總傷亡 = sum(total_casualties, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ad_year)
print(yearly_stats)

# 主要車種統計 (含其他)
cat("主要車種統計 (含其他):\n\n")
vehicle_stats <- traffic_data %>%
  group_by(is_multi_vehicle) %>%
  summarise(
    事故件數 = n(),
    涉及多車種 = sum(is_multi_vehicle, na.rm = TRUE),
    多車種比例 = round(sum(is_multi_vehicle, na.rm = TRUE) / n() * 100, 2),
    死亡人數 = sum(death_count, na.rm = TRUE),
    受傷人數 = sum(injured_count, na.rm = TRUE),
    平均傷亡 = round(mean(total_casualties, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(事故件數))
print(vehicle_stats)

# 多車種事故統計
cat("多車種事故統計:\n\n")
multi_vehicle_stats <- traffic_data %>%
  group_by(is_multi_vehicle) %>%
  summarise(
    事故件數 = n(),
    死亡人數 = sum(death_count, na.rm = TRUE),
    受傷人數 = sum(injured_count, na.rm = TRUE),
    平均傷亡 = round(mean(total_casualties, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  mutate(類型 = ifelse(is_multi_vehicle == 1, "多車種", "單一車種"))
print(multi_vehicle_stats)

# 各車型涉及統計 (含其他)
cat("各車型涉及事故統計 (含其他):\n\n")
vehicle_involvement <- data.frame(
  車型 = c("小客車", "機車", "大貨車", "公車", "小貨車", "計程車", "非機動", "其他"),
  涉及事故 = c(
    sum(traffic_data$has_passenger_car, na.rm = TRUE),
    sum(traffic_data$has_motorcycle, na.rm = TRUE),
    sum(traffic_data$has_truck, na.rm = TRUE),
    sum(traffic_data$has_bus, na.rm = TRUE),
    sum(traffic_data$has_small_truck, na.rm = TRUE),
    sum(traffic_data$has_taxi, na.rm = TRUE),
    sum(traffic_data$has_non_motor, na.rm = TRUE),
    sum(traffic_data$has_other, na.rm = TRUE)
  )
) %>%
  arrange(desc(涉及事故))
print(vehicle_involvement)

# 縣市統計
cat("縣市統計:\n\n")
county_stats <- traffic_data %>%
  group_by(county_city) %>%
  summarise(
    事故件數 = n(),
    多車種事故 = sum(is_multi_vehicle, na.rm = TRUE),
    死亡人數 = sum(death_count, na.rm = TRUE),
    受傷人數 = sum(injured_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(事故件數))
print(county_stats)

# 月份統計
cat("月份統計:\n\n")
monthly_stats <- traffic_data %>%
  group_by(month) %>%
  summarise(
    事故件數 = n(),
    多車種事故 = sum(is_multi_vehicle, na.rm = TRUE),
    死亡人數 = sum(death_count, na.rm = TRUE),
    受傷人數 = sum(injured_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(月份名稱 = month.abb[month]) %>%
  arrange(month)
print(monthly_stats)

# 季節統計
cat("季節統計:\n\n")
season_stats <- traffic_data %>%
  group_by(season) %>%
  summarise(
    事故件數 = n(),
    多車種事故 = sum(is_multi_vehicle, na.rm = TRUE),
    死亡人數 = sum(death_count, na.rm = TRUE),
    受傷人數 = sum(injured_count, na.rm = TRUE),
    .groups = "drop"
  )
print(season_stats)

# ============================================================================
# 步驟8：輸出清理後的數據
# ============================================================================

cat("步驟8: 保存清理後的數據\n")

# 選擇要保存的欄位
traffic_data_output <- traffic_data %>%
  dplyr::select(
    ad_year,
    month,
    month_name,
    season,
    county_city,
    is_multi_vehicle,
    vehicle_count,
    has_passenger_car,
    has_motorcycle,
    has_truck,
    has_bus,
    has_small_truck,
    has_taxi,
    has_non_motor,
    has_other,
    incident_type,
    death_count,
    injured_count,
    total_casualties
  )

# 保存為 CSV
# write.csv(traffic_data_output, "E:/碩一/consulation/final data/traffic_accident_cleaned.csv", 
#          row.names = FALSE, fileEncoding = "UTF-8")

cat("✓ 清理後的數據已保存: traffic_accident_cleaned.csv\n")
cat("  共", nrow(traffic_data_output), "筆記錄\n")
cat("  共", ncol(traffic_data_output), "個欄位\n\n")

# ============================================================================
# 基於清理數據的完整分析
# ============================================================================

cat("台灣交通事故分析 - 完整版本\n")

library(tidyverse)
library(ggplot2)
library(forecast)
library(lattice)
library(caret)
library(randomForest)
library(ranger)
library(Matrix)
library(glmnet)
library(xgboost)
library(e1071)

# ============================================================================
# PHASE 0: 讀取清理後的數據
# ============================================================================

cat("PHASE 0: 讀取清理後的數據\n\n")

# 讀取清理後的數據
traffic_data <- read.csv("E:/碩一/consulation/final data/traffic_accident_cleaned.csv")

cat("清理後的數據資訊:\n")
cat("  行數:", nrow(traffic_data), "\n")
cat("  列數:", ncol(traffic_data), "\n")
cat("  欄位名稱:\n")
print(names(traffic_data))

cat("前5筆數據:\n")
print(head(traffic_data, 5))

# ============================================================================
# PHASE 1: 描述性統計分析
# ============================================================================

cat("PHASE 1: 描述性統計分析\n")

# 年度統計
cat("1.1 年度統計:\n\n")
yearly_stats <- traffic_data %>%
  group_by(ad_year) %>%
  summarise(
    事故件數 = n(),
    死亡人數 = sum(death_count, na.rm = TRUE),
    受傷人數 = sum(injured_count, na.rm = TRUE),
    總傷亡 = sum(total_casualties, na.rm = TRUE),
    A1事故 = sum(incident_type == "A1", na.rm = TRUE),
    A2事故 = sum(incident_type == "A2", na.rm = TRUE),
    多車種事故 = sum(is_multi_vehicle, na.rm = TRUE),
    A1比率 = round(sum(incident_type == "A1", na.rm = TRUE) / n() * 100, 2),
    平均傷亡 = round(mean(total_casualties, na.rm = TRUE), 2),
    .groups = "drop"
  )
print(yearly_stats)

# 縣市統計
cat("1.2 縣市統計:\n\n")
county_stats <- traffic_data %>%
  group_by(county_city) %>%
  summarise(
    事故件數 = n(),
    多車種事故 = sum(is_multi_vehicle, na.rm = TRUE),
    死亡人數 = sum(death_count, na.rm = TRUE),
    受傷人數 = sum(injured_count, na.rm = TRUE),
    A1事故 = sum(incident_type == "A1", na.rm = TRUE),
    A1比率 = round(sum(incident_type == "A1", na.rm = TRUE) / n() * 100, 2),
    平均傷亡 = round(mean(total_casualties), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(事故件數))
print(county_stats)

# 車型統計 (基於標誌變數)
cat("1.3 車型統計 (基於標誌變數):\n\n")
vehicle_stats_by_type <- data.frame(
  車型 = c("小客車", "機車", "大貨車", "公車", "小貨車", "計程車", "非機動", "其他"),
  涉及事故 = c(
    sum(traffic_data$has_passenger_car, na.rm = TRUE),
    sum(traffic_data$has_motorcycle, na.rm = TRUE),
    sum(traffic_data$has_truck, na.rm = TRUE),
    sum(traffic_data$has_bus, na.rm = TRUE),
    sum(traffic_data$has_small_truck, na.rm = TRUE),
    sum(traffic_data$has_taxi, na.rm = TRUE),
    sum(traffic_data$has_non_motor, na.rm = TRUE),
    sum(traffic_data$has_other, na.rm = TRUE)
  )
) %>%
  mutate(佔比 = round(涉及事故 / nrow(traffic_data) * 100, 2)) %>%
  arrange(desc(涉及事故))

print(vehicle_stats_by_type)

# 月份統計
cat("1.4 月份統計:\n\n")
monthly_stats <- traffic_data %>%
  group_by(month) %>%
  summarise(
    事故件數 = n(),
    死亡人數 = sum(death_count, na.rm = TRUE),
    受傷人數 = sum(injured_count, na.rm = TRUE),
    A1比率 = round(sum(incident_type == "A1", na.rm = TRUE) / n() * 100, 2),
    平均傷亡 = round(mean(total_casualties), 2),
    .groups = "drop"
  ) %>%
  mutate(月份 = month.abb[month])
print(monthly_stats)

# 季節統計
cat("1.5 季節統計:\n\n")
season_stats <- traffic_data %>%
  group_by(season) %>%
  summarise(
    事故件數 = n(),
    多車種事故 = sum(is_multi_vehicle, na.rm = TRUE),
    死亡人數 = sum(death_count, na.rm = TRUE),
    受傷人數 = sum(injured_count, na.rm = TRUE),
    A1比率 = round(sum(incident_type == "A1", na.rm = TRUE) / n() * 100, 2),
    平均傷亡 = mean(total_casualties),
    .groups = "drop"
  )
print(season_stats)

# 事故類型統計
cat("1.6 事故類型統計:\n\n")
type_stats <- traffic_data %>%
  group_by(incident_type) %>%
  summarise(
    事故件數 = n(),
    死亡人數 = sum(death_count, na.rm = TRUE),
    受傷人數 = sum(injured_count, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(!is.na(incident_type))  # 移除 NA
print(type_stats)

# ============================================================================
# PHASE 2: 相關性分析
# ============================================================================

cat("\nPHASE 2: 相關性分析\n")

# 準備數值型數據
numeric_vars <- traffic_data %>%
  dplyr::select(
    is_multi_vehicle, vehicle_count,
    has_passenger_car, has_motorcycle, has_truck, has_bus,
    has_small_truck, has_taxi, has_non_motor, has_other,
    death_count, injured_count, total_casualties
  )

# 相關矩陣
cor_matrix <- cor(numeric_vars, use = "complete.obs")

cat("【關鍵相關係數 - 與傷亡的關係】\n")
casualty_cors <- cor_matrix[, "total_casualties"]
print(sort(casualty_cors, decreasing = TRUE))
# 轉換為長格式
library(reshape2)
cor_melted <- melt(cor_matrix)
names(cor_melted) <- c("Var1", "Var2", "Correlation")

p_cor <- ggplot(cor_melted, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white", size = 0.5) +
  geom_text(aes(label = round(Correlation, 2)), color = "black", size = 2.5) +
  scale_fill_gradient2(
    low = "#D73027",      # 負相關 (紅色)
    mid = "white",        # 零相關 (白色)
    high = "#4575B4",     # 正相關 (藍色)
    midpoint = 0,
    limit = c(-1, 1),
    name = "相關係數"
  ) +
  labs(
    title = "交通事故變數相關矩陣熱力圖",
    subtitle = "紅色 = 負相關 | 白色 = 無相關 | 藍色 = 正相關",
    x = "", y = ""
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5, size = 10, color = "gray50"),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y = element_text(size = 9),
    legend.position = "right",
    panel.grid = element_blank()
  ) +
  coord_fixed()

print(p_cor)

# ============================================================================
# PHASE 3: 時間序列模型
# ============================================================================

cat("PHASE 3: 時間序列模型\n")

# 按月聚合
monthly_agg <- traffic_data %>%
  group_by(ad_year, month) %>%
  summarise(
    incidents = n(),
    deaths = sum(death_count, na.rm = TRUE),
    injured = sum(injured_count, na.rm = TRUE),
    a1_count = sum(incident_type == "A1", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(ad_year, month)

# 建立時序
ts_incidents <- ts(monthly_agg$incidents, start = c(2013, 1), frequency = 12)
ts_deaths <- ts(monthly_agg$deaths, start = c(2013, 1), frequency = 12)

# 分割
n_total <- length(ts_incidents)
n_train <- floor(n_total * 0.8)
train_ts <- window(ts_incidents, end = c(2013 + (n_train - 1) %/% 12, ((n_train - 1) %% 12) + 1))
test_ts <- window(ts_incidents, start = c(2013 + n_train %/% 12, (n_train %% 12) + 1))

cat("訓練集:", length(train_ts), "月 | 測試集:", length(test_ts), "月\n\n")

# 時序模型結果儲存
ts_results <- data.frame()

#評估函數
eval_ts <- function(actual, predicted, name) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  mape <- mean(abs((actual - predicted) / actual)) * 100
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2 <- 1 - (ss_res / ss_tot)
  data.frame(Model = name, RMSE = rmse, MAE = mae, MAPE = mape, R2 = r2)
}

# 3.1 ARIMA
cat("訓練 ARIMA...\n")
tryCatch({
  arima_model <- auto.arima(train_ts, seasonal = TRUE, stepwise = FALSE, approximation = FALSE)
  arima_pred <- forecast(arima_model, h = length(test_ts))$mean
  ts_results <- rbind(ts_results, eval_ts(as.numeric(test_ts), as.numeric(arima_pred), "ARIMA"))
  cat("  ✓ ARIMA:", arima_model$arma, "\n")
}, error = function(e) cat("  ✗ ARIMA 失敗\n"))


# 3.2 ETS
cat("訓練 ETS...\n")
tryCatch({
  ets_model <- ets(train_ts)
  ets_pred <- forecast(ets_model, h = length(test_ts))$mean
  ts_results <- rbind(ts_results, eval_ts(as.numeric(test_ts), as.numeric(ets_pred), "ETS"))
  cat("  ✓ ETS:", ets_model$method, "\n")
}, error = function(e) cat("  ✗ ETS 失敗\n"))

# 3.3 Holt-Winters
cat("訓練 Holt-Winters...\n")
tryCatch({
  hw_model <- HoltWinters(train_ts)
  hw_pred <- forecast(hw_model, h = length(test_ts))$mean
  ts_results <- rbind(ts_results, eval_ts(as.numeric(test_ts), as.numeric(hw_pred), "Holt-Winters"))
  cat("  ✓ Holt-Winters 完成\n")
}, error = function(e) cat("  ✗ Holt-Winters 失敗\n"))

# 3.4 TBATS (複雜季節性)
cat("訓練 TBATS...\n")
tryCatch({
  tbats_model <- tbats(train_ts)
  tbats_pred <- forecast(tbats_model, h = length(test_ts))$mean
  ts_results <- rbind(ts_results, eval_ts(as.numeric(test_ts), as.numeric(tbats_pred), "TBATS"))
  cat("  ✓ TBATS 完成\n")
}, error = function(e) cat("  ✗ TBATS 失敗\n"))

# 3.5 NNAR (神經網路自迴歸)
cat("訓練 NNAR...\n")
tryCatch({
  set.seed(2025)
  nnar_model <- nnetar(train_ts)
  nnar_pred <- forecast(nnar_model, h = length(test_ts))$mean
  ts_results <- rbind(ts_results, eval_ts(as.numeric(test_ts), as.numeric(nnar_pred), "NNAR"))
  cat("  ✓ NNAR:", nnar_model$method, "\n")
}, error = function(e) cat("  ✗ NNAR 失敗\n"))

cat("\n【時間序列模型結果】\n")
print(ts_results %>% arrange(RMSE))


# ============================================================================
# PHASE 4: 機器學習模型
# ============================================================================

cat("PHASE 4: 機器學習模型\n")

# 準備 ML 數據
ml_data <- traffic_data %>%
  group_by(ad_year, month) %>%
  summarise(
    total_incidents = n(),
    total_deaths = sum(death_count, na.rm = TRUE),
    total_injured = sum(injured_count, na.rm = TRUE),
    total_casualties = sum(total_casualties, na.rm = TRUE),
    a1_count = sum(incident_type == "A1", na.rm = TRUE),
    a2_count = sum(incident_type == "A2", na.rm = TRUE),
    multi_vehicle = sum(is_multi_vehicle, na.rm = TRUE),
    motorcycle_count = sum(has_motorcycle, na.rm = TRUE),
    car_count = sum(has_passenger_car, na.rm = TRUE),
    truck_count = sum(has_truck, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    quarter = ceiling(month / 3),
    is_summer = ifelse(month %in% c(6, 7, 8), 1, 0),
    is_winter = ifelse(month %in% c(12, 1, 2), 1, 0),
    year_trend = ad_year - min(ad_year)
  )

# 分割數據
set.seed(2025)
trainIndex <- createDataPartition(ml_data$total_incidents, p = 0.8, list = FALSE)
train_ml <- ml_data[trainIndex, ]
test_ml <- ml_data[-trainIndex, ]

cat("ML 數據: ", nrow(train_ml), " 訓練樣本, ", nrow(test_ml), " 測試樣本\n\n")

# 評估函數
eval_ml <- function(actual, predicted, name) {
  rmse <- sqrt(mean((actual - predicted)^2))
  mae <- mean(abs(actual - predicted))
  mape <- mean(abs((actual - predicted) / actual)) * 100
  ss_res <- sum((actual - predicted)^2)
  ss_tot <- sum((actual - mean(actual))^2)
  r2 <- 1 - (ss_res / ss_tot)
  data.frame(Model = name, RMSE = rmse, MAE = mae, MAPE = mape, R2 = r2)
}

ml_results <- data.frame()

# 定義特徵和目標
features <- setdiff(names(train_ml), c("total_incidents", "ad_year"))
X_train <- as.matrix(train_ml[, features])
y_train <- train_ml$total_incidents
X_test <- as.matrix(test_ml[, features])
y_test <- test_ml$total_incidents

# 4.1 線性迴歸
cat("4.1 線性迴歸")
tryCatch({
  lm_model <- lm(total_incidents ~ ., data = train_ml[, c("total_incidents", features)])
  lm_pred <- predict(lm_model, test_ml)
  ml_results <- rbind(ml_results, eval_ml(y_test, lm_pred, "線性迴歸"))
  cat(" ✓\n")
}, error = function(e) cat(" ✗\n"))

# 4.2 Ridge
cat("4.2 Ridge 迴歸")
tryCatch({
  ridge_cv <- cv.glmnet(X_train, y_train, alpha = 0, nfolds = 5)
  ridge_pred <- predict(ridge_cv, X_test, s = ridge_cv$lambda.min)[, 1]
  ml_results <- rbind(ml_results, eval_ml(y_test, ridge_pred, "Ridge"))
  cat(" ✓\n")
}, error = function(e) cat(" ✗\n"))

# 4.3 Lasso
cat("4.3 Lasso 迴歸")
tryCatch({
  lasso_cv <- cv.glmnet(X_train, y_train, alpha = 1, nfolds = 5)
  lasso_pred <- predict(lasso_cv, X_test, s = lasso_cv$lambda.min)[, 1]
  ml_results <- rbind(ml_results, eval_ml(y_test, lasso_pred, "Lasso"))
  cat(" ✓\n")
}, error = function(e) cat(" ✗\n"))

# 4.4 Elastic Net
cat("4.4 Elastic Net")
tryCatch({
  enet_cv <- cv.glmnet(X_train, y_train, alpha = 0.5, nfolds = 5)
  enet_pred <- predict(enet_cv, X_test, s = enet_cv$lambda.min)[, 1]
  ml_results <- rbind(ml_results, eval_ml(y_test, enet_pred, "ElasticNet"))
  cat(" ✓\n")
}, error = function(e) cat(" ✗\n"))


# 4.5 隨機森林
cat("4.5 隨機森林")
tryCatch({
  set.seed(2025)
  rf_model <- randomForest(
    x = X_train, y = y_train,
    ntree = 500, mtry = floor(sqrt(ncol(X_train))),
    importance = TRUE
  )
  rf_pred <- predict(rf_model, X_test)
  ml_results <- rbind(ml_results, eval_ml(y_test, rf_pred, "隨機森林"))
  cat(" ✓\n")
}, error = function(e) cat(" ✗\n"))

# 4.6 XGBoost
cat("4.6 XGBoost")
tryCatch({
  dtrain <- xgb.DMatrix(X_train, label = y_train)
  dtest <- xgb.DMatrix(X_test, label = y_test)
  
  set.seed(2025)
  xgb_model <- xgb.train(
    params = list(
      objective = "reg:squarederror",
      max_depth = 6,
      eta = 0.1,
      subsample = 0.8,
      colsample_bytree = 0.8
    ),
    data = dtrain,
    nrounds = 200,
    watchlist = list(train = dtrain, test = dtest),
    early_stopping_rounds = 20,
    verbose = 0
  )
  xgb_pred <- predict(xgb_model, dtest)
  ml_results <- rbind(ml_results, eval_ml(y_test, xgb_pred, "XGBoost"))
  cat(" ✓\n")
}, error = function(e) cat(" ✗\n"))

cat("\n【機器學習模型結果 - 迴歸】\n")
print(ml_results %>% arrange(RMSE))

# ============================================================================
# PHASE 5: 分類模型 - 預測事故嚴重程度 (A1/A2)
# ============================================================================

cat("\nPHASE 5: 分類模型 - 事故嚴重程度預測 (A1/A2)\n")
library(pROC)
# 準備分類數據
class_data <- traffic_data %>%
  filter(!is.na(incident_type)) %>%
  mutate(
    is_severe = factor(ifelse(incident_type == "A1", 1, 0), levels = c(0, 1)),
    month_sin = sin(2 * pi * month / 12),
    month_cos = cos(2 * pi * month / 12)
  ) %>%
  dplyr::select(
    is_severe, ad_year, month, month_sin, month_cos,
    is_multi_vehicle, vehicle_count,
    has_passenger_car, has_motorcycle, has_truck, has_bus,
    has_small_truck, has_taxi, has_non_motor, has_other
  )

# 處理類別不平衡 - 查看比例
cat("類別分布:\n")
print(table(class_data$is_severe))
cat("A1 比例:", round(mean(class_data$is_severe == 1) * 100, 2), "%\n\n")

# 分割數據
set.seed(2025)
trainIndex_class <- createDataPartition(class_data$is_severe, p = 0.8, list = FALSE)
train_class <- class_data[trainIndex_class, ]
test_class <- class_data[-trainIndex_class, ]

cat("訓練集:", nrow(train_class), "筆\n")
cat("測試集:", nrow(test_class), "筆\n\n")

#建立平衡數據集 (下採樣)
set.seed(2025)
# 分離兩類
class_0 <- train_class %>% filter(is_severe == 0)
class_1 <- train_class %>% filter(is_severe == 1)

cat("原始分布:\n")
cat("  A2 (is_severe=0):", nrow(class_0), "筆\n")
cat("  A1 (is_severe=1):", nrow(class_1), "筆\n")

# 下採樣多數類
n_minority <- nrow(class_1)
class_0_downsampled <- class_0 %>% sample_n(min(n_minority * 3, nrow(class_0)))

# 合併並打亂
train_balanced <- rbind(class_0_downsampled, class_1)
train_balanced <- train_balanced[sample(nrow(train_balanced)), ]

cat("\n平衡後分布:\n")
cat("  A2 (is_severe=0):", sum(train_balanced$is_severe == 0), "筆\n")
cat("  A1 (is_severe=1):", sum(train_balanced$is_severe == 1), "筆\n\n")

# 分類評估函數
eval_class <- function(actual, predicted, prob = NULL, name) {
  cm <- confusionMatrix(predicted, actual, positive = "1")
  acc <- cm$overall["Accuracy"]
  sens <- cm$byClass["Sensitivity"]
  spec <- cm$byClass["Specificity"]
  prec <- cm$byClass["Precision"]
  f1 <- cm$byClass["F1"]
  
  auc_val <- NA
  if (!is.null(prob) && length(unique(prob)) > 1) {
    tryCatch({
      roc_obj <- pROC::roc(actual, prob, quiet = TRUE)
      auc_val <- as.numeric(pROC::auc(roc_obj))
    }, error = function(e) {
      cat("AUC 計算錯誤:", e$message, "\n")
    })
  }
  
  data.frame(
    Model = name,
    Accuracy = round(acc, 4),
    Sensitivity = round(sens, 4),
    Specificity = round(spec, 4),
    Precision = round(ifelse(is.na(prec), 0, prec), 4),
    F1 = round(ifelse(is.na(f1), 0, f1), 4),
    AUC = round(auc_val, 4)
  )
}

class_results <- data.frame()

# 5.1 邏輯迴歸
cat("1. 邏輯迴歸 (Ridge正則化)")
tryCatch({
  X_train_bal <- as.matrix(train_balanced[, -1])
  y_train_bal <- as.numeric(as.character(train_balanced$is_severe))
  X_test <- as.matrix(test_class[, -1])
  
  logit_cv <- cv.glmnet(X_train_bal, y_train_bal, family = "binomial", alpha = 0, nfolds = 5)
  logit_prob <- predict(logit_cv, X_test, s = "lambda.min", type = "response")[, 1]
  
  threshold <- 0.3
  logit_pred <- factor(ifelse(logit_prob > threshold, 1, 0), levels = c(0, 1))
  
  class_results <- rbind(class_results, 
                         eval_class(test_class$is_severe, logit_pred, logit_prob, "邏輯迴歸"))
  cat(" ✓\n")
}, error = function(e) cat(" ✗:", e$message, "\n"))


# 5.2 隨機森林分類
cat("2. 隨機森林")
tryCatch({
  set.seed(2025)
  rf_ranger <- ranger(
    is_severe ~ ., 
    data = train_balanced, 
    num.trees = 500,
    probability = TRUE,
    importance = "impurity"
  )
  
  rf_pred_obj <- predict(rf_ranger, test_class)
  rf_class_prob <- rf_pred_obj$predictions[, "1"]
  
  threshold <- 0.3
  rf_class_pred <- factor(ifelse(rf_class_prob > threshold, 1, 0), levels = c(0, 1))
  
  class_results <- rbind(class_results, 
                         eval_class(test_class$is_severe, rf_class_pred, rf_class_prob, "隨機森林"))
  cat(" ✓\n")
}, error = function(e) cat(" ✗:", e$message, "\n"))


# 5.3 XGBoost 分類
cat("3. XGBoost")
tryCatch({
  X_train <- as.matrix(train_class[, -1])
  y_train <- as.numeric(as.character(train_class$is_severe))
  X_test <- as.matrix(test_class[, -1])
  
  dtrain <- xgb.DMatrix(X_train, label = y_train)
  dtest <- xgb.DMatrix(X_test)
  
  weight_ratio <- sum(y_train == 0) / sum(y_train == 1)
  
  set.seed(2025)
  xgb_class <- xgb.train(
    params = list(
      objective = "binary:logistic",
      eval_metric = "auc",
      max_depth = 5,
      eta = 0.1,
      scale_pos_weight = weight_ratio
    ),
    data = dtrain,
    nrounds = 100,
    verbose = 0
  )
  
  xgb_class_prob <- predict(xgb_class, dtest)
  
  threshold <- 0.3
  xgb_class_pred <- factor(ifelse(xgb_class_prob > threshold, 1, 0), levels = c(0, 1))
  
  class_results <- rbind(class_results, 
                         eval_class(test_class$is_severe, xgb_class_pred, xgb_class_prob, "XGBoost"))
  cat(" ✓\n")
}, error = function(e) cat(" ✗:", e$message, "\n"))

cat("\n【分類模型結果】\n")
print(class_results %>% arrange(desc(AUC)))

# ============================================================================
# PHASE 6: 特徵重要性分析
# ============================================================================

cat("\nPHASE 6: 特徵重要性分析\n")

# 6.1 隨機森林特徵重要性 (迴歸)
if (exists("rf_model")) {
  cat("\n【隨機森林特徵重要性 - 事故數預測】\n")
  
  # 明確使用 randomForest 套件的 importance 函數
  rf_importance <- randomForest::importance(rf_model)
  
  rf_imp_df <- data.frame(
    Feature = rownames(rf_importance),
    Importance = rf_importance[, "%IncMSE"]
  ) %>%
    arrange(desc(Importance))
  
  print(head(rf_imp_df, 10))
  
  # 視覺化
  varImpPlot(rf_model, main = "隨機森林特徵重要性")
}

# 6.2 XGBoost 特徵重要性
if (exists("xgb_model")) {
  cat("\n【XGBoost 特徵重要性 - 事故數預測】\n")
  xgb_importance <- xgb.importance(model = xgb_model)
  print(head(xgb_importance, 10))
  
  # 視覺化
  xgb.plot.importance(xgb_importance, top_n = 10, main = "XGBoost 特徵重要性")
}

# 6.3 分類模型特徵重要性
if (exists("rf_ranger")) {
  cat("\n【Ranger 特徵重要性 - A1/A2 分類】\n")
  
  # ranger 的重要性直接存在模型物件中
  rf_ranger_imp <- rf_ranger$variable.importance
  
  rf_ranger_imp_df <- data.frame(
    Feature = names(rf_ranger_imp),
    Importance = rf_ranger_imp
  ) %>%
    arrange(desc(Importance))
  
  print(head(rf_ranger_imp_df, 10))
  
  # 視覺化
  ggplot(head(rf_ranger_imp_df, 10), aes(x = reorder(Feature, Importance), y = Importance)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Ranger 特徵重要性 (A1/A2 分類)", x = "特徵", y = "重要性") +
    theme_minimal()
}


# ============================================================================
# PHASE 7: 模型比較
# ============================================================================

cat("PHASE 7: 模型評估與比較\n")

# 7.1 時間序列 vs 機器學習
cat("\n【迴歸模型綜合比較】\n")
all_regression <- rbind(
  ts_results %>% mutate(類型 = "時間序列"),
  ml_results %>% mutate(類型 = "機器學習")
) %>%
  arrange(RMSE)

print(all_regression)

best_reg <- all_regression[1, ]
cat("\n✓ 最佳迴歸模型:", best_reg$Model, "\n")
cat("  RMSE:", round(best_reg$RMSE, 2), "\n")
cat("  MAE:", round(best_reg$MAE, 2), "\n")
cat("  MAPE:", round(best_reg$MAPE, 2), "%\n")
cat("  R²:", round(best_reg$R2, 4), "\n")

# 7.2 分類模型最佳
if (nrow(class_results) > 0) {
  best_class <- class_results[which.max(class_results$AUC), ]
  cat("\n✓ 最佳分類模型:", best_class$Model, "\n")
  cat("  AUC:", round(best_class$AUC, 4), "\n")
  cat("  F1:", round(best_class$F1, 4), "\n")
  cat("  Accuracy:", round(best_class$Accuracy, 4), "\n")
}


cat("所有機器學習模型比較 (按RMSE排序):\n\n")

ml_results_sorted <- ml_results %>%
  arrange(RMSE)

print(ml_results_sorted)

# 最優機器學習模型
ml_best_idx <- which.min(ml_results$RMSE)
ml_best_model <- ml_results$Model[ml_best_idx]
ml_best_rmse <- ml_results$RMSE[ml_best_idx]
ml_best_r2 <- ml_results$R2[ml_best_idx]
ml_best_mape <- ml_results$MAPE[ml_best_idx]


# 與時序模型比較
cat("機器學習 vs 時序模型\n")

# 合併所有模型結果
all_models <- rbind(
  ts_results,
  ml_results
) %>%
  arrange(RMSE)

cat("所有模型綜合比較:\n\n")
print(all_models)

best_overall_idx <- which.min(all_models$RMSE)
best_overall_model <- all_models$Model[best_overall_idx]
best_overall_rmse <- all_models$RMSE[best_overall_idx]
best_overall_r2 <- all_models$R2[best_overall_idx]
best_overall_mape <- all_models$MAPE[best_overall_idx]

cat("✓ 最優模型 (綜合):", best_overall_model, "\n")
cat("  RMSE:", round(best_overall_rmse, 3), "\n")
cat("  MAPE:", round(best_overall_mape, 3), "%\n\n")
cat("  R²:    ", round(best_overall_r2, 4), "")

# 模型性能視覺化
# 機器學習模型RMSE對比
p_ml <- ggplot(ml_results_sorted, aes(x = reorder(Model, RMSE), y = RMSE)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = round(RMSE, 1)), vjust = -0.3, size = 3) +
  coord_flip() +
  labs(title = "機器學習模型性能比較 (RMSE)",
       subtitle = "5個模型: 線性迴歸、Ridge、Lasso、隨機森林、XGBoost",
       x = "模型", y = "RMSE") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11))

print(p_ml)
cat("✓ 機器學習模型性能圖已顯示\n\n")

# 所有模型RMSE對比
p_all <- ggplot(all_models %>% arrange(RMSE), 
                aes(x = reorder(Model, RMSE), y = RMSE, fill = ifelse(grepl("迴歸|Ridge|Lasso|XGBoost", Model), "機器學習", "時序"))) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = round(RMSE, 1)), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("時序" = "coral", "機器學習" = "steelblue")) +
  coord_flip() +
  labs(title = "所有模型綜合性能比較",
       subtitle = "8個模型: ARIMA、ETS、HW + 線性迴歸、Ridge、Lasso、隨機森林、XGBoost",
       x = "模型", y = "RMSE", fill = "模型類型") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5, size = 11),
        legend.position = "right")

print(p_all)
cat("✓ 所有模型性能圖已顯示\n\n")

# MAPE對比
p_mape <- ggplot(all_models %>% arrange(MAPE), 
                 aes(x = reorder(Model, MAPE), y = MAPE)) +
  geom_bar(stat = "identity", fill = "coral", alpha = 0.7) +
  geom_text(aes(label = round(MAPE, 2)), vjust = -0.3, size = 3) +
  coord_flip() +
  labs(title = "模型MAPE對比 (越低越好)",
       x = "模型", y = "MAPE (%)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))

print(p_mape)
cat("✓ MAPE對比圖已顯示\n\n")

# ============================================================================
# PHASE 8: 視覺化
# ============================================================================

cat("PHASE 8: 視覺化\n")

# 圖表 1: 年度趨勢
cat("生成圖表 1: 年度趨勢...\n")
p1 <- ggplot(yearly_stats, aes(x = ad_year, y = 事故件數)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(size = 3, color = "steelblue") +
  geom_smooth(method = "loess", color = "red", se = FALSE, alpha = 0.3) +
  labs(title = "交通事故年度趨勢 (2013-2019)",
       subtitle = "藍線: 實際數據  |  紅線: LOESS平滑趨勢",
       x = "年份", y = "事故件數") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 12))
print(p1)

p11 <- ggplot(yearly_stats, aes(x = ad_year)) +
  geom_line(aes(y = 事故件數, color = "事故件數"), size = 1.2) +
  geom_point(aes(y = 事故件數, color = "事故件數"), size = 3) +
  geom_line(aes(y = 死亡人數 * 100, color = "死亡人數(×100)"), size = 1, linetype = "dashed") +
  scale_color_manual(values = c("事故件數" = "steelblue", "死亡人數(×100)" = "coral")) +
  labs(title = "交通事故年度趨勢 (2013-2019)",
       x = "年份", y = "件數", color = "") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.position = "bottom")

print(p11)
cat("✓ 圖1: 年度趨勢\n")

# 圖表 2: 車型對比
cat("生成圖表 2: 車型對比...\n")
p2 <- ggplot(vehicle_stats_by_type, 
             aes(x = reorder(車型, 涉及事故), y = 涉及事故, fill = 車型)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = paste0(涉及事故, " (", 佔比, "%)")), hjust = 0.5, size = 3) +
  coord_flip() +
  scale_fill_viridis_d() +
  labs(title = "車型事故統計 (2013-2019)", x = "車型", y = "涉及事故件數") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")
print(p2)

# 圖表 3: 縣市
cat("生成圖表 3: 縣市排名...\n")
p3 <- ggplot(county_stats, aes(x = reorder(county_city, 事故件數), y = 事故件數)) +
  geom_bar(stat = "identity", fill = "steelblue", alpha = 0.7) +
  geom_text(aes(label = 事故件數), hjust = 0.3, size = 3) +
  coord_flip() +
  labs(title = "縣市事故排名(2013-2019)", x = "縣市", y = "事故件數") +
  theme_minimal() + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(p3)

# 圖表 4: 模型性能
cat("生成圖表 4: 模型性能...\n")
p4 <- ggplot(all_regression, aes(x = reorder(Model, -RMSE), y = RMSE, fill = 類型)) +
  geom_bar(stat = "identity", alpha = 0.8) +
  geom_text(aes(label = round(RMSE, 1)), vjust = -0.3, size = 3) +
  scale_fill_manual(values = c("時間序列" = "coral", "機器學習" = "steelblue")) +
  labs(title = "迴歸模型性能比較 (RMSE)",
       x = "模型", y = "RMSE") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p4)

# 圖表 5: 月份分布
cat("生成圖表 5: 月份分布...\n")
p5 <- ggplot(monthly_stats, aes(x = month, y = 事故件數, fill = as.factor(month))) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = 事故件數), vjust = -0.3, size = 3) +
  scale_x_continuous(breaks = 1:12, labels = month.abb) +
  scale_fill_viridis_d() +
  labs(title = "月份事故分布 (2013-2019)",
       x = "月份", y = "事故件數") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")
print(p5)

# 8.5 月份分布熱力圖
monthly_yearly <- traffic_data %>%
  group_by(ad_year, month) %>%
  summarise(incidents = n(), .groups = "drop")

p_mon <- ggplot(monthly_yearly, aes(x = factor(month), y = factor(ad_year), fill = incidents)) +
  geom_tile() +
  scale_fill_viridis_c(option = "plasma") +
  scale_x_discrete(labels = month.abb) +
  labs(title = "事故數量熱力圖 (年份 × 月份)",
       x = "月份", y = "年份", fill = "事故數") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p_mon)
cat("✓ 圖5: 熱力圖\n")

# 圖表 6: 事故類型
cat("生成圖表 6: 事故類型分布...\n")
p6 <- ggplot(type_stats, aes(x = incident_type, y = 事故件數, fill = incident_type)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = 事故件數), vjust = -0.3, size = 4) +
  scale_fill_manual(values = c("A1" = "#e74c3c", "A2" = "#3498db")) +
  labs(title = "事故類型分布 (2013-2019)",
       x = "事故類型", y = "事故件數") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")
print(p6)

# 圖表 7: 季節分布
cat("生成圖表 7: 季節分布...\n")
p7 <- ggplot(season_stats, aes(x = season, y = 事故件數, fill = season)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_text(aes(label = 事故件數), vjust = -0.3, size = 4) +
  scale_fill_manual(values = c("冬季" = "#3498db", "春季" = "#2ecc71", 
                               "夏季" = "#f39c12", "秋季" = "#e74c3c")) +
  labs(title = "季節事故分布 (2013-2021)",
       x = "季節", y = "事故件數") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        legend.position = "none")
print(p7)

cat("✓ 所有圖表已生成\n\n")

# 8.6 分類模型 ROC 曲線 
if (exists("logit_prob") && exists("rf_class_prob") && exists("xgb_class_prob")) {
  roc_logit <- pROC::roc(test_class$is_severe, logit_prob, quiet = TRUE)
  roc_rf <- pROC::roc(test_class$is_severe, rf_class_prob, quiet = TRUE)
  roc_xgb <- pROC::roc(test_class$is_severe, xgb_class_prob, quiet = TRUE)
  
  plot(roc_logit, col = "blue", main = "ROC 曲線比較 - A1/A2 分類")
  lines(roc_rf, col = "green")
  lines(roc_xgb, col = "red")
  abline(a = 0, b = 1, lty = 2, col = "gray")
  
  legend("bottomright", 
         legend = c(
           paste0("邏輯迴歸 (AUC=", round(auc(roc_logit), 3), ")"),
           paste0("隨機森林 (AUC=", round(auc(roc_rf), 3), ")"),
           paste0("XGBoost (AUC=", round(auc(roc_xgb), 3), ")")
         ),
         col = c("blue", "green", "red"), lty = 1)
  
  cat("✓ ROC 曲線已繪製\n")
}
# ============================================================================
# PHASE 9: 未來預測
# ============================================================================

cat("\nPHASE 9: 未來預測\n")

if (exists("arima_model")) {
  # 使用完整數據重新訓練
  final_arima <- auto.arima(ts_incidents, seasonal = TRUE)
  future_forecast <- forecast(final_arima, h = 12)
  
  forecast_df <- data.frame(
    月份 = 1:12,
    預測值 = round(as.numeric(future_forecast$mean)),
    下界_95 = round(future_forecast$lower[, 2]),
    上界_95 = round(future_forecast$upper[, 2])
  )
  
  cat("\n【未來12個月事故數預測】\n")
  print(forecast_df)
  
  # 預測視覺化
  plot(future_forecast, main = "未來12個月事故數預測",
       xlab = "時間", ylab = "事故件數")
}

