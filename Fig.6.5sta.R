# =============================================================================
# 加载必要包
# =============================================================================
library(tidyverse)   # 数据处理与整理
library(readxl)      # 读取 Excel 输入文件
library(writexl)     # 导出结果为 Excel 文件

# =============================================================================
# 0. 路径配置
# =============================================================================
input_file <- "C:/Users/lenovo/Desktop/全球坡耕地研究数据/3第三篇论文整理/手工整理后形成数据/数据处理1（第二阶段分析）/LUCC坡度分布变化指数1.xlsx"
output_file <- "C:/Users/lenovo/Desktop/LUCC_坡度分布_统计结果.xlsx"

# =============================================================================
# 1. 数据读取与处理
# =============================================================================

# 读取 Excel 数据
lucc_data <- read_excel(
  path = input_file,
  sheet = 1,
  skip = 1,
  col_names = FALSE,
  col_types = c("text", "numeric", rep("numeric", 12))
)

# 设置列名
colnames(lucc_data) <- c("Country", "分组", paste0("Slope", 1:12))

# --- 分组编号 → 地类名称映射 ---------------------------------------------------
group_labels <- c(
  "0" = "Forest",
  "1" = "Shrubland",
  "2" = "Grassland",
  "3" = "Cropland",
  "4" = "Artificial surface",
  "5" = "Bareland",
  "6" = "Permanent snow & ice",
  "7" = "Wetlands",
  "8" = "Permanent water bodies",
  "9" = "Moss and lichen"
)

# 将分组编号替换为地类名称
lucc_data <- lucc_data %>%
  mutate(
    分组 = factor(
      as.character(分组),
      levels = names(group_labels),
      labels = group_labels
    )
  )

# 宽格式 → 长格式转换
lucc_long <- lucc_data %>%
  pivot_longer(
    cols = starts_with("Slope"),
    names_to = "Slope_Level",
    values_to = "Index") %>%
  mutate(
    Slope_Level = as.numeric(gsub("Slope", "", Slope_Level)),
    Index = as.numeric(Index)) %>%
  drop_na(Index)

# 数据完整性检查
stopifnot(all(lucc_long$Slope_Level %in% 1:12))

# =============================================================================
# 2. 计算统计量并添加时期标签
# =============================================================================

# 按地类和坡度等级分组，计算均值、标准差、样本量
lucc_summary <- lucc_long %>%
  group_by(分组, Slope_Level) %>%
  summarise(
    Mean_Index = mean(Index, na.rm = TRUE),
    SD_Index = sd(Index, na.rm = TRUE),
    n = n(),
    .groups = "drop") %>%
  complete(分组, Slope_Level, fill = list(Mean_Index = 0, SD_Index = 0, n = 0))

# --- 添加坡度标签与时期标签 ----------------------------------------------------
# Slope_Level 1-6 对应第一时期，7-12 对应第二时期
x_labels <- tibble(
  Slope_Level = 1:12,
  Slope_Label = c("0-5°", "5-10°", "10-15°", "15-25°", "25-35°", ">35°",
                  "0-5°", "5-10°", "10-15°", "15-25°", "25-35°", ">35°"),
  Period = c(rep("2000-2010", 6), rep("2010-2020", 6))
)

lucc_summary <- lucc_summary %>%
  left_join(x_labels, by = "Slope_Level") %>%
  select(分组, Period, Slope_Level, Slope_Label, n, Mean_Index, SD_Index) %>%
  arrange(分组, Slope_Level)

# =============================================================================
# 3. 输出结果
# =============================================================================

# 导出为 Excel
write_xlsx(lucc_summary, path = output_file)

# 控制台预览
print("===== 统计结果预览（前30行）=====")
print(head(lucc_summary, 30))

# RStudio 中查看完整数据
View(lucc_summary)