# 加载必要的包
library(ggplot2)
library(RColorBrewer)
library(tidyr)
library(readxl)
library(scales)
library(dplyr)

#---------------------------- 用户可调整参数区域 ----------------------------#
# 数据与文件参数
FILE_PATH <- "C:/Users/lenovo/Desktop/全球坡耕地研究数据/3第三篇论文整理/手工整理后形成数据/数据处理3/6.xlsx"
SHEET_NAME <- "Sheet1"
TIME_COL <- "time"

# 图形布局参数
STEP_FACTOR <- 1.0         # 间距因子（基于数据最大值）
PLOT_TYPE <- "ribbon"
ALPHA <- 0.8
LINE_SIZE <- 0.1
COLOR_PALETTE <- "Spectral"
REV_COLOR <- TRUE

# ==================== 字体总控参数（新增） ====================
FONT_FAMILY <- "Times New Roman"      # 全局字体族：sans/serif/mono 或 "Times New Roman" 等
FONT_FACE  <- "plain"      # 字形：plain/italic/bold/bold.italic
FONT_SIZE  <- 6            # 全局基础字号（除单独指定外均继承此值）

# 各元素字号（若留空或设为 NULL 则自动继承 FONT_SIZE）
TITLE_SIZE      <- 6       # 标题
SUBTITLE_SIZE   <- NULL    # 副标题（NULL = 继承 FONT_SIZE）
CAPTION_SIZE    <- NULL    # 来源注（NULL = 继承 FONT_SIZE）
LEGEND_TITLE_SIZE <- 6     # 图例标题
LEGEND_TEXT_SIZE  <- 5     # 图例正文
X_TICK_SIZE     <- 4.5     # X 轴刻度
Y_TICK_SIZE     <- 6       # Y 轴刻度
# ===========================================================

GRID_COLOR <- "grey60"     # 普通网格线颜色
AXIS_COLOR <- "black"      # 坐标轴线颜色
LEGEND_POS <- "right"
BACKGROUND_COLOR <- "white"
SHOW_GRID <- TRUE

# 图片保存参数
SAVE_PLOT <- TRUE          # 是否保存图片
SAVE_PATH <- "C:/Users/lenovo/Desktop/plot.tiff"  # 保存路径
IMG_WIDTH <- 13           # 图片宽度(cm)
IMG_HEIGHT <- 6          # 图片高度(cm)
DPI <- 600                 # 图片分辨率

# 特殊网格线参数
GRID_SPECIAL_POSITIONS <- c(7, 13)  # 需要黑色网格线的时间值
GRID_SPECIAL_COLOR <- "black"       # 特殊网格线颜色

# 坐标标签参数
X_LABEL <- "时间轴"         # 横坐标标签
X_LABEL_COLOR <- "black"   # 横坐标标签颜色
Y_LABEL <- "分类轴"         # 纵坐标标签
Y_LABEL_COLOR <- "black"   # 纵坐标标签颜色
PLOT_TITLE <- "时空分布峰峦图"

# 断开位置参数
BREAK_POINTS <- c(6, 12)    # 在时间值为6和12后断开

# 自定义坐标刻度标签（纯文本）
X_TICK_LABELS <- c(
  "1" = "0-5°", 
  "2" = "5-10°", 
  "3" = "10-15°", 
  "4" = "15-25°", 
  "5" = "25-35°", 
  "6" = ">35°", 
  "7" = "0-5°", 
  "8" = "5-10°", 
  "9" = "10-15°", 
  "10" = "15-25°", 
  "11" = "25-35°", 
  "12" = ">35°", 
  "13" = "0-5°", 
  "14" = "5-10°", 
  "15" = "10-15°", 
  "16" = "15-25°", 
  "17" = "25-35°", 
  "18" = ">35°"
)

Y_TICK_LABELS <- c(
  "TOTAL0" = "Forest", 
  "TOTAL1" = "Shrubland", 
  "TOTAL2" = "Grassland", 
  "TOTAL3" = "Cropland", 
  "TOTAL4" = "Artificial 
              surface", 
  "TOTAL5" = "Bareland",
  "TOTAL6" = "Permanent 
              snow&ice",
  "TOTAL7" = "Permanent 
            water bodies", 
  "TOTAL8" = "Wetlands", 
  "TOTAL9" = "Moss/lichen"
)

#---------------------------- 数据预处理 ----------------------------#
# 读取数据
df <- read_excel(FILE_PATH, sheet = SHEET_NAME)

# 数据清洗：移除时间列非数字字符
df[[TIME_COL]] <- gsub("[^0-9.]", "", df[[TIME_COL]]) %>% 
  as.numeric()

# 转换长数据格式
df_long <- df %>%
  pivot_longer(
    cols = -all_of(TIME_COL),
    names_to = "Class",
    values_to = "Value"
  )

# 动态计算间距
max_value <- max(df_long$Value)
STEP <- max_value * STEP_FACTOR

# 处理时间轴（生成含断开标签）
time_values <- sort(unique(df[[TIME_COL]]))  # 确保数值型数据
time_labels <- character(0)
for(value in time_values) {
  time_labels <- c(time_labels, as.character(value))
  if(value %in% BREAK_POINTS) {
    time_labels <- c(time_labels, "")  # 插入空标签
  }
}

# 构建因子水平映射
valid_labels <- time_labels[time_labels != ""]
all_levels <- unique(c(valid_labels, ""))
df_long[[TIME_COL]] <- factor(
  as.character(df_long[[TIME_COL]]),
  levels = all_levels,
  ordered = TRUE
)

# 生成网格线定位数据
grid_positions <- sapply(GRID_SPECIAL_POSITIONS, function(x) {
  which(levels(df_long[[TIME_COL]]) == as.character(x))
})
grid_data <- data.frame(
  x = grid_positions,
  color = GRID_SPECIAL_COLOR
)

# 保持原始类别顺序
original_classes <- setdiff(names(df), TIME_COL)
df_long$Class <- factor(df_long$Class, levels = original_classes)

# 计算偏移量
df_long <- df_long %>%
  group_by(Class) %>%
  mutate(
    offset = -as.numeric(Class) * STEP,
    y_position = Value + offset
  ) %>%
  ungroup()

#---------------------------- 关键修改：在断开区间移除数据 ----------------------------#
# 筛选数据：排除6-7和12-13之间的时间点
df_long <- df_long %>%
  mutate(
    time_num = as.numeric(as.character(.data[[TIME_COL]]))
  ) %>%
  filter(
    !(time_num > 6 & time_num < 7),   # 排除6-7之间
    !(time_num > 12 & time_num < 13)  # 排除12-13之间
  ) %>%
  select(-time_num)

#---------------------------- 绘图核心 ----------------------------#
base_plot <- ggplot(df_long, aes(x = .data[[TIME_COL]], y = y_position, group = Class)) +
  # 绘制峰峦图
  geom_ribbon(
    aes(ymin = offset, ymax = y_position, fill = Class),
    alpha = ALPHA,
    color = NA
  ) +
  geom_line(color = "black", linewidth = LINE_SIZE) +
  
  # 绘制特殊网格线（虚线）
  geom_vline(
    data = grid_data,
    aes(xintercept = x),
    color = GRID_SPECIAL_COLOR,
    linetype = "dashed",
    linewidth = 0.2
  ) +
  
  # 颜色与坐标轴设置
  scale_fill_manual(values = colorRampPalette(brewer.pal(11, COLOR_PALETTE))(nlevels(df_long$Class)) %>% 
                      {if(REV_COLOR) rev(.) else .}) +
  scale_y_continuous(
    breaks = seq(-STEP, -(STEP * nlevels(df_long$Class)), by = -STEP),
    labels = function(breaks) {
      Y_TICK_LABELS[original_classes]  # 应用自定义纵坐标标签
    },
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  scale_x_discrete(
    breaks = valid_labels,
    labels = function(x) {
      ifelse(x %in% names(X_TICK_LABELS), 
             X_TICK_LABELS[x], 
             ifelse(x == "", "", x))
    },
    expand = expansion(mult = c(0.01, 0.01))
  ) +
  
  # 标签与主题设置
  labs(
    x = X_LABEL,
    y = Y_LABEL,
    title = PLOT_TITLE
  ) +
  theme_classic() +
  theme(
    # ---------- 全局字体继承（所有文本元素统一绑定 FONT_FAMILY） ----------
    text = element_text(
      family = FONT_FAMILY, 
      face   = FONT_FACE,
      size   = FONT_SIZE,
      color  = "black"
    ),
    
    # 隐藏默认坐标轴线，添加闭合边框
    axis.line = element_blank(),
    panel.border = element_rect(color = AXIS_COLOR, fill = NA, linewidth = 0.5),
    
    # 网格线设置
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(
      color = if(SHOW_GRID) GRID_COLOR else NA,
      linewidth = 0.2
    ),
    
    # 坐标标签（显式继承字体族，避免被系统默认覆盖）
    axis.title.x = element_text(
      color = X_LABEL_COLOR, 
      size  = FONT_SIZE, 
      family = FONT_FAMILY, 
      face  = FONT_FACE
    ),
    axis.title.y = element_text(
      color = Y_LABEL_COLOR, 
      size  = FONT_SIZE, 
      family = FONT_FAMILY, 
      face  = FONT_FACE
    ),
    axis.text.x = element_text(
      color  = "black", 
      size   = X_TICK_SIZE, 
      angle  = 0, 
      hjust  = 0.5,
      family = FONT_FAMILY, 
      face   = FONT_FACE
    ),
    axis.text.y = element_text(
      color  = "black", 
      size   = Y_TICK_SIZE,
      family = FONT_FAMILY, 
      face   = FONT_FACE
    ),
    
    # 刻度线
    axis.ticks.x = element_line(linewidth = 0.3), 
    axis.ticks.y = element_line(linewidth = 0.3),
    axis.ticks.length.x = unit(0.08, "cm"),
    axis.ticks.length.y = unit(0.08, "cm"),
    
    # 标题 / 副标题 / 来源注
    plot.title = element_text(
      size   = TITLE_SIZE, 
      hjust  = 0.5,
      family = FONT_FAMILY, 
      face   = FONT_FACE
    ),
    plot.subtitle = element_text(
      size   = if(is.null(SUBTITLE_SIZE)) FONT_SIZE else SUBTITLE_SIZE,
      family = FONT_FAMILY, 
      face   = FONT_FACE
    ),
    plot.caption = element_text(
      size   = if(is.null(CAPTION_SIZE)) FONT_SIZE else CAPTION_SIZE,
      family = FONT_FAMILY, 
      face   = FONT_FACE
    ),
    
    # 图例字体（新增，防止图例使用系统默认字体）
    legend.text = element_text(
      size   = LEGEND_TEXT_SIZE, 
      family = FONT_FAMILY, 
      face   = FONT_FACE
    ),
    legend.title = element_text(
      size   = LEGEND_TITLE_SIZE, 
      family = FONT_FAMILY, 
      face   = FONT_FACE
    ),
    
    # 背景与其他
    panel.background = element_rect(fill = BACKGROUND_COLOR),
    legend.position = LEGEND_POS
  )

# 显示图形
print(base_plot)

# 保存高清图片
if(SAVE_PLOT){
  ggsave(
    filename = SAVE_PATH,
    plot = base_plot,
    device = "tiff",
    width = IMG_WIDTH,
    height = IMG_HEIGHT,
    units = "cm",
    dpi = DPI,
    compression = "lzw"
  )
  message("图片已保存至: ", normalizePath(SAVE_PATH))
}