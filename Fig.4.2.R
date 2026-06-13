# 加载必要包 ----------------------------------------------------------------
library(tidyverse)
library(readxl)
library(ggplot2)
library(showtext)  # 用于自定义字体

# 0. 初始化配置（用户修改区域）#################################################
# 设置新罗马字体（需要系统安装对应字体）
font_add("TimesNewRoman", "C:/Windows/Fonts/times.ttf")  # 新罗马字体路径
showtext_auto()

# 文件路径配置
input_file <- "C:/Users/lenovo/Desktop/全球坡耕地研究数据/3第三篇论文整理/手工整理后形成数据/数据处理1（第二阶段分析）/LUCC坡度分布变化指数2.xlsx"
output_path <- "C:/Users/lenovo/Desktop/"

# 横坐标标签配置表 ----------------------------------------------------------
x_labels <- tibble(
  Slope_Level = 1:12,
  Label = c("0-5°", "5-10°", "10-15°", "15-25°", "25-35°", ">35°", 
            "0-5°", "5-10°", "10-15°", "15-25°", "25-35°", ">35°")
)

# 可视化样式参数 ------------------------------------------------------------
plot_settings <- list(
  # 通用参数
  base_size = 6,          # 基础字体大小
  line_color = "#2d2d2d",  # 轴线颜色
  
  # 横坐标参数（已修改字体为sans）
  x_axis = list(
    title = "地形坡度分级", 
    label_size = 35,        
    label_family = "TimesNewRoman",  # <- 修改点：原为"SimSun"
    label_color = "black",
    label_angle = 0,
    label_hjust = 0.4,
    label_vjust = 0.4,
    tick_length = 0.1
  ),
  axis = list(
    line_width = 0.4,
    tick_width = 0.4
  ),
  
  # 纵坐标参数（已修改字体为sans）
  y_axis = list(
    title = "LUCC指数 (均值±标准差)",
    label_size = 35,        
    label_family = "TimesNewRoman",  # <- 修改点：原为"SimSun"
    label_color = "black",
    label_angle = 0,
    label_hjust = 0.4,
    label_vjust = 0.4,
    limits = c(-0.3, 0.3),
    breaks = seq(-0.3, 0.3, 0.1),
    labels_format = "%.2f",
    tick_length = 0.1
  ),
  axis = list(
    line_width = 0.4,
    tick_width = 0.4
  ),
  
  # 图例参数
  legend = list(
    title = "国家分组",
    title_size = 7,
    text_size = 7,
    symbol_size = 8,
    line_size = 0.3,
    row_spacing = 0.2,
    key_width = 0.4,
    key_height = 0.4
  ),
  
  # 图形尺寸
  output = list(
    width = 7.5,
    height = 6.5,
    dpi = 600
  )
)

# 1. 数据读取与处理#############################################################
# 修正数据读取错误（补充缺失的闭合括号）
lucc_data <- read_excel(
  path = input_file,
  sheet = 1,
  skip = 1,
  col_names = FALSE,
  col_types = c("text", "numeric", rep("numeric", 12))
)  # <- 修正点：补充分号

# 设置列名和分组因子
colnames(lucc_data) <- c("Country", "分组", paste0("Slope", 1:12))
actual_groups <- sort(unique(lucc_data$分组))
lucc_data <- lucc_data %>% 
  mutate(分组 = factor(分组, levels = actual_groups))

# 数据长格式转换
lucc_long <- lucc_data %>%
  pivot_longer(
    cols = starts_with("Slope"),
    names_to = "Slope_Level",
    values_to = "Index") %>%
  mutate(
    Slope_Level = as.numeric(gsub("Slope", "", Slope_Level)),
    Index = as.numeric(Index)) %>%
  drop_na(Index)

# 检查横坐标范围（确保Slope_Level在1-12之间）
stopifnot(all(lucc_long$Slope_Level %in% 1:12))  # 若报错说明数据异常

# 计算统计量
lucc_summary <- lucc_long %>%
  group_by(分组, Slope_Level) %>%
  summarise(
    Mean_Index = mean(Index, na.rm = TRUE),
    SD_Index = sd(Index, na.rm = TRUE),
    .groups = "drop") %>%
  complete(分组, Slope_Level, fill = list(Mean_Index = 0, SD_Index = 0))

# 2. 可视化实现#################################################################
line_colors <- c("#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F",
                 "#EDC948","#B07AA1","#FF9DA7","#9C755F","#BAB0AC")[1:length(actual_groups)]
shape_values <- c(15,16,17,18,19,20,21,22,23,24)[1:length(actual_groups)]

y_min <- plot_settings$y_axis$limits[1]
y_max <- plot_settings$y_axis$limits[2]

p <- ggplot(lucc_summary, aes(x = Slope_Level, y = Mean_Index, color = 分组)) +
  geom_ribbon(
    aes(
      ymin = pmax(Mean_Index - SD_Index, y_min),
      ymax = pmin(Mean_Index + SD_Index, y_max),
      fill = 分组),
    alpha = 0.15,
    color = NA) +
  geom_line(linewidth = 0.4) +
  geom_point(aes(shape = 分组), size = plot_settings$legend$symbol_size/12) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_vline(xintercept = 6, linetype = "dashed", color = "black", linewidth = 0.4) +
  scale_x_continuous(
    name = plot_settings$x_axis$title,
    breaks = x_labels$Slope_Level,
    labels = x_labels$Label,
    expand = expansion(0.015, 0),  # 确保标签不超出边界
    limits = c(1.0, 12.0)   # 扩展边界以防止标签被截断
  ) +
  scale_y_continuous(
    name = plot_settings$y_axis$title,
    limits = plot_settings$y_axis$limits,
    breaks = plot_settings$y_axis$breaks,
    labels = sprintf(plot_settings$y_axis$labels_format, plot_settings$y_axis$breaks)) +
  scale_color_manual(values = line_colors) +
  scale_fill_manual(values = line_colors) +
  scale_shape_manual(values = shape_values) +
  theme_minimal(base_size = plot_settings$base_size) +
  theme(
    axis.line = element_line(color = plot_settings$line_color, linewidth = plot_settings$axis$line_width),
    axis.ticks = element_line(color = plot_settings$line_color, linewidth = plot_settings$axis$tick_width),
    axis.ticks.length = unit(plot_settings$x_axis$tick_length, "cm"),
    axis.text.x = element_text(
      family = plot_settings$x_axis$label_family,
      size = plot_settings$x_axis$label_size,
      color = plot_settings$x_axis$label_color,
      face = "bold"),
    axis.text.y = element_text(
      family = plot_settings$y_axis$label_family,
      size = plot_settings$y_axis$label_size,
      color = plot_settings$y_axis$label_color,
      face = "bold"),
    legend.position = "bottom",
    legend.spacing.y = unit(plot_settings$legend$row_spacing, "cm"),
    legend.key.height = unit(plot_settings$legend$key_height, "cm"),
    legend.key.width = unit(plot_settings$legend$key_width, "cm"),
    legend.title = element_text(size = plot_settings$legend$title_size),
    legend.text = element_text(size = plot_settings$legend$text_size),
    panel.border = element_rect(color = "black", fill = NA, size = 0.4),
    panel.grid = element_blank(),
    text = element_text(family = "TimesNewRoman")) +
  guides(
    color = guide_legend(
      title = plot_settings$legend$title,
      nrow = 2,
      override.aes = list(
        shape = shape_values,
        size = plot_settings$legend$symbol_size/12,
        linetype = 1,
        linewidth = plot_settings$legend$line_size,
        fill = NA)),
    fill = "none",
    shape = "none")
# 3. 输出图形###################################################################
ggsave(
  filename = "LUCC_plot_final2.tiff",
  plot = p,
  path = output_path,
  width = plot_settings$output$width,
  height = plot_settings$output$height,
  units = "cm",
  dpi = plot_settings$output$dpi,
  compression = "lzw"
)
# 显示图形
print(p)