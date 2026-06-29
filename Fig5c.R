# 加载必要的包 ------------------------------------------------------------------
library(ggplot2)      # 核心可视化引擎
library(data.table)   # 高效数据处理
library(RColorBrewer) # 科学配色方案
library(readxl)       # 读取Excel文件

# ==================================================================
# 1. 全局字体与文字参数配置（所有文字大小和字体均可在此统一调整）
# ==================================================================

BASE_FONT_FAMILY <- "Times New Roman"

AXIS_TEXT_X_SIZE   <- 9
AXIS_TEXT_Y_SIZE   <- 7
AXIS_TITLE_X_SIZE  <- 10
AXIS_TITLE_Y_SIZE  <- 7
LEGEND_TITLE_SIZE  <- 6
LEGEND_TEXT_SIZE   <- 8
PLOT_TITLE_SIZE    <- 8

PT_TO_MM <- 0.352777

# =============================================================================
# 2. 文件路径配置（输入路径 + 输出路径）
# =============================================================================

file_path <- "C:/Users/lenovo/Desktop/全球坡耕地研究数据/3第三篇论文整理/手工整理后形成数据/数据处理/国家尺度的瀑布图/Total2.xlsx"

output_dir  <- "C:/Users/lenovo/Desktop/全球坡耕地研究数据/3第三篇论文整理/手工整理后形成数据/初稿、图件和表格/图片重绘/图20260511"
output_file <- "c.tiff"
output_path <- file.path(output_dir, output_file)

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

if (!file.exists(file_path)) {
  stop(paste("文件路径不存在 -", file_path))
}

dtExp <- data.table(read_excel(file_path))
dtExp[, original_order := .I]

# =============================================================================
# 核心参数配置
# =============================================================================

global_min <- min(dtExp$value)
global_max <- max(dtExp$value)
Height <- 6

time_range_mapping <- list(
  "0-5"   = list(angle_start = 0,   angle_end = 60),
  "5-10"  = list(angle_start = 60,  angle_end = 120),
  "10-15" = list(angle_start = 120, angle_end = 180),
  "15-25" = list(angle_start = 180, angle_end = 240),
  "25-35" = list(angle_start = 240, angle_end = 300),
  "35"    = list(angle_start = 300, angle_end = 360)
)

# =============================================================================
# 数据处理流程
# =============================================================================

groups <- unique(dtExp$group)
Step <- 6
current_base <- Step
dtCombined <- data.table()

for (grp in groups) {
  dt_group <- dtExp[group == grp][order(original_order)]
  dt_group[, time := gsub("--", "-", time)]
  
  dt_group[, DateNum := {
    range <- time_range_mapping[[time]]
    if (is.null(range)) stop(paste("未定义的时间区间 -", time))
    range$angle_start + runif(.N, 0, range$angle_end - range$angle_start)
  }, by = time]
  
  dt_group[, time_order := as.integer(factor(time, levels = names(time_range_mapping)))]
  dt_group[, Asst := current_base + Step * (time_order / max(time_order))]
  current_base <- max(dt_group$Asst)
  
  dt_group[, Valueht := (value - global_min) / (global_max - global_min) * Height * 1.0]
  
  dtCombined <- rbind(dtCombined, dt_group)
}

dtCombined <- dtCombined[order(original_order)]

# =============================================================================
# 可视化引擎配置
# =============================================================================

max_radius <- max(dtCombined$Asst + dtCombined$Valueht) - 5 + 0

p <- ggplot(dtCombined) +
  # 核心条带层
  geom_linerange(
    aes(
      x = DateNum,
      ymin = Asst - 5,
      ymax = Asst + Valueht - 5,
      color = value
    ),
    linewidth = 0.2,
    alpha = 1.0
  ) +
  
  # 主参考线：每60度一条实线（手动控制，线宽可调）
  geom_segment(
    data = data.frame(x = seq(0, 300, by = 60)),
    aes(x = x, xend = x, y = 0, yend = max_radius),
    color = "black",
    linewidth = 0.3          # ← 修改此处即可看到变化
  ) +
  
  # 次参考线：每20度一条虚线（手动控制，线宽可调）
  geom_segment(
    data = data.frame(x = c(seq(20, 340, by = 60), seq(40, 340, by = 60))),
    aes(x = x, xend = x, y = 0, yend = 1.05 * max_radius),
    color = "black",
    linewidth = 0.3,         # ← 修改此处即可看到变化
    linetype = "dashed"
  ) +
  
  coord_polar(theta = "x", start = 0, clip = "off") +
  
  scale_y_continuous(
    name = "Radial Distance",
    breaks = seq(0, floor(max_radius), by = 5),
    limits = c(0, max_radius * 1.05),
    expand = c(0, 0)
  ) +
  
  scale_x_continuous(
    breaks = seq(0, 330, by = 30),
    labels = NULL
  ) +
  
  scale_color_gradientn(
    colours = rev(brewer.pal(11, "Spectral")),
    name = "Value Intensity"
  ) +
  
  theme_minimal(base_family = BASE_FONT_FAMILY) +
  theme(
    text = element_text(family = BASE_FONT_FAMILY),
    
    legend.text = element_text(
      family = BASE_FONT_FAMILY,
      size = LEGEND_TEXT_SIZE
    ),
    legend.title = element_text(
      family = BASE_FONT_FAMILY,
      size = LEGEND_TITLE_SIZE
    ),
    legend.key.height = unit(1.4, "cm"),
    legend.key.width  = unit(0.8, "cm"),
    legend.spacing.x  = unit(0.3, "cm"),
    
    axis.title.y = element_text(
      family = BASE_FONT_FAMILY,
      size = AXIS_TITLE_Y_SIZE
    ),
    axis.text.y = element_text(
      family = BASE_FONT_FAMILY,
      size = AXIS_TEXT_Y_SIZE
    ),
    
    axis.text.x = element_blank(),
    axis.title.x = element_text(
      family = BASE_FONT_FAMILY,
      size = AXIS_TITLE_X_SIZE
    ),
    
    plot.margin = margin(30, 30, 30, 30, "pt"),
    
    # ========================================================================
    # 【关键修改】关闭默认极坐标角度网格线，避免覆盖手动 geom_segment 参考线
    # ========================================================================
    panel.grid.major.x = element_blank(),   # 关闭角度方向主网格线
    panel.grid.minor.x = element_blank(),   # 关闭角度方向次网格线
    
    # 保留径向同心圆（Y方向主网格线），颜色/线宽也可在此自由调整
    panel.grid.major.y = element_line(color = "black", linewidth = 0.3),
    panel.grid.minor.y = element_blank()
  )

options(repr.plot.width = 5.3, repr.plot.height = 5.3)
print(p)

ggsave(
  filename = output_path,
  plot = p,
  width = 5.3,
  height = 5.3,
  units = "in",
  dpi = 500,
  device = "tiff"
)

message(paste("图片已成功保存至:", output_path))