# ================== 包加载 ==================
library(readxl)
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# ================== 参数设置 ==================
start_row <- 7
num_rows <- 11
file_path <- "C:/Users/lenovo/Desktop/全球坡耕地研究数据/3第三篇论文整理/手工整理后形成数据/数据处理1（第二阶段分析）/LUCC坡度分布变化指数3.xlsx"

# ================== 地类标签与固定配色（与图 b 图例完全一致）==================
group_labels <- c(
  "5" = "Bareland",
  "6" = "Permanent snow & ice",
  "7" = "Permanent water bodies",
  "8" = "Wetland",
  "9" = "Moss and lichen"
)

# 颜色严格按图例对应：蓝 → 橙 → 青 → 红 → 绿
color_palette <- c(
  "5" = "#4E79A7",   # Bareland: 蓝
  "6" = "#F28E2B",   # Permanent snow & ice: 橙
  "7" = "#76B7B2",   # Permanent water bodies: 青
  "8" = "#E15759",   # Wetland: 红
  "9" = "#59A14F"    # Moss and lichen: 绿
)

# 形状严格按图例对应：方块 → 圆 → 三角 → 菱形 → 圆点
shape_palette <- c(
  "5" = 15,   # Bareland: 实心方块
  "6" = 16,   # Permanent snow & ice: 实心圆
  "7" = 18,   # Permanent water bodies: 实心三角
  "8" = 17,   # Wetland: 实心菱形
  "9" = 19    # Moss and lichen: 实心圆点
)

# ================== 读取 Excel 数据 ==================
lucc_data <- readxl::read_excel(
  path = file_path,
  sheet = "0",
  range = readxl::cell_rows(c(start_row, start_row + num_rows - 1)),
  col_names = FALSE,
  col_types = c("text", "numeric", rep("numeric", 12)),
  na = c("", "NA")
) %>% 
  `colnames<-`(c("Country", "分组", paste0("Slope", 1:12))) %>%  
  dplyr::filter(
    !is.na(Country), 
    !is.na(`分组`),
    !dplyr::if_any(starts_with("Slope"), ~is.na(.))
  )

# ================== 数据转换 ==================
lucc_long <- lucc_data %>% 
  tidyr::pivot_longer(
    cols = starts_with("Slope"),
    names_to = "Slope_Level",
    values_to = "Index"
  ) %>% 
  dplyr::mutate(
    Slope_Level = as.numeric(stringr::str_remove(Slope_Level, "Slope")),
    `分组` = as.character(`分组`),
    Index = as.numeric(Index)
  )

# ================== 可视化输出 ==================
p <- ggplot(lucc_long, aes(x = Slope_Level, y = Index, group = `分组`)) +
  geom_line(aes(color = `分组`), linewidth = 0.3, alpha = 0.9) +
  geom_point(aes(color = `分组`, shape = `分组`), size = 1.0, alpha = 0.9) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.2) +
  geom_vline(xintercept = 6, linetype = "dashed", color = "black", linewidth = 0.2) +
  scale_x_continuous(
    breaks = 1:12,
    labels = c("0-5°", "5-10°", "10-15°", "15-25°", "25-35°", ">35°",
               "0-5°", "5-10°", "10-15°", "15-25°", "25-35°", ">35°"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(-0.30, 0.30),
    expand = expansion(mult = c(0, 0.01)),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  scale_color_manual(
    values = color_palette,
    labels = group_labels,
    guide = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  scale_shape_manual(
    values = shape_palette,
    labels = group_labels,
    guide = guide_legend(nrow = 2, byrow = TRUE)
  ) +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.2),
    axis.line = element_line(colour = "black", linewidth = 0.2),
    axis.ticks = element_line(colour = "black", linewidth = 0.2),
    axis.ticks.length = unit(1.0, "mm"),
    axis.title.x = element_text(size = 6),
    axis.title.y = element_text(size = 6),
    axis.text.x = element_text(size = 5.5, colour = "black"),
    axis.text.y = element_text(size = 6, colour = "black"),
    legend.text = element_text(size = 5),
    legend.title = element_text(size = 5),
    legend.spacing.y = unit(0.2, "mm"),
    legend.key.height = unit(3, "mm"),
    legend.key.width = unit(5, "mm"),
    legend.position = "bottom"
  ) +
  labs(x = "Slope Level", y = "LUCC Index")

# ================== 保存图片 ==================
ggsave(
  filename = "LUCC_Index1.tiff",
  plot = p,
  device = "tiff",
  dpi = 700,
  width = 9,
  height = 7.3,
  units = "cm",
  bg = "white"
)

print(p)