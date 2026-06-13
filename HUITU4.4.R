# ================== 包加载 ==================
library(readxl)    # Excel文件读取
library(ggplot2)   # 绘图核心包
library(tidyr)     # 数据长宽格式转换
library(dplyr)     # 数据操作与管道处理
library(purrr)     # 函数式编程工具
library(magrittr)  # 管道操作符支持
library(scales)    # 坐标轴标签格式化

# ================== 参数设置 ==================
# 读取参数
start_row <- 7     # 数据起始行号（对应Excel中第1行）
num_rows <- 11      # 需要读取的行数（包含标题行）

# ================== 动态读取Excel数据 ==================
# 定义文件路径（需根据实际路径修改）
file_path <- "C:/Users/lenovo/Desktop/全球坡耕地研究数据/3第三篇论文整理/手工整理后形成数据/数据处理1（第二阶段分析）/LUCC坡度分布变化指数3.xlsx"

# 读取Excel指定范围数据
lucc_data <- readxl::read_excel(
  path = file_path,
  sheet = "0",                              # 第一个工作表（索引从0开始）
  range = readxl::cell_rows(
    c(start_row, start_row + num_rows - 1)  # 动态计算读取范围
  ),
  col_names = FALSE,                      # 禁用首行作为列名
  col_types = c("text", "numeric", rep("numeric", 12)),  # 指定列数据类型
  na = c("", "NA")                        # 定义缺失值标识符
) %>% 
  # 重命名列（国家、分组、12个坡度等级）
  `colnames<-`(c("Country", "分组", paste0("Slope", 1:12))) %>%  
  # 数据清洗：删除包含NA的行
  dplyr::filter(
    !is.na(Country), 
    !is.na(`分组`),
    !dplyr::if_any(starts_with("Slope"), ~is.na(.))
  )

# ================== 数据转换 ==================
# 将宽格式数据转为长格式（适用ggplot绘图）
lucc_long <- lucc_data %>% 
  tidyr::pivot_longer(
    cols = starts_with("Slope"),  # 选择所有坡度列
    names_to = "Slope_Level",     # 新列名：坡度等级
    values_to = "Index"           # 新列名：LUCC指数
  ) %>% 
  dplyr::mutate(
    # 提取坡度等级数值（去除"Slope"前缀）
    Slope_Level = as.numeric(stringr::str_remove(Slope_Level, "Slope")),
    # 分组转为因子变量（确保绘图颜色/形状正确映射）
    `分组` = factor(`分组`),  
    # 确保指数为数值类型
    Index = as.numeric(Index)     
  )

# ================== 自动配色方案 ==================
n_groups <- n_distinct(lucc_long$`分组`)  # 计算分组数量
color_palette <- grDevices::colorRampPalette(
  c("#59A14F", "#F28E2B")                # 定义颜色渐变范围
)(n_groups)
shape_palette <- rep(15:24, length.out = n_groups)  # 形状循环扩展

# ================== 可视化输出 ==================
p <- ggplot(lucc_long, aes(x = Slope_Level, y = Index, group = `分组`)) +
  # 核心图层：折线+散点
  geom_line(aes(color = `分组`), linewidth = 0.4, alpha = 0.9) +
  geom_point(aes(color = `分组`, shape = `分组`), size = 1.0, alpha = 0.9) +
  # 参考线：零线和坡度分界线
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.4) +
  geom_vline(xintercept = 6, linetype = "dashed", color = "black", linewidth = 0.4) +
  # 坐标轴设置
  scale_x_continuous(
    breaks = 1:12,
    labels = c("0-5°", "5-10°", "10-15°", "15-25°", "25-35°", ">35°",  # 修改横坐标刻度标签内容
               "0-5°", "5-10°", "10-15°", "15-25°", "25-35°", ">35°"),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    limits = c(-0.10, 0.20),
    expand = expansion(mult = c(0, 0.01)),
    labels = scales::number_format(accuracy = 0.01)
  ) +
  # 颜色与形状映射（修改图例项内容示例）
  scale_color_manual(
    values = color_palette,
    labels = c("Bareland", "Permanent snow & ice", "Permanent water bodies", 
               "Wetland", "Moss and lichen"),
    guide = guide_legend(nrow = 2, byrow = TRUE)  # 关键修改：分两行显示
  ) +
  scale_shape_manual(
    values = shape_palette,
    labels = c("Bareland", "Permanent snow & ice", "Permanent water bodies", 
               "Wetland", "Moss and lichen"),
    guide = guide_legend(nrow = 2, byrow = TRUE)  # 需与颜色图例保持一致
  ) +
  # 主题设置
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.4),
    axis.line = element_line(colour = "black", linewidth = 0.4),
    axis.ticks = element_line(colour = "black", linewidth = 0.4),
    axis.ticks.length = unit(1.0, "mm"),
    # 修改坐标轴标签大小
    axis.title.x = element_text(size = 6),  # 横坐标标签大小
    axis.title.y = element_text(size = 6),  # 纵坐标标签大小
    # 修改刻度标签大小
    axis.text.x = element_text(size = 5.5, colour = "black"),   # 横坐标刻度标签
    axis.text.y = element_text(size = 6, colour = "black"),   # 纵坐标刻度标签
    # 修改图例字体
    legend.text = element_text(size = 5),   # 图例项字体大小
    legend.title = element_text(size = 5),  # 图例标题字体大小
    legend.spacing.y = unit(0.2, "mm"),    # 行间距
    legend.key.height = unit(3, "mm"),     # 图例项高度
    legend.key.width = unit(5, "mm"),      # 图例项宽度
    legend.position = "bottom"
  ) +
  labs(x = "Slope Level", y = "LUCC Index")
# ================== 保存图片 ==================
ggsave(
  filename = "LUCC_Index1.tiff",          # 输出文件名
  plot = p,                              # 绘图对象
  device = "tiff",                       # 文件格式
  dpi = 700,                            # 分辨率（印刷级）
  width = 9,                            # 宽度（厘米）
  height = 7.3,                            # 高度（厘米）
  units = "cm",                          # 尺寸单位
  bg = "white"                           # 背景色
)

print(p)