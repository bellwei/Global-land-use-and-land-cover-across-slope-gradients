# =============================================================================
# 加载必要包
# =============================================================================
# tidyverse: 包含 dplyr（数据处理）、tidyr（数据整形）、readr（读取）、
#            ggplot2（可视化）等核心包，是数据科学工作流的基础
library(tidyverse)

# readxl: 专门用于读取 Excel 文件（.xlsx/.xls），不依赖 Java，比 xlsx 包更稳定
library(readxl)

# ggplot2: 声明式图形语法，用于构建高质量科研图表
# （tidyverse 已包含，但显式加载可提高代码可读性）
library(ggplot2)

# showtext: 让 R 的图形设备支持自定义中文字体/特殊字体。
# 原理：将字体渲染为位图后嵌入图形，解决 Windows 默认设备对非系统字体支持差的问题。
# 注意：showtext 在某些输出设备（如 TIFF）中可能出现字体度量计算偏差，
#       导致预览与导出效果不一致。如遇到此问题，可改用 windowsFonts() 方案。
library(showtext)

# =============================================================================
# 0. 初始化配置（用户修改区域）
# =============================================================================

# --- 字体配置 ----------------------------------------------------------------
# font_add(): 将系统字体文件注册到 R 中，供 showtext 使用。
# 参数1: 字体在 R 中的别名（后续 theme() 中通过此名称引用）
# 参数2: 字体文件的绝对路径（Windows 系统字体通常位于 C:/Windows/Fonts/）
# "times.ttf" 对应 Times New Roman（新罗马），是英文学术论文的标准正文字体
font_add("TimesNewRoman", "C:/Windows/Fonts/times.ttf")

# showtext_auto(): 自动让当前及后续所有图形设备启用 showtext 字体渲染。
# 调用后，theme() 中 family = "TimesNewRoman" 即可生效。
showtext_auto()

# --- 文件路径配置 --------------------------------------------------------------
# input_file: 输入数据文件路径（Excel 格式）
# 数据应为手工整理后的 LUCC 坡度分布变化指数，包含国家、分组及 12 个坡度等级列
input_file <- "C:/Users/lenovo/Desktop/全球坡耕地研究数据/3第三篇论文整理/手工整理后形成数据/数据处理1（第二阶段分析）/LUCC坡度分布变化指数1.xlsx"

# output_path: 图形输出目录（末尾需加斜杠或确保为文件夹路径）
output_path <- "C:/Users/lenovo/Desktop/"

# --- 横坐标标签配置表 ----------------------------------------------------------
# 建立坡度等级（1-12）与显示标签的映射关系。
# 前 6 个等级对应第一时期（如 2000-2010），后 6 个对应第二时期（如 2010-2020）。
# 每个等级代表一个坡度区间，单位为度（°）。
x_labels <- tibble(
  Slope_Level = 1:12,
  Label = c("0-5°", "5-10°", "10-15°", "15-25°", "25-35°", ">35°",   # 第一时期
            "0-5°", "5-10°", "10-15°", "15-25°", "25-35°", ">35°")   # 第二时期
)

# --- 可视化样式参数（集中管理，方便统一修改）------------------------------------
plot_settings <- list(
  
  # 通用参数
  base_size = 6,            # ggplot2 theme_minimal() 的基础字号，影响未显式设置的文字元素
  line_color = "#2d2d2d", # 坐标轴线、刻度线的颜色（深灰色，比纯黑更柔和）
  
  # 横坐标（X轴）参数
  x_axis = list(
    title = "地形坡度分级",           # X轴标题文字
    label_size = 35,                  # X轴刻度标签字号（单位：pt）
    label_family = "TimesNewRoman",   # X轴刻度标签字体（新罗马）
    label_color = "black",            # X轴刻度标签颜色
    label_angle = 0,                  # X轴刻度标签旋转角度（0 = 水平）
    label_hjust = 0.4,                # X轴刻度标签水平对齐（0.4 略偏左，用于微调间距）
    label_vjust = 0.4,                # X轴刻度标签垂直对齐（0.4 略偏上）
    tick_length = 0.1                 # 刻度线长度（单位：cm）
  ),
  
  # 纵坐标（Y轴）参数
  y_axis = list(
    title = "LUCC指数 (均值±标准差)", # Y轴标题文字
    label_size = 35,                  # Y轴刻度标签字号
    label_family = "TimesNewRoman",   # Y轴刻度标签字体
    label_color = "black",            # Y轴刻度标签颜色
    label_angle = 0,                  # Y轴刻度标签旋转角度
    label_hjust = 0.4,                # Y轴刻度标签水平对齐
    label_vjust = 0.4,                # Y轴刻度标签垂直对齐
    limits = c(-0.3, 0.3),            # Y轴显示范围（最小值, 最大值）
    breaks = seq(-0.3, 0.3, 0.1),     # Y轴刻度位置：从 -0.3 到 0.3，间隔 0.1
    labels_format = "%.2f",           # Y轴刻度标签格式：保留两位小数
    tick_length = 0.1                 # Y轴刻度线长度（单位：cm）
  ),
  
  # 坐标轴线通用参数（X、Y轴共享）
  axis = list(
    line_width = 0.1,   # 轴线粗细（单位：mm）
    tick_width = 0.2    # 刻度线粗细
  ),
  
  # 图例参数
  legend = list(
    title = "国家分组",       # 图例标题文字
    title_size = 7,           # 图例标题字号
    text_size = 7,            # 图例项文字字号
    symbol_size = 8,          # 图例中符号（点/线）的大小（后续需除以 12 转换为 ggplot2 的 size 单位）
    line_size = 0.3,          # 图例中线条粗细
    row_spacing = 0.2,        # 图例行间距（单位：cm）
    key_width = 0.4,          # 图例键（色块/符号区域）宽度
    key_height = 0.4          # 图例键高度
  ),
  
  # 输出图形尺寸参数
  output = list(
    width = 7.5,    # 输出图片宽度（单位：cm）
    height = 6.5,   # 输出图片高度（单位：cm）
    dpi = 600       # 输出分辨率（每英寸点数），600 dpi 满足期刊印刷要求
  )
)

# =============================================================================
# 1. 数据读取与处理
# =============================================================================

# --- 读取 Excel 数据 ---------------------------------------------------------
# read_excel(): 读取 Excel 文件。
# 参数说明：
#   path: 文件路径
#   sheet = 1: 读取第 1 个工作表
#   skip = 1: 跳过前 1 行（通常是表头说明或空行）
#   col_names = FALSE: 第一行不是列名，由后续代码手动指定
#   col_types: 指定每列的数据类型，避免自动推断错误。
#              c("text", "numeric", rep("numeric", 12)) 表示：
#              第1列 = 文本（国家名），第2列 = 数值（分组编号），
#              第3-14列 = 数值（Slope1 到 Slope12 的指数值）
lucc_data <- read_excel(
  path = input_file,
  sheet = 1,
  skip = 1,
  col_names = FALSE,
  col_types = c("text", "numeric", rep("numeric", 12))
)

# --- 设置列名并转换分组为因子 --------------------------------------------------
# colnames(): 为数据框指定列名，方便后续按名称引用
# "Country": 国家名称
# "分组": 国家所属的分组编号（如 0, 1, 2, 3, 4...）
# paste0("Slope", 1:12): 生成 Slope1, Slope2, ..., Slope12 共 12 个坡度等级列
colnames(lucc_data) <- c("Country", "分组", paste0("Slope", 1:12))

# 提取实际存在的分组编号，并排序
actual_groups <- sort(unique(lucc_data$分组))

# mutate() + factor(): 将"分组"列转换为因子（分类变量）。
# levels = actual_groups: 固定因子水平顺序，确保图例按编号从小到大排列，
# 避免 R 按字母顺序重新排列导致图例顺序混乱。
lucc_data <- lucc_data %>% 
  mutate(分组 = factor(分组, levels = actual_groups))

# --- 数据格式转换：宽格式 → 长格式 --------------------------------------------
# pivot_longer(): 将"宽格式"（每列一个坡度等级）转换为"长格式"（每行一个观测），
# 这是 ggplot2 绘图所需的"tidy data"格式。
# 参数说明：
#   cols = starts_with("Slope"): 选择所有以 "Slope" 开头的列进行转换
#   names_to = "Slope_Level": 原列名（Slope1, Slope2...）存入新列 "Slope_Level"
#   values_to = "Index": 原列的值存入新列 "Index"
# mutate():
#   gsub("Slope", "", Slope_Level): 去掉 "Slope" 前缀，保留数字（1-12）
#   as.numeric(): 转换为数值型，用于作为连续型 X 轴变量
# drop_na(Index): 删除 Index 为 NA 的行，避免绘图报错
lucc_long <- lucc_data %>%
  pivot_longer(
    cols = starts_with("Slope"),
    names_to = "Slope_Level",
    values_to = "Index") %>%
  mutate(
    Slope_Level = as.numeric(gsub("Slope", "", Slope_Level)),
    Index = as.numeric(Index)) %>%
  drop_na(Index)

# --- 数据完整性检查 ------------------------------------------------------------
# stopifnot(): 断言检查。如果条件不满足，立即停止执行并报错。
# 检查所有 Slope_Level 是否都在 1-12 范围内。
# 若报错，说明数据中存在异常的坡度等级编号，需检查原始 Excel。
stopifnot(all(lucc_long$Slope_Level %in% 1:12))

# --- 按分组和坡度等级计算统计量（均值 ± 标准差）--------------------------------
# group_by(分组, Slope_Level): 按"分组"和"坡度等级"分组，后续计算在每个组内独立进行
# summarise():
#   Mean_Index = mean(Index, na.rm = TRUE): 计算每组均值，na.rm = TRUE 忽略缺失值
#   SD_Index = sd(Index, na.rm = TRUE): 计算每组标准差
#   .groups = "drop": 计算完成后删除分组结构，返回普通数据框
# complete(分组, Slope_Level, fill = ...): 确保所有"分组×坡度等级"组合都存在。
#   若某些组合在原始数据中缺失，用 fill 参数填充默认值（均值=0，标准差=0），
#   保证绘图时每个分组都有完整的 12 个数据点，折线不会中断。
lucc_summary <- lucc_long %>%
  group_by(分组, Slope_Level) %>%
  summarise(
    Mean_Index = mean(Index, na.rm = TRUE),
    SD_Index = sd(Index, na.rm = TRUE),
    .groups = "drop") %>%
  complete(分组, Slope_Level, fill = list(Mean_Index = 0, SD_Index = 0))

# =============================================================================
# 2. 可视化实现
# =============================================================================

# --- 颜色与形状配置 ------------------------------------------------------------
# line_colors: 折线颜色向量，使用 Tableau 10 配色方案（色盲友好、期刊常用）。
# 通过 [1:length(actual_groups)] 截取实际需要的颜色数量，避免分组不足时颜色冗余。
line_colors <- c("#4E79A7","#F28E2B","#E15759","#76B7B2","#59A14F",
                 "#EDC948","#B07AA1","#FF9DA7","#9C755F","#BAB0AC")[1:length(actual_groups)]

# shape_values: 点的形状编号（R 基础图形参数）。
# 15=实心方块, 16=实心圆, 17=实心三角, 18=实心菱形, 19=实心圆点...
# 不同形状帮助黑白打印时区分各分组。
shape_values <- c(15,16,17,18,19,20,21,22,23,24)[1:length(actual_groups)]

# --- Y轴边界变量（用于限制阴影区域不超出绘图范围）------------------------------
y_min <- plot_settings$y_axis$limits[1]  # -0.3
y_max <- plot_settings$y_axis$limits[2]  #  0.3

# --- 构建 ggplot2 图形对象 -----------------------------------------------------
# ggplot(): 初始化图形，指定数据框和全局映射（aes）。
#   x = Slope_Level: X轴为坡度等级（1-12）
#   y = Mean_Index: Y轴为各组各坡度等级的均值
#   color = 分组: 按分组变量映射颜色，不同分组显示不同颜色折线
p <- ggplot(lucc_summary, aes(x = Slope_Level, y = Mean_Index, color = 分组)) +
  
  # --- 阴影区域：均值 ± 标准差（Mean ± SD）------------------------------------
# geom_ribbon(): 绘制填充带状区域，表示数据的离散程度（此处用标准差而非置信区间）。
# aes() 内部映射：
#   ymin = pmax(Mean_Index - SD_Index, y_min): 阴影下限 = 均值 - 标准差，
#          但不得低于 Y轴最小值（防止阴影超出绘图边界）
#   ymax = pmin(Mean_Index + SD_Index, y_max): 阴影上限 = 均值 + 标准差，
#          但不得高于 Y轴最大值
#   fill = 分组: 填充颜色按分组映射，与折线颜色一致
# alpha = 0.15: 填充透明度（0=完全透明，1=完全不透明），15% 透明度避免遮挡折线
# color = NA: 不绘制阴影区域的边框线
geom_ribbon(
  aes(
    ymin = pmax(Mean_Index - SD_Index, y_min),
    ymax = pmin(Mean_Index + SD_Index, y_max),
    fill = 分组),
  alpha = 0.15,
  color = NA) +
  
  # --- 均值折线 ----------------------------------------------------------------
# geom_line(): 绘制连接各坡度等级均值的折线。
# linewidth = 0.4: 线宽（ggplot2 3.4.0+ 版本使用 linewidth 替代 size）
geom_line(linewidth = 0.4) +
  
  # --- 均值点标记 --------------------------------------------------------------
# geom_point(): 在折线节点处绘制形状标记，增强可读性。
# aes(shape = 分组): 按分组映射不同形状
# size = plot_settings$legend$symbol_size/12: 将图例参数中的符号大小转换为 ggplot2 的 size 单位
geom_point(aes(shape = 分组), size = plot_settings$legend$symbol_size/12) +
  
  # --- 零参考线（Y = 0）--------------------------------------------------------
# geom_hline(): 绘制水平虚线，标记 Y=0 位置，便于判断指数正负变化。
# linetype = "dashed": 虚线样式
# color = "black": 黑色
# linewidth = 0.4: 线宽
geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 0.2) +
  
  # --- 时期分隔线（X = 6）------------------------------------------------------
# geom_vline(): 绘制垂直虚线，分隔两个时期。
# 前 6 个坡度等级（1-6）为第一时期，后 6 个（7-12）为第二时期。
# xintercept = 6: 在 X=6 处绘制（即第6和第7个等级之间）
geom_vline(xintercept = 6, linetype = "dashed", color = "black", linewidth = 0.2) +
  
  # --- X轴比例尺设置 -----------------------------------------------------------
# scale_x_continuous(): 自定义连续型 X 轴。
#   name: 轴标题
#   breaks: 刻度位置（1 到 12）
#   labels: 刻度标签（从 x_labels 数据框中提取）
#   expand = expansion(0.015, 0): 在坐标轴两端添加微小留白（1.5%），
#         防止最左侧/最右侧的点或标签被边缘截断
#   limits: 强制 X轴范围固定在 1.0 到 12.0
scale_x_continuous(
  name = plot_settings$x_axis$title,
  breaks = x_labels$Slope_Level,
  labels = x_labels$Label,
  expand = expansion(0.015, 0),
  limits = c(1.0, 12.0)
) +
  
  # --- Y轴比例尺设置 -----------------------------------------------------------
# scale_y_continuous(): 自定义连续型 Y 轴。
#   labels = sprintf(...): 使用格式化字符串将刻度值保留两位小数（如 -0.30, -0.20...）
scale_y_continuous(
  name = plot_settings$y_axis$title,
  limits = plot_settings$y_axis$limits,
  breaks = plot_settings$y_axis$breaks,
  labels = sprintf(plot_settings$y_axis$labels_format, plot_settings$y_axis$breaks)) +
  
  # --- 手动映射颜色、填充色、形状 ----------------------------------------------
# scale_color_manual(): 折线和点的外框颜色
# scale_fill_manual(): 阴影填充颜色（与折线颜色一致）
# scale_shape_manual(): 点的形状
scale_color_manual(values = line_colors) +
  scale_fill_manual(values = line_colors) +
  scale_shape_manual(values = shape_values) +
  
  # --- 主题设置 ----------------------------------------------------------------
# theme_minimal(): 使用极简主题作为基础（无灰色背景，无边框线）
# base_size = plot_settings$base_size: 设置基础字号
theme_minimal(base_size = plot_settings$base_size) +
  
  # 通过 theme() 逐项覆盖主题元素，实现精细控制
  theme(
    # 坐标轴线：绘制 X/Y 轴线
    axis.line = element_line(color = plot_settings$line_color, linewidth = plot_settings$axis$line_width),
    
    # 刻度线：绘制坐标轴刻度
    axis.ticks = element_line(color = plot_settings$line_color, linewidth = plot_settings$axis$tick_width),
    
    # 刻度线长度
    axis.ticks.length = unit(plot_settings$x_axis$tick_length, "cm"),
    
    # X轴刻度标签样式
    axis.text.x = element_text(
      family = plot_settings$x_axis$label_family,   # 字体：Times New Roman
      size = plot_settings$x_axis$label_size,       # 字号：35
      color = plot_settings$x_axis$label_color,     # 颜色：黑色
      face = "bold"),                               # 加粗
    
    # Y轴刻度标签样式
    axis.text.y = element_text(
      family = plot_settings$y_axis$label_family,
      size = plot_settings$y_axis$label_size,
      color = plot_settings$y_axis$label_color,
      face = "bold"),
    
    # 图例位置：底部
    legend.position = "bottom",
    
    # 图例行间距
    legend.spacing.y = unit(plot_settings$legend$row_spacing, "cm"),
    
    # 图例键（色块/符号背景）高度和宽度
    legend.key.height = unit(plot_settings$legend$key_height, "cm"),
    legend.key.width = unit(plot_settings$legend$key_width, "cm"),
    
    # 图例标题文字样式
    legend.title = element_text(size = plot_settings$legend$title_size),
    
    # 图例项文字样式
    legend.text = element_text(size = plot_settings$legend$text_size),
    
    # 面板边框：绘制一个黑色矩形边框包围整个绘图区域
    # fill = NA: 内部不填充
    # size = 0.4: 边框线宽（ggplot2 旧版参数名；新版为 linewidth）
    panel.border = element_rect(color = "black", fill = NA, size = 0.2),
    
    # 隐藏网格线（panel.grid = element_blank() 同时移除主网格和次网格）
    panel.grid = element_blank(),
    
    # 全局文字字体：所有未显式指定字体的文字元素默认使用 Times New Roman
    text = element_text(family = "TimesNewRoman")) +
  
  # --- 图例指南（guides）------------------------------------------------------
# guides(): 精细控制各美学映射（color/fill/shape）的图例显示方式。
# color = guide_legend(...): 控制颜色图例
#   title: 图例标题
#   nrow = 2: 图例排成 2 行（水平排列，适合底部放置）
#   override.aes: 覆盖图例中的默认美学参数
#     shape: 显示形状符号
#     size: 符号大小
#     linetype = 1: 实线
#     linewidth: 线宽
#     fill = NA: 图例键中不显示填充色（避免与阴影填充混淆）
# fill = "none": 不显示独立的 fill（填充色）图例，因为 fill 已合并到 color 图例中
# shape = "none": 不显示独立的 shape 图例，因为 shape 已合并到 color 图例中
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

# =============================================================================
# 3. 输出图形
# =============================================================================

# --- 保存为 TIFF 文件 ----------------------------------------------------------
# ggsave(): ggplot2 的标准保存函数，自动根据文件扩展名选择输出设备。
# 参数说明：
#   filename: 输出文件名
#   plot = p: 指定要保存的图形对象（若省略，默认保存最后绘制的图形）
#   path: 输出目录
#   width / height: 图片尺寸（单位由 units 参数指定）
#   units = "cm": 尺寸单位：厘米
#   dpi = 600: 分辨率 600 dpi，满足 Nature/Science 等顶级期刊印刷要求
#   compression = "lzw": TIFF 压缩方式（LZW 无损压缩，减小文件体积且不影响画质）
ggsave(
  filename = "LUCC_plot_final1.tiff",
  plot = p,
  path = output_path,
  width = plot_settings$output$width,
  height = plot_settings$output$height,
  units = "cm",
  dpi = plot_settings$output$dpi,
  compression = "lzw"
)

# --- 在 R 图形窗口中显示图形 ---------------------------------------------------
# print(p): 将图形对象输出到当前图形设备（RStudio Plots 面板或独立窗口），
# 便于在保存前预览最终效果。
print(p)