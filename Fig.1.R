# ==========================================
# 1. 加载必要的R包
# ==========================================

library(readxl)    # 用于读取Excel文件中的数据
library(ggplot2)   # 核心绘图包，提供ggplot图形系统
library(tidyr)     # 用于数据长宽格式转换（pivot_longer等）
library(dplyr)     # 用于数据清洗、筛选和汇总
library(purrr)     # 用于循环处理多个工作表（map_dfr等）
library(scales)     # 用于控制坐标轴刻度格式（保留小数位数）

# ==========================================
# 2. 用户自定义参数配置区（可在此修改样式）
# ==========================================

# 定义Excel数据文件的完整路径
excel_path <- "C:/Users/lenovo/Desktop/全球坡耕地研究数据/3第三篇论文整理/手工整理后形成数据/数据处理/total地形坡谱.xlsx"

# 定义输出图片的保存目录路径
output_dir <- "C:/Users/lenovo/Desktop/全球坡耕地研究数据/3第三篇论文整理/手工整理后形成数据/初稿、图件和表格/图片重绘/图20260511"

# 定义画布总宽度，单位为厘米
CANVAS_WIDTH <- 16

# 定义画布总高度，单位为厘米（大幅调高以容纳底部标签和图例）
CANVAS_HEIGHT <- 8

# 定义输出图片的分辨率（DPI，每英寸点数）
DPI_SETTING <- 500

# 定义全局字体系列（如 "sans"、"serif"、"Times New Roman"）
FONT_FAMILY <- "Times New Roman"

# 定义纵轴刻度数字的字体大小
AXIS_NUM_SIZE <- 7

# 定义坡度段标签的字体大小（调小防止文字过大被裁剪）
LABEL_TEXT_SIZE <- 2.25

# 定义Sheet名称标签的字体大小
SHEET_LABEL_SIZE <- 0

# 定义图表标题的字体大小
TITLE_FONT_SIZE <- 14

# 定义每个坡度段的显示标签内容
slope_labels <- c("0-5°", "5-10°", "10-15°", "15-25°", "25-35°", ">35°")

# ==========================================
# 3. 路径检查与工作表初始化
# ==========================================

# 检查输出目录是否存在，若不存在则递归创建完整路径
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# 读取Excel文件中所有工作表的名称列表
sheet_names <- excel_sheets(excel_path)

# 从列表中排除名为"total地形坡谱"的汇总表，只保留分表
sheet_names <- sheet_names[!sheet_names %in% c("total地形坡谱")]

# ==========================================
# 4. 定义辅助数据框与函数
# ==========================================

# 创建坡度范围与显示标签的映射数据框
# 坡度范围列必须与Excel清洗后的列名完全匹配
slope_mapping <- data.frame(
  坡度范围 = c("0--5", "5--10", "10--15", "15--25", "25--35", "35"),  # 内部匹配用标准格式
  坡度标签 = slope_labels,                                            # 图上显示的标签文字
  坡度序号 = 1:6,                                                     # 横坐标顺序编号
  stringsAsFactors = FALSE                                            # 禁止自动转为因子型，避免匹配失败
)

# 定义列名标准化函数：将Excel中各种格式的坡度列头统一为映射表格式
standardize_slope_name <- function(x) {
  x <- as.character(x)                          # 强制转换为字符型，防止因子或数值型干扰
  x <- gsub("\\s+|°|>|＞|≥", "", x)             # 去除所有空格、度符号°、各种大于号
  x <- gsub("~|－|—", "-", x)                  # 统一全角横线、波浪线为英文短横线
  x <- gsub("^0[-–]5$", "0--5", x)             # 将单横线或全角横线的坡度格式转为双横线
  x <- gsub("^5[-–]10$", "5--10", x)
  x <- gsub("^10[-–]15$", "10--15", x)
  x <- gsub("^15[-–]25$", "15--25", x)
  x <- gsub("^25[-–]35$", "25--35", x)
  x <- gsub("^35[+]?$", "35", x)               # 处理"35"或"35+"格式，统一为"35"
  return(x)                                     # 返回清洗后的列名字符串
}

# 定义曲线颜色方案（当曲线数超过4条时会自动循环或扩展）
curve_colors <- c(
  "#00A06B", "#ff7f0e", "#e377c2", "#000000", "#00A06B",
  "#ff7f0e", "#e377c2", "#000000", "#00A06B", "#ff7f0e",
  "#e377c2", "#000000", "#00A06B", "#ff7f0e", "#e377c2",
  "#000000", "#00A06B", "#ff7f0e", "#e377c2", "#000000"
)

# ==========================================
# 5. 主循环：按每批4个Sheet依次处理
# ==========================================

# 从第1个Sheet开始，每次步进4个，直到遍历完所有Sheet
for (i in seq(1, length(sheet_names), by = 4)) {
  
  # 获取当前批次的Sheet名称列表（最多4个，防止超出总数量）
  current_sheets <- sheet_names[i:min(i + 3, length(sheet_names))]
  
  # ========================================
  # 5.1 读取并合并当前批次的所有Sheet数据
  # ========================================
  
  # 使用map_dfr循环读取每个Sheet，并纵向合并为一个统一的数据框
  combined_data <- map_dfr(seq_along(current_sheets), function(idx) {
    
    # 获取当前Sheet的名称
    sheet_name <- current_sheets[idx]
    
    # 从Excel中读取当前Sheet的原始数据框
    raw_df <- read_excel(excel_path, sheet = sheet_name)
    
    # 在控制台打印当前Sheet的原始列名（调试用，便于核对格式）
    message(">>> [", sheet_name, "] 原始列名: ", 
            paste(colnames(raw_df)[-1], collapse = " | "))
    
    # 数据处理管道：清洗 -> 转长格式 -> 匹配映射表
    raw_df %>%
      rename(YearLabel = 1) %>%                                    # 将第一列重命名为年份标签
      mutate(across(-1, ~ replace_na(as.numeric(.x), 0))) %>%      # 除首列外全转为数值型，缺失值填0
      pivot_longer(
        cols = -1,                                                 # 除首列外全部转为长格式
        names_to = "坡度范围原始",                                  # 原列名存入此列
        values_to = "比例值"                                        # 数值存入此列
      ) %>%
      mutate(
        坡度范围 = standardize_slope_name(坡度范围原始),            # 标准化坡度列名以匹配映射表
        SheetName = sheet_name,                                     # 记录数据来源的Sheet名称
        SheetIndex = idx                                            # 记录Sheet在当前批次的序号（1-4）
      ) %>%
      left_join(slope_mapping, by = "坡度范围") %>%                # 左连接映射表，获取坡度序号和显示标签
      mutate(CurveID = paste(SheetName, YearLabel, sep = " - "))   # 生成唯一曲线标识符（Sheet+年份）
  })
  
  # ========================================
  # 5.2 检查列名匹配情况（调试与容错）
  # ========================================
  
  # 筛选出未能匹配到坡度序号的记录（即映射失败的列）
  unmatched <- combined_data %>% 
    filter(is.na(坡度序号)) %>%                                    # 坡度序号为NA表示未匹配成功
    distinct(坡度范围原始, 坡度范围)                                # 去重显示原始列名和清洗后的列名
  
  # 若存在未匹配的列名，在控制台发出警告并打印详细信息供用户核对
  if (nrow(unmatched) > 0) {
    warning("以下坡度列名未能匹配到预设标签，请检查Excel列头格式：\n",
            paste(capture.output(print(unmatched)), collapse = "\n"))
  }
  
  # ========================================
  # 5.3 计算每个数据点的横坐标位置
  # ========================================
  
  # 定义相邻坡度段之间的水平间距（数据坐标单位）
  slope_spacing <- 5
  
  # 计算单个Sheet在横轴上所占的总宽度（6个坡度段 × 间距）
  sheet_width <- 6 * slope_spacing
  
  # 为每个数据点计算其在图表中的最终横坐标位置
  combined_data <- combined_data %>%
    filter(!is.na(坡度序号)) %>%                                    # 过滤掉匹配失败的行，防止NA传播导致错误
    group_by(SheetIndex) %>%                                        # 按Sheet分组，独立计算每个Sheet内的位置
    mutate(
      横坐标位置 = (坡度序号 - 0.5) * slope_spacing + (SheetIndex - 1) * (sheet_width + 2)
    ) %>%
    ungroup()                                                       # 解除分组，恢复平铺数据框
  
  # ========================================
  # 5.4 生成平滑插值曲线数据（样条插值）
  # ========================================
  
  # 统计当前图表中不同曲线的总数
  n_curves <- n_distinct(combined_data$CurveID)
  
  # 若预设颜色数量不足，使用colorRampPalette自动扩展颜色方案
  if (n_curves > length(curve_colors)) {
    curve_colors <- colorRampPalette(curve_colors)(n_curves)
  }
  
  # 对每个CurveID进行自然样条插值，生成平滑曲线点
  interp_data <- combined_data %>%
    group_by(CurveID) %>%
    group_modify(~ {
      .x <- .x %>% arrange(坡度序号)                               # 按坡度序号升序排列，确保插值方向正确
      n_pts <- nrow(.x)                                             # 获取当前曲线的有效数据点数量
      
      # 若数据点不足2个，无法进行任何插值，返回空数据框避免报错
      if (n_pts < 2) {
        return(tibble(比例值 = numeric(0), 横坐标位置 = numeric(0)))
      }
      
      # 若数据点不足4个，不使用样条插值（易报错），直接返回原始点连线
      if (n_pts < 4) {
        return(tibble(比例值 = .x$比例值, 横坐标位置 = .x$横坐标位置))
      }
      
      # 使用自然样条插值，生成200个点使曲线视觉上平滑
      spline_fit <- spline(
        x = .x$坡度序号,
        y = .x$比例值,
        n = 200,
        method = "natural"
      )
      
      # 返回插值结果，并确保比例值不会为负数（pmax与0取大）
      tibble(
        比例值 = pmax(spline_fit$y, 0),
        横坐标位置 = spline_fit$x * slope_spacing + (first(.x$SheetIndex) - 1) * (sheet_width + 2) - slope_spacing / 2
      )
    }) %>%
    ungroup()                                                       # 解除分组
  
  # ========================================
  # 5.5 准备底部横坐标标签数据
  # ========================================
  
  # 提取坡度标签的横坐标位置（每个Sheet中每个坡度段只保留一个位置）
  x_labels_pos <- combined_data %>%
    filter(!is.na(坡度序号), !is.na(坡度标签), !is.na(横坐标位置)) %>%  # 过滤任何含NA的行，确保标签可渲染
    distinct(坡度序号, 坡度标签, SheetIndex, 横坐标位置) %>%            # 去重，防止同一点重复标注
    arrange(SheetIndex, 坡度序号)                                        # 按Sheet和坡度排序
  
  # 计算每个Sheet名称的居中横坐标位置（取该Sheet所有点横坐标的平均值）
  sheet_titles_pos <- combined_data %>%
    filter(!is.na(SheetIndex), !is.na(SheetName), !is.na(横坐标位置)) %>%
    group_by(SheetIndex, SheetName) %>%
    summarise(
      x_mid = mean(横坐标位置),                                     # 均值即为该Sheet的居中位置
      .groups = "drop"
    )
  
  # ========================================
  # 5.6 绘制图表（核心可视化部分）
  # ========================================
  
  # 初始化ggplot对象
  p <- ggplot() +
    
    # 绘制平滑曲线（使用插值后的密集点）
    geom_line(
      data = interp_data,
      aes(x = 横坐标位置, y = 比例值, color = CurveID),
      linewidth = 0.3,
      alpha = 0.9
    ) +
    
    # 绘制原始数据散点（突出显示实际观测值）
    geom_point(
      data = combined_data,
      aes(x = 横坐标位置, y = 比例值, color = CurveID),
      shape = 16,
      size = 1.3,
      alpha = 0.9
    ) +
    
    # 绘制Sheet之间的垂直虚线分隔线（便于区分不同区域）
    geom_vline(
      xintercept = sheet_titles_pos$x_mid + (sheet_width + 2) / 2,
      linetype = "dashed",
      linewidth = 0.25,
      color = "black",
      alpha = 0.8
    ) +
    
    # 手动映射曲线颜色（使用预设或扩展后的颜色方案）
    scale_color_manual(values = curve_colors) +
    
    # 设置横坐标：彻底隐藏默认刻度线和标签，仅保留轴线
    scale_x_continuous(
      name = "",                                                    # X轴标题设为空
      breaks = unique(x_labels_pos$横坐标位置),                      # 刻度位置与标签对齐（虽然已隐藏文字）
      labels = NULL,                                                # 不显示默认刻度标签
      expand = expansion(mult = 0.0)                                # 不扩展横轴边界，紧贴数据范围
    ) +
    
    # 设置纵坐标：范围0-0.8，强制保留两位小数，不扩展上下边界
    scale_y_continuous(
      name = "比例",
      limits = c(0, 0.8),
      labels = label_number(accuracy = 0.01),
      expand = c(0, 0)
    ) +
    
    # 在y=0处绘制一条黑色水平线，强化x轴视觉效果
    geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
    
    # 添加图表主标题
    labs(
      title = "",
      color = "Sheet - 年份"
    ) +
    
    # 关键设置：限制y轴显示范围为0-0.8，但关闭裁剪，允许标签绘制在面板外
    coord_cartesian(ylim = c(0, 0.8), clip = "off") +
    
    # 应用黑白基础主题，并指定全局字体
    theme_bw(base_family = FONT_FAMILY) +
    
    # 自定义主题细节（控制所有视觉元素）
    theme(
      panel.grid.major = element_blank(),                           # 移除主网格线，保持图面干净
      panel.grid.minor = element_blank(),                           # 移除次网格线
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.2),  # 保留面板黑色边框
      text = element_text(family = FONT_FAMILY),                    # 全局统一字体系列
      plot.title = element_text(hjust = 0.5, size = TITLE_FONT_SIZE, face = "plain"),  # 标题居中、加粗
      axis.text.y = element_text(size = AXIS_NUM_SIZE, color = "black", face = "plain"),  # 纵轴刻度加粗
      axis.text.x = element_blank(),                                # 隐藏默认X轴刻度文字（我们用geom_text自定义）
      axis.ticks.x = element_line(),                                # 保留X轴刻度线（与geom_text位置对应）
      axis.line.x = element_line(color = "black", linewidth = 0.2),
      axis.line.y = element_line(color = "black", linewidth = 0.2),  # 刻度线样式
      axis.ticks.length = unit(0.8, "mm"),# 刻度线长度
      axis.title.x = element_blank(),                               # 移除X轴标题
      axis.title.y = element_text(size = AXIS_NUM_SIZE, margin = margin(r = 10)),  # 纵轴标题及与轴线的间距
      axis.line = element_line(color = "black", linewidth = 0.2),   # 坐标轴线样式
      legend.position = "none",                                   # 图例放置在底部
      legend.box.spacing = unit(5, "mm"),                           # 图例框与图形主体的间距
      legend.key.size = unit(1, "mm"),                              # 图例中色块的大小
      legend.text = element_text(size = 8),                         # 图例文字大小
      legend.title = element_text(size = 2),                        # 图例标题大小
      # 关键：大幅增加底部边距（60mm），为横坐标标签留出充足物理空间
      plot.margin = margin(t = 0.5, r = 1, b = 8, l = 0.5, unit = "mm")
    ) +
    
    # 设置图例排列：自动计算所需行数，最多3行，并覆盖点的大小和透明度
    guides(
      color = guide_legend(
        nrow = min(2, ceiling(n_curves / 1)),
        override.aes = list(size = 1, alpha = 1)
      )
    ) +
    
    # ========================================
  # 5.7 添加自定义横坐标标签（参考上传代码方案）
  # ========================================
  
  # 添加坡度段标签：锚点设在y=0（x轴线），vjust=2.0使文本向下偏移显示在轴下方
  geom_text(
    data = x_labels_pos,
    aes(x = 横坐标位置, y = 0, label = 坡度标签),
    size = LABEL_TEXT_SIZE,                                       # 使用较小字体防止超出画布
    color = "black",
    angle = 0,                                                    # 水平显示（不旋转）
    fontface = "plain",                                            # 加粗显示
    vjust = 2.0,                                                  # 垂直调整：>1时文本向下偏移，显示在x轴下方
    hjust = 0.5,                                                  # 水平居中对齐
    family = FONT_FAMILY,
    inherit.aes = FALSE                                           # 不继承全局aes映射，避免冲突
  ) +
    
    # 添加Sheet名称标签：锚点同样设在y=0，vjust=3.5使文本显示在坡度标签更下方
    geom_text(
      data = sheet_titles_pos,
      aes(x = x_mid, y = 0, label = SheetName),
      size = SHEET_LABEL_SIZE,                                      # Sheet名称字体略大于坡度标签
      color = "black",
      angle = 0,                                                    # 水平显示
      fontface = "plain",
      vjust = 1.5,                                                  # 更大的向下偏移，位于坡度标签下方
      hjust = 0.5,
      family = FONT_FAMILY,
      inherit.aes = FALSE
    )
  
  # ========================================
  # 5.8 导出高清TIFF图片
  # ========================================
  
  # 构造输出文件的完整路径和文件名
  out_file <- file.path(output_dir, paste0("Combined_Chart_", i, ".tiff"))
  
  # 使用ggsave保存图片，参数与画布设置保持一致
  ggsave(
    filename = out_file,
    plot = p,
    device = "tiff",
    dpi = DPI_SETTING,
    width = CANVAS_WIDTH,
    height = CANVAS_HEIGHT,
    units = "cm",
    bg = "white"
  )
  
  # 在控制台打印当前批次的完成信息
  message(">>> 已完成批次保存: ", out_file)
}