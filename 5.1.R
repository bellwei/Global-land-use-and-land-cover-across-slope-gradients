# =============================================================================
# PCA双标图（Biplot）绘制脚本
# 功能：基于主成分分析结果，绘制分组样本得分与变量载荷的联合可视化图形
# 适用场景：展示不同年份（2000/2010/2020）样本在PC1-PC2二维空间中的分布
#           及各变量对主成分的贡献方向和强度
# =============================================================================

# -----------------------------------------------------------------------------
# 0. 加载所需R包
# -----------------------------------------------------------------------------
library(readxl)    # 读取Excel格式数据文件
library(ggplot2)   # 数据可视化核心包（基于图层语法的绘图系统）
library(dplyr)     # 数据清洗与变换（提供管道操作符 %>% 及数据处理函数）
library(ggrepel)   # 智能标签布局（自动调整文本标签位置以避免重叠）

# -----------------------------------------------------------------------------
# 1. 数据准备
# -----------------------------------------------------------------------------

# --- 1.1 读取样本得分数据（PCA变换后的样本坐标） ---
pca_scores <- read_excel(
  "C:/Users/lenovo/Desktop/全球坡耕地研究数据/3第三篇论文整理/手工整理后形成数据/回归分析/图件制作/PCA2000-2020.xlsx", 
  sheet = "PCA后数据"  # 指定Excel工作表名称
) %>% 
  mutate(Group = factor(Group))  # 将Group列转换为因子类型，用于后续分组着色和形状映射

# --- 1.2 读取变量载荷数据（原始变量在主成分空间中的投影） ---
loadings <- read_excel(
  "C:/Users/lenovo/Desktop/全球坡耕地研究数据/3第三篇论文整理/手工整理后形成数据/回归分析/图件制作/载荷系数矩阵.xlsx", 
  sheet = "Sheet1"
) %>% 
  mutate(
    # 使用group1列作为分组依据，与样本数据的Group对应
    Group = factor(group1),
    # 计算每个变量到原点的欧氏距离（载荷向量模长），用于衡量变量重要性
    # 模长越大，表示该变量对PC1和PC2的整体贡献越大
    magnitude = sqrt(主成分1^2 + 主成分2^2),
    # 仅显示重要变量的标签：筛选模长位于前80%分位数以上的变量
    # 避免图形中标签过多导致视觉混乱
    display_label = if_else(magnitude > quantile(magnitude, 0.8), variable, "")
  ) %>% 
  group_by(Group) %>%           # 按年份分组
  top_n(10, magnitude) %>%       # 从每组中选取模长最大的前9个变量（保留最重要的变量）
  ungroup()                     # 解除分组状态，恢复完整数据框

# -----------------------------------------------------------------------------
# 2. 可视化参数设置
# -----------------------------------------------------------------------------

# --- 2.1 形状映射（区分不同年份的样本点形状） ---
shape_mapping <- c(
  "2000" = 16,   # 实心圆
  "2010" = 17,   # 实心三角形
  "2020" = 15    # 实心方块
)

# --- 2.2 颜色映射（区分不同年份的配色方案） ---
# 采用ColorBrewer Set1调色板，具有高对比度和色盲友好特性
color_mapping <- c(
  "2000" = "#E41A1C",  # 红色
  "2010" = "#377EB8",  # 蓝色
  "2020" = "#4DAF4A"   # 绿色
)

# --- 2.3 箭头缩放系数 ---
# 将载荷系数乘以此系数进行视觉放大，使箭头在图中更加清晰可辨
# 需要根据实际数据范围反复调试以获得最佳视觉效果
arrow_scale <- 3.5

# -----------------------------------------------------------------------------
# 3. 绘制PCA双标图
# -----------------------------------------------------------------------------

ggplot() +  # 初始化空白画布，后续逐层添加图形元素
  
  # --- 3.1 添加坐标轴参考线 ---
  geom_hline(
    yintercept = 0,           # 水平线位于y=0处
    color = "black",          # 黑色
    linewidth = 0.3           # 线宽0.3毫米（较细的参考线）
  ) +
  geom_vline(
    xintercept = 0,           # 垂直线位于x=0处
    color = "black", 
    linewidth = 0.3
  ) +
  
  # --- 3.2 添加分组置信椭圆（带半透明填充） ---
  # 功能：展示每个年份组样本点在二维空间中的分布范围和聚集程度
  stat_ellipse(
    data = pca_scores,                          # 使用样本得分数据
    aes(x = FAC20001, y = FAC20002, fill = Group),  # 映射坐标和填充颜色
    geom = "polygon",                           # 绘制多边形填充区域
    type = "t",                                 # 使用t分布椭圆（适合小样本）
    level = 0.9,                                # 90%置信水平
    alpha = 0.3,                                # 填充透明度30%（避免遮挡样本点）
    linewidth = 1.2,                            # 椭圆边框线宽
    linetype = "solid"                          # 实线边框
  ) +
  
  # --- 3.3 添加样本散点 ---
  geom_point(
    data = pca_scores,
    aes(
      x = FAC20001,      # 第一主成分得分（x轴坐标）
      y = FAC20002,      # 第二主成分得分（y轴坐标）
      shape = Group,     # 形状按年份分组
      color = Group      # 颜色按年份分组
    ),
    size = 1,            # 点的大小
    alpha = 0.8          # 点的透明度（80%不透明，便于观察重叠区域）
  ) +
  
  # --- 3.4 添加变量载荷箭头（按年份分组，使用不同颜色） ---
  # 箭头起点为原点(0,0)，终点为载荷坐标乘以缩放系数
  # 箭头方向表示变量与主成分的正/负相关关系
  # 箭头长度表示变量对主成分的贡献强度
  geom_segment(
    data = loadings,
    aes(
      x = 0,                            # 箭头起点x坐标（原点）
      y = 0,                            # 箭头起点y坐标（原点）
      xend = 主成分1 * arrow_scale,     # 箭头终点x坐标（PC1载荷 × 缩放系数）
      yend = 主成分2 * arrow_scale,     # 箭头终点y坐标（PC2载荷 × 缩放系数）
      color = Group                     # 箭头颜色按年份分组
    ),
    arrow = arrow(
      length = unit(0.15, "cm"),        # 箭头头部大小
      type = "closed"                   # 闭合箭头（实心三角形箭头head）
    ),
    linewidth = 0.5,                    # 箭头线宽
    alpha = 0.9                         # 箭头透明度
  ) +
  
  # --- 3.5 添加变量标签（使用智能防重叠布局） ---
  geom_text_repel(
    data = loadings,
    aes(
      x = 主成分1 * arrow_scale,        # 标签x坐标（与箭头终点对齐）
      y = 主成分2 * arrow_scale,        # 标签y坐标（与箭头终点对齐）
      label = display_label,             # 显示的标签文本（仅重要变量）
      color = Group                      # 标签颜色与分组一致
    ),
    size = 2.5,                          # 标签字体大小（2.5毫米）
    box.padding = 0.8,                   # 标签与数据点之间的内边距
    max.overlaps = 50,                   # 最大允许的标签重叠数量
    segment.color = "grey40",            # 连接线颜色（灰色）
    min.segment.length = 0.2,            # 连接线的最小长度（避免过短线段）
    force = 2                            # 标签间的排斥力强度（值越大间距越大）
  ) +
  
  # --- 3.6 设置x轴（PC1）刻度与范围 ---
  scale_x_continuous(
    # 格式化坐标轴标签：保留两位小数
    labels = function(x) sprintf("%.2f", x),
    # 自动生成约8个均匀分布的主刻度
    breaks = scales::pretty_breaks(n = 8),
    # 在坐标轴两端增加15%的空白边距，避免数据点紧贴边框
    expand = expansion(mult = 0.15)
  ) +
  
  # --- 3.7 设置y轴（PC2）刻度与范围 ---
  scale_y_continuous(
    labels = function(y) sprintf("%.2f", y),
    breaks = scales::pretty_breaks(n = 8),
    expand = expansion(mult = 0.15)
  ) +
  
  # --- 3.8 应用手动形状和颜色映射 ---
  scale_shape_manual(values = shape_mapping) +   # 应用预定义的形状映射
  scale_color_manual(values = color_mapping) +   # 应用预定义的颜色映射
  scale_fill_manual(values = color_mapping) +    # 椭圆填充色使用相同映射
  
  # --- 3.9 设置坐标轴标题和图形标题 ---
  labs(
    # x轴标题：PC1及方差解释率
    x = "PC1 (70.707% , 71.097%, and 68.529% explained 
         variance in 2000,2010,and 2020)",
    # y轴标题：PC2及方差解释率
    y = "PC2 (13.219% , 13.455%, and 14.266% explained 
         variance in 2000,2010,and 2020)",
    # 图形主标题
    title = "Group-specific PCA Biplot with Loadings"
  ) +
  
  # --- 3.10 应用黑白主题并自定义细节 ---
  theme_bw() +  # 使用黑白网格背景主题（学术论文常用）
  theme(
    legend.position = "right",           # 图例放置在右侧
    panel.grid = element_blank(),        # 移除背景网格线（保持图形简洁）
    # 设置刻度线样式
    axis.ticks = element_line(color = "black", linewidth = 0.5),
    # 设置绘图边框样式
    panel.border = element_rect(color = "black", linewidth = 1),
    # 标题居中、加粗、9磅字号
    plot.title = element_text(hjust = 0.5, face = "bold", size = 9),
    # 坐标轴刻度文字：黑色、9磅
    axis.text = element_text(color = "black", size = 9),
    # 坐标轴标题：9磅
    axis.title = element_text(size = 9),
    # 图例文字：8磅
    legend.text = element_text(size = 8),
    # 图例标题：加粗
    legend.title = element_text(face = "bold")
  ) +
  
  # --- 3.11 自定义图例显示规则 ---
  guides(
    # color图例：显示为"Year Group"标题，覆盖点大小和线型样式
    color = guide_legend(
      title = "Year Group",
      override.aes = list(size = 1, linetype = 1)
    ),
    # shape图例：显示为"Year Group"标题
    shape = guide_legend(title = "Year Group"),
    # fill图例：不显示（避免与color图例重复）
    fill = "none"
  )

# -----------------------------------------------------------------------------
# 4. 保存图形（TIFF高分辨率栅格格式，适合期刊投稿）
# -----------------------------------------------------------------------------

# --- 4.1 方式一：使用ggsave默认参数保存 ---
ggsave(
  filename = "PCA_Biplot.tiff",    # 输出文件名（TIFF格式）
  # 输出路径（请根据实际环境修改）
  path = "C:/Users/lenovo/Desktop/全球坡耕地研究数据/3第三篇论文整理/手工整理后形成数据/回归分析/图件制作/",
  width = 6.5,                      # 图像宽度（英寸）
  height = 4.12,                    # 图像高度（英寸）
  dpi = 700                         # 分辨率（每英寸像素数，700dpi满足印刷要求）
)

# --- 4.2 方式二：显式指定TIFF设备参数保存（更精细控制） ---
ggsave(
  filename = "PCA_Biplot.tiff",    # 输出文件名
  device = "tiff",                  # 明确指定输出设备为TIFF格式
  width = 6.5,                      # 宽度（英寸）
  height = 4.12,                    # 高度（英寸）
  units = "in",                     # 尺寸单位：英寸（inches）
  bg = "white",                     # 背景颜色：白色（适合印刷）
  dpi = 700                         # 高分辨率，满足学术期刊印刷质量要求
)