####### 所有需要加载的包，提前加载
library(dplyr)
library(purrr)
library(broom)
library(gt)
library(mice)
library(tibble)
library(car)  
library(lmtest)
library(sandwich)
library(interactions)
library(emmeans)
library(mitools)
library(ggplot2)
library(tidyr)
library(grid) 
library(flextable)
library(officer)
#######



# 数据导入
library(readxl)
mydata <- read_excel("D:/研究项目/ASD/数据分析/mydata.xlsx")

var1 <- c(
  "WPPSI",
  "PPVT_Pre", "PPVT_Post",
  "EVT_Pre",  "EVT_Post",
  "SRS_Pre",  "SRS_Post",
  "JA_Pre",   "JA_Post",
  "SI_Pre",   "SI_Post",
  "JE_Pre",   "JE_Post"
)

var_srs <- c(
  "SocialAwareness_Pre",   "SocialAwareness_Post",
  "SocialCognition_Pre",   "SocialCognition_Post",
  "SocialCommunication_Pre","SocialCommunication_Post",
  "SocialMotivation_Pre",  "SocialMotivation_Post",
  "RRB_Pre",               "RRB_Post"
)

# Group 设为因子，其他转为数值
mydata$Group <- as.factor(mydata$Group)
mydata[, var1] <- lapply(mydata[, var1], as.numeric)
mydata$Age <- as.numeric(mydata$Age)
mydata[, var_srs] <- lapply(mydata[, var_srs], as.numeric)


########### Baseline Characteristics and Equivalence ###################


# 变量映射,确保数据变量与表格变量对应

var_map <- c(
  Age = "Age [months, mean (SD)]",
  Gender = "Gender [Female (%)]",
  WPPSI = "WPPSI (mean, SD)",
  PPVT_Pre = "PPVT (mean, SD)",
  EVT_Pre = "EVT (mean, SD)",
  SocialAwareness_Pre = "Social Awareness (mean, SD)",
  SocialCognition_Pre = "Social Cognition (mean, SD)",
  SocialCommunication_Pre = "Social Communication (mean, SD)",
  SocialMotivation_Pre = "Social Motivation (mean, SD)",
  RRB_Pre = "RRB (mean, SD)",
  SRS_Pre = "SRS (mean, SD)",
  JA_Pre = "Joint Attention (mean, SD)",
  SI_Pre = "Social Initiating (mean, SD)",
  JE_Pre = "Joint Engagement (mean, SD)"
)

vars <- names(var_map)


# 分组

dat_i <- mydata %>% filter(Group == 1)
dat_c <- mydata %>% filter(Group == 0)

n_i <- nrow(dat_i)
n_c <- nrow(dat_c)


# Gender 处理

# 计算每组女性人数和百分比
n_female_i <- sum(dat_i$Gender == "2", na.rm = TRUE)
n_female_c <- sum(dat_c$Gender == "2", na.rm = TRUE)

n_gender_i <- sum(!is.na(dat_i$ID))
n_gender_c <- sum(!is.na(dat_c$ID))

perc_female_i <- n_female_i / n_gender_i * 100
perc_female_c <- n_female_c / n_gender_c * 100

gender_row <- tibble(
  Variables = var_map["Gender"],
  Intervention = sprintf("%d (%.1f%%)", n_female_i, perc_female_i),
  Control      = sprintf("%d (%.1f%%)", n_female_c, perc_female_c),
  t = "",  
  p = ""    
)


# 描述性统计、学生t检验完成基线可比性分析

cont_vars <- setdiff(vars, "Gender")

cont_table <- map_df(cont_vars, function(v){
  
  x <- dat_i[[v]]
  y <- dat_c[[v]]
  
  test <- t.test(x, y, var.equal = TRUE, alternative = "two.sided")
  
  tibble(
    Variables     = var_map[v],
    Intervention  = sprintf("%.2f (%.2f)", mean(x, na.rm=TRUE), sd(x, na.rm=TRUE)),
    Control       = sprintf("%.2f (%.2f)", mean(y, na.rm=TRUE), sd(y, na.rm=TRUE)),
    t             = sprintf("%.2f", test$statistic),
    p             = sprintf("%.3f", test$p.value)
  )
})


# 制作表格时，合并性别行与连续变量行
final_table <- bind_rows(gender_row, cont_table)


# 表格输出
gt_tbl2 <- final_table %>%
  gt() %>%
  cols_label(
    Variables    = "Variables",
    Intervention = sprintf("Intervention (n = %d)", n_i),
    Control      = sprintf("Control (n = %d)", n_c),
    t            = "t",
    p            = "p"
  ) %>%
  cols_align(
    align   = "left",
    columns = c(Variables)
  ) %>%
  cols_align(
    align   = "center",
    columns = c(Intervention, Control, t, p)
  ) %>%
  # ======== 样式区域 ========
tab_options(
  column_labels.border.top.style    = "solid",
  column_labels.border.top.color    = "black",
  column_labels.border.top.width    = px(2),

  column_labels.border.bottom.style = "solid",
  column_labels.border.bottom.color = "black",
  column_labels.border.bottom.width = px(2),
  
  table_body.border.bottom.style    = "solid",
  table_body.border.bottom.color    = "black",
  table_body.border.bottom.width    = px(2),
  
  table_body.hlines.style = "none",     
  table_body.vlines.style = "none",     
  table.border.left.style   = "none",
  table.border.right.style  = "none",
  table.border.top.style    = "none"    
)

gt_tbl2


#输出并保存表格
df_tbl2 <- as.data.frame(gt_tbl2)
ft_tbl2 <- flextable(df_tbl2) %>%
  autofit() %>%
  theme_booktabs()
doc <- read_docx() %>%
  body_add_flextable(ft_tbl2)
print(
  doc,
  target = "C:/Users/J.N.RAN/Desktop/ASD图表/gt_tbl2.docx"
)



########### Within-Group Pre–Post Changes ###################
##分析思路为SRS PPVT EVT JA SI JE 一起插补并完成分析
##然后单独对SRS的5个子类别进行插补并完成分析，避免SRS子分类和
##以上变量放一起造成干扰

# 多重插补
library(mice)
dat1_for_imputation <- mydata[, c("Group", var1)] #用于插补的数据
imp1 <- mice(dat1_for_imputation, m = 20, seed = 2025)

# 取第5个数据集检查一下
# dat5 <- complete(imp1, 5)

### 配对样本t检验
library(broom)
measures1 <- c("PPVT", "EVT", "SRS", "JA", "SI", "JE")

run_paired1 <- function(imp1, group_value, measure1) {
  # 在第一个插补数据集里确认列名
  comp1 <- complete(imp1, 1)
  pre_name  <- paste0(measure1, "_Pre")
  post_name <- paste0(measure1, "_Post")
  # 组内样本量
  n_group <- sum(comp1$Group == group_value, na.rm = TRUE)
  # 在每个插补数据集里构造配对t检验
  fit1 <- with(
    imp1,
    {
      pre  <- get(pre_name)
      post <- get(post_name)
      diff <- post - pre
      lm(diff ~ 1, subset = Group == group_value)
    }
  )
  
  pooled <- pool(fit1)
  s <- summary(pooled, conf.int = TRUE)
  
  est <- s$estimate[1]      # 平均差值
  t   <- s$statistic[1]
  p   <- s$p.value[1]
  
  d_z <- as.numeric(t) / sqrt(n_group)  # 配对效应量
  
  tibble(
    measure   = measure1,
    group     = group_value,
    n         = n_group,
    mean_diff = est,
    t         = t,
    p         = p,
    d_z       = d_z
  )
}

# 实验组（Group == 1）
paired1_g1 <- map_df(measures1, ~ run_paired1(imp1, group_value = 1, measure1 = .x))

# 对照组（Group == 0）
paired1_g0 <- map_df(measures1, ~ run_paired1(imp1, group_value = 0, measure1 = .x))

# 查看当前结果 paired1 <- bind_rows(paired1_g1, paired1_g0)
# paired1 


### SRS子分类的配对样本t检验

# 基于SRS子量表单独做多重插补
dat_srs_for_imputation <- mydata[, c("Group","WPPSI", var_srs)]
imp2 <- mice(dat_srs_for_imputation, m = 20, seed = 2025)

# 取第5个数据集检查一下
# dat5_srs <- complete(imp2, 5)

# 配对t检验
measures2 <- c(
  "SocialAwareness",
  "SocialCognition",
  "SocialCommunication",
  "SocialMotivation",
  "RRB"
)

# 
run_paired2 <- function(imp2, group_value, measure2) {
  # 从第一个插补数据集确认列名
  comp_srs_1 <- complete(imp2, 1)
  pre_name_srs  <- paste0(measure2, "_Pre")
  post_name_srs <- paste0(measure2, "_Post")
  # 该组样本量
  n_group_srs <- sum(comp_srs_1$Group == group_value, na.rm = TRUE)
  
  # 在每个插补数据集中构造配对t检验
  fit2 <- with(
    imp2,
    {
      pre_srs  <- get(pre_name_srs)
      post_srs <- get(post_name_srs)
      diff_srs <- post_srs - pre_srs
      lm(diff_srs ~ 1, subset = Group == group_value)
    }
  )
  
  # Rubin 规则 pool
  pooled_srs <- pool(fit2)
  s_srs <- summary(pooled_srs, conf.int = TRUE)
  
  est_srs <- s_srs$estimate[1]      # 平均差值
  t_srs   <- s_srs$statistic[1]
  p_srs   <- s_srs$p.value[1]
  
  # 配对效应量 d_z = t / sqrt(n)
  d_z_srs <- as.numeric(t_srs) / sqrt(n_group_srs)
  
  tibble(
    measure_srs = measure2,      # 量表名（子量表）
    group_srs   = group_value,   # 组别（0/1）
    n_srs       = n_group_srs,   # 样本量
    mean_diff_srs = est_srs,     # 平均差值
    t_srs       = t_srs,
    p_srs       = p_srs,
    d_z_srs     = d_z_srs
  )
}

# 实验组（Group == 1）
paired2_g1 <- map_df(
  measures2,
  ~ run_paired2(imp2, group_value = 1, measure2 = .x)
)

# 对照组（Group == 0）
paired2_g0 <- map_df(
  measures2,
  ~ run_paired2(imp2, group_value = 0, measure2 = .x)
)

# 查看结果 paired2 <- bind_rows(paired2_g1, paired2_g0)
# paired2


## 原始数据mean SD,用于制表

get_desc_raw <- function(data, group_value, measure) {
  pre_name  <- paste0(measure, "_Pre")
  post_name <- paste0(measure, "_Post")
  
  dat_g <- data %>% dplyr::filter(Group == group_value)
  diff  <- dat_g[[post_name]] - dat_g[[pre_name]]
  
  tibble(
    baseline_mean = mean(dat_g[[pre_name]],  na.rm = TRUE),
    baseline_sd   = sd(  dat_g[[pre_name]],  na.rm = TRUE),
    endpoint_mean = mean(dat_g[[post_name]], na.rm = TRUE),
    endpoint_sd   = sd(  dat_g[[post_name]], na.rm = TRUE),
    change_mean   = mean(diff,              na.rm = TRUE),
    change_sd     = sd(  diff,              na.rm = TRUE)
  )
}

## measures1: PPVT / EVT / SRS / JA / SI / JE

desc1 <- map_dfr(measures1, function(m) {
  # Intervention = 1
  d1 <- get_desc_raw(mydata, 1, m) %>%
    mutate(
      measure = m,
      Group   = "Intervention"
    )
  # Control = 0
  d0 <- get_desc_raw(mydata, 0, m) %>%
    mutate(
      measure = m,
      Group   = "Control"
    )
  bind_rows(d1, d0)
}) %>%
  transmute(
    measure,
    Group,
    baseline_mean,
    baseline_sd,
    endpoint_mean,
    endpoint_sd,
    change_mean,
    change_sd
  )

## measures2:,SRS 子量表数据

desc2 <- map_dfr(measures2, function(m) {
  d1 <- get_desc_raw(mydata, 1, m) %>%
    mutate(
      measure = m,
      Group   = "Intervention"
    )
  d0 <- get_desc_raw(mydata, 0, m) %>%
    mutate(
      measure = m,
      Group   = "Control"
    )
  bind_rows(d1, d0)
}) %>%
  transmute(
    measure,
    Group,
    baseline_mean,
    baseline_sd,
    endpoint_mean,
    endpoint_sd,
    change_mean,
    change_sd
  )

## 整理pool后的t  p  d

paired1 <- bind_rows(paired1_g1, paired1_g0) %>%
  mutate(
    Group = if_else(group == 1, "Intervention", "Control")
  ) %>%
  transmute(
    measure,
    Group,
    t = t,
    p = p,
    d = d_z
  )

paired2 <- bind_rows(paired2_g1, paired2_g0) %>%
  mutate(
    Group = if_else(group_srs == 1, "Intervention", "Control"),
    measure = measure_srs
  ) %>%
  transmute(
    measure,
    Group,
    t = t_srs,
    p = p_srs,
    d = d_z_srs
  )


## 合并描述性统计、检验结果

res1 <- desc1 %>%
  left_join(paired1, by = c("measure", "Group"))

res2 <- desc2 %>%
  left_join(paired2, by = c("measure", "Group"))

## 变量名 -> Outcome 显示名
label_map <- tribble(
  ~measure,           ~Outcome,
  "JA",               "Joint Attention",
  "SI",               "Social Initiating",
  "JE",               "Joint Engagement",
  "PPVT",             "PPVT",
  "EVT",              "EVT",
  "SRS",              "SRS",
  "SocialAwareness",  "Social Awareness",
  "SocialCognition",  "Social Cognition",
  "SocialCommunication", "Social Communication",
  "SocialMotivation", "Social Motivation",
  "RRB",              "RRB"
)

res_all <- bind_rows(res1, res2) %>%
  left_join(label_map, by = "measure") %>%
  mutate(
    Outcome = factor(
      Outcome,
      levels = c(
        "Joint Attention",
        "Social Initiating",
        "Joint Engagement",
        "PPVT",
        "EVT",
        "SRS",
        "Social Awareness",
        "Social Cognition",
        "Social Communication",
        "Social Motivation",
        "RRB"
      )
    ),
    Group = factor(Group, levels = c("Intervention", "Control"))
  ) %>%
  arrange(Outcome, Group)

## 同一 Outcome 下，只在第一行显示 Outcome 名
res_all <- res_all %>%
  group_by(Outcome) %>%
  mutate(
    Outcome_display = if_else(row_number() == 1,
                              as.character(Outcome), "")
  ) %>%
  ungroup()


## 格式化数值 显著性星号


table_data <- res_all %>%
  mutate(
    Baseline = sprintf("%.2f (%.2f)", baseline_mean, baseline_sd),
    Endpoint = sprintf("%.2f (%.2f)", endpoint_mean, endpoint_sd),
    Change   = sprintf("%.2f (%.2f)", change_mean,   change_sd),
    t_fmt    = sprintf("%.2f", t),
    p_fmt    = sprintf("%.3f", p),
    d_fmt    = sprintf("%.2f", d),
    sig = case_when(
      p < 0.001 ~ "***",
      p < 0.01  ~ "**",
      p < 0.05  ~ "*",
      TRUE      ~ ""
    ),
    p_disp = paste0(p_fmt, sig)
  ) %>%
  select(
    Outcome  = Outcome_display,
    Group,
    Baseline,
    Endpoint,
    Change,
    t = t_fmt,
    p = p_disp,
    d = d_fmt
  )

## gt 画表

gt_tbl <- table_data %>%
  gt() %>%
  cols_label(
    Outcome  = "Outcome",
    Group    = "",
    Baseline = "Baseline Mean (SD)",
    Endpoint = "Endpoint Mean (SD)",
    Change   = "Change from baseline Mean (SD)",
    t        = "t",
    p        = "p",
    d        = "d"
  ) %>%
  cols_align(
    align = "left",
    columns = c(Outcome, Group)
  ) %>%
  cols_align(
    align = "center",
    columns = c(Baseline, Endpoint, Change, t, p, d)
  ) %>%
  tab_options(
    column_labels.border.top.style    = "solid",
    column_labels.border.top.color    = "black",
    column_labels.border.top.width    = px(2),
    
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2),
    
    table_body.border.bottom.style = "solid",
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = px(2),
    
    table_body.hlines.style = "none",     
    table_body.vlines.style = "none",     
    table.border.left.style   = "none",
    table.border.right.style  = "none",
    table.border.top.style    = "none"    
  )

gt_tbl


df_tbl <- as.data.frame(gt_tbl)
ft_tbl <- flextable(df_tbl) %>%
  autofit() %>%
  theme_booktabs()
doc <- read_docx() %>%
  body_add_flextable(ft_tbl)
print(
  doc,
  target = "C:/Users/J.N.RAN/Desktop/ASD图表/gt_tbl.docx"
)






########### Between-Group Differences: ANCOVA ###################

# imp1里 PPVT EVT SRS JA SI JE的ANCOVA

## 
label_map <- c(
  PPVT = "PPVT",
  EVT  = "EVT",
  SRS  = "SRS",
  JA   = "Joint Attention",
  SI   = "Social Initiating",
  JE   = "Joint Engagement"
)

## 结果表
res1_ancova <- tibble(
  Measure       = measures1,
  Outcome       = unname(label_map[measures1]),
  F_value       = NA_real_,
  P_value       = NA_real_,
  Partial_Eta2  = NA_real_,
  Df            = NA_real_
)

m_imp <- imp1$m  # 插补次数


## 对每个插补集做 ANCOVA + Rubin pool + 偏η²
for (i in seq_along(measures1)) {
  
  m    <- measures1[i]
  pre  <- paste0(m, "_Pre")
  post <- paste0(m, "_Post")
  
  ## 存每个插补数据集的 β 和 Var(β)
  beta_vec <- numeric(m_imp)
  var_vec  <- numeric(m_imp)
  
  ## 存每个插补数据集的 partial η²
  eta_vec  <- numeric(m_imp)
  df_res_vec <- numeric(m_imp) 
  
  ## 建公式字符串（用于 lm）
  f_str <- paste(post, "~ WPPSI +", pre, "+ Group")
  form  <- as.formula(f_str)
  
  for (k in 1:m_imp) {
    
    dat_k <- complete(imp1, k)
    
    ## ANCOVA
    fit_k <- lm(form, data = dat_k)
    sum_k <- summary(fit_k)
    coef_k <- coef(sum_k)
    df_res_vec[k] <- df.residual(fit_k)
    # Group 的行
    row_g <- grep("^Group", rownames(coef_k))[1]
    if (is.na(row_g)) {
      stop("在第 ", k, " 个插补集中，找不到 Group 的回归系数行。请检查 Group 是否为因子。")
    }
    
    beta_k <- coef_k[row_g, "Estimate"]
    se_k   <- coef_k[row_g, "Std. Error"]
    
    beta_vec[k] <- beta_k
    var_vec[k]  <- se_k^2
    
    ## Type III Anova 计算 partial η²
    aov_k <- car::Anova(fit_k, type = 3)
    
    row_aov_g <- which(rownames(aov_k) == "Group")
    ss_g   <- aov_k[row_aov_g,        "Sum Sq"]
    ss_res <- aov_k["Residuals",     "Sum Sq"]
    
    eta_vec[k] <- ss_g / (ss_g + ss_res)
  }
  
  ## Rubin 规则合并 Group 效应
  Q_bar <- mean(beta_vec)
  U_bar <- mean(var_vec)
  B     <- var(beta_vec)
  
  T_var <- U_bar + (1 + 1/m_imp) * B
  
  t_mi <- Q_bar / sqrt(T_var)
  df_mi <- (m_imp - 1) * (1 + U_bar / ((1 + 1/m_imp) * B))^2
  
  p_mi <- 2 * pt(-abs(t_mi), df = df_mi)
  F_mi <- t_mi^2
  
  ## 偏η²取各插补数据集平均
  partial_eta2 <- mean(eta_vec, na.rm = TRUE)
  
  ## 写入结果
  res1_ancova$F_value[i]      <- F_mi
  res1_ancova$P_value[i]      <- p_mi
  res1_ancova$Partial_Eta2[i] <- partial_eta2
  df_res_bar <- mean(df_res_vec, na.rm = TRUE)
  res1_ancova$Df[i] <- df_res_bar
}


## 查看 ANCOVA 结果（注意，JE的建模不可用，见前提假设检验）
res1_ancova





###
### 由于JE与pre可能存在交互，对JE进行单独的JE_Post ~ JE_Pre * Group + WPPSI建模，
###


## 在每个插补集中跑 JE 的交互 ANCOVA
fit_JE_mi <- with(
  imp1,
  lm(JE_Post ~ JE_Pre * Group + WPPSI)
)

## Rubin 规则合并
pool_JE <- pool(fit_JE_mi)
sum_JE  <- summary(pool_JE, conf.int = TRUE)

sum_JE

# 模型整体 F 检验
D1(fit_JE_mi)

# 每个插补模型的 R²
R2_list <- sapply(fit_JE_mi$analyses, function(mod) summary(mod)$r.squared)

# 每个插补模型的 adj.R²
adjR2_list <- sapply(fit_JE_mi$analyses, function(mod) summary(mod)$adj.r.squared)

# 取平均作为 pooled
R2_pool    <- mean(R2_list)
adjR2_pool <- mean(adjR2_list)

R2_pool
adjR2_pool





## 取代表数据集进行简单斜率分析

fit_JE_5 <- lm(JE_Post ~ JE_Pre * Group + WPPSI, data = dat5_imp1)

# simple slopes 简单斜率分析
sim_slopes(fit_JE_5, pred = JE_Pre, modx = Group)

# 图
interact_plot(fit_JE_5, pred = JE_Pre, modx = Group)

# 简单斜率分析，JE初始为高中低时的比较
emm_je_levels <- emmeans(
  fit_JE_5, 
  ~ Group | JE_Pre,
  at = list(JE_Pre = quantile(dat5_imp1$JE_Pre, probs = c(.16, .50, .84)))
)

emm_je_levels
contrast(emm_je_levels, method = "pairwise")



## SRS子量表 ANCOVA（imp2）


# 显示用名字
label_map_srs <- c(
  SocialAwareness   = "Social Awareness",
  SocialCognition   = "Social Cognition",
  SocialCommunication = "Social Communication",
  SocialMotivation  = "Social Motivation",
  RRB               = "RRB"
)

# 结果表
res2_ancova <- tibble(
  Measure       = measures2,
  Outcome       = unname(label_map_srs[measures2]),
  F_value       = NA_real_,
  P_value       = NA_real_,
  Partial_Eta2  = NA_real_,
  Df            = NA_real_
)

m_imp2 <- imp2$m  # imp2 的插补次数

## 对每个 SRS 子量表做 ANCOVA + Rubin pool + 偏η²
for (i in seq_along(measures2)) {
  
  m    <- measures2[i]
  pre  <- paste0(m, "_Pre")
  post <- paste0(m, "_Post")
  
  # 存每个插补数据集的 β 和 Var(β)
  beta_vec <- numeric(m_imp2)
  var_vec  <- numeric(m_imp2)
  
  # 存每个插补数据集的 partial η²
  eta_vec     <- numeric(m_imp2)
  # 存每个插补集的“残差 df”
  df_res_vec  <- numeric(m_imp2)
  
  # 构造公式：Post ~ WPPSI + Pre + Group
  f_str <- paste(post, "~ WPPSI +", pre, "+ Group")
  form  <- as.formula(f_str)
  
  for (k in 1:m_imp2) {
    
    dat_k <- complete(imp2, k)
    
    # 从 mydata 补回 WPPSI
    dat_k$WPPSI <- mydata$WPPSI
    
    ## ANCOVA
    fit_k  <- lm(form, data = dat_k)
    sum_k  <- summary(fit_k)
    coef_k <- coef(sum_k)
    
    df_res_vec[k] <- df.residual(fit_k)
    # Group 的行
    row_g <- grep("^Group", rownames(coef_k))[1]
    if (is.na(row_g)) {
      stop("在第 ", k, " 个插补集中，找不到 Group 的回归系数行。请检查 Group 是否为因子。")
    }
    
    beta_k <- coef_k[row_g, "Estimate"]
    se_k   <- coef_k[row_g, "Std. Error"]
    
    beta_vec[k] <- beta_k
    var_vec[k]  <- se_k^2
    
    ## Type III Anova 计算 partial η²
    aov_k <- car::Anova(fit_k, type = 3)
    
    row_aov_g <- which(rownames(aov_k) == "Group")
    ss_g   <- aov_k[row_aov_g,        "Sum Sq"]
    ss_res <- aov_k["Residuals",     "Sum Sq"]
    
    eta_vec[k] <- ss_g / (ss_g + ss_res)
  }
  
  ## Rubin 规则合并 Group 效应
  Q_bar <- mean(beta_vec)
  U_bar <- mean(var_vec)
  B     <- var(beta_vec)
  
  T_var <- U_bar + (1 + 1/m_imp2) * B
  
  t_mi  <- Q_bar / sqrt(T_var)
  df_mi <- (m_imp2 - 1) * (1 + U_bar / ((1 + 1/m_imp2) * B))^2
  
  p_mi <- 2 * pt(-abs(t_mi), df = df_mi)
  F_mi <- t_mi^2
  
  ## 偏η²：各插补数据集平均
  partial_eta2 <- mean(eta_vec, na.rm = TRUE)
  ## 🔹 报告用的 df（分母 df）：使用各插补残差 df 的平均值
  df_res_bar   <- mean(df_res_vec, na.rm = TRUE)
  
  ## 写入结果
  res2_ancova$F_value[i]      <- F_mi
  res2_ancova$P_value[i]      <- p_mi
  res2_ancova$Partial_Eta2[i] <- partial_eta2
  res2_ancova$Df[i]           <- df_res_bar   
}

## 查看SRS子量表 ANCOVA 
res2_ancova


###
### 由于social motivation与pre可能存在交互，尝试对其进行单独的交互建模
###


## 在每个插补集中跑的交互 ANCOVA
fit_SM_mi <- with(
  imp2,
  lm(SocialMotivation_Post ~ SocialMotivation_Pre * Group + WPPSI)
)

## Rubin 规则合并
pool_SM <- pool(fit_SM_mi)
sum_SM  <- summary(pool_SM, conf.int = TRUE)

sum_SM

#
fit_SM_5 <- lm(SocialMotivation_Post ~ SocialMotivation_Pre * Group + WPPSI, data = dat5_imp2)
summary(fit_SM_5)
# simple slopes 简单斜率分析
sim_slopes(fit_SM_5, pred = SocialMotivation_Pre, modx = Group)

# 图
interact_plot(fit_SM_5, pred = SocialMotivation_Pre, modx = Group)

pre_mean <- mean(dat5_imp2$SocialMotivation_Pre, na.rm = TRUE)

# 简单斜率分析，SM初始为高中低时的比较
emm_pre_levels <- emmeans(fit_SM_5, ~ Group | SocialMotivation_Pre,
                          at = list(SocialMotivation_Pre = 
                                      quantile(dat5_imp2$SocialMotivation_Pre, 
                                               probs = c(.16, .50, .84))))

emm_pre_levels
contrast(emm_pre_levels, method = "pairwise")



### 前提假设检验

# 线性关系检查
dat5_imp1 <- complete(imp1, 5) #取第5插补集为代表数据集


measures1 <- c("PPVT", "EVT", "SRS", "JA", "SI", "JE")
ancova_models <- list()

for (m in measures1) {
  
  pre_var  <- paste0(m, "_Pre")
  post_var <- paste0(m, "_Post")
  
  f_str <- paste0(post_var, " ~ ", pre_var, " + WPPSI + Group")
  form  <- as.formula(f_str)
  
  model <- lm(form, data = dat5_imp1)
  
  ancova_models[[m]] <- model
  
  cat("\n===================\n")
  cat("ANCOVA model for:", m, "\n")
  print(summary(model))
}


for (m in measures1) {
  cat("\n===================\n")
  cat("Linearity check for:", m, "\n")
  
  model <- ancova_models[[m]]
  
  # partial residual plots
  car::crPlots(model)

}


# 回归斜率同质性检查


## 结果表
res_slope <- tibble(
  Measure          = measures1,
  Pre_Group_F      = NA_real_,
  Pre_Group_p      = NA_real_,
  WPPSI_Group_F    = NA_real_,
  WPPSI_Group_p    = NA_real_
)

for (i in seq_along(measures1)) {
  
  m <- measures1[i]
  pre_var  <- paste0(m, "_Pre")
  post_var <- paste0(m, "_Post")
  
  form_slope <- as.formula(
    paste0(post_var, " ~ ", pre_var, "*Group + WPPSI*Group")
  )
  
  fit_slope  <- lm(form_slope, data = dat5_imp1)
  aov_slope  <- car::Anova(fit_slope, type = 3)
  rn         <- rownames(aov_slope)
  
  idx_pre_group <- grep(
    pattern = paste0("(^", pre_var, ":Group$)|(^Group:", pre_var, "$)"),
    x       = rn
  )
  
  if (length(idx_pre_group) == 1) {
    res_slope$Pre_Group_F[i] <- aov_slope[idx_pre_group, "F value"]
    res_slope$Pre_Group_p[i] <- aov_slope[idx_pre_group, "Pr(>F)"]
  }
  
  idx_wppsi_group <- grep(
    pattern = "(^WPPSI:Group$)|(^Group:WPPSI$)",
    x       = rn
  )
  
  if (length(idx_wppsi_group) == 1) {
    res_slope$WPPSI_Group_F[i] <- aov_slope[idx_wppsi_group, "F value"]
    res_slope$WPPSI_Group_p[i] <- aov_slope[idx_wppsi_group, "Pr(>F)"]
  }
}

res_slope



# 残差正态性

measures1 <- c("PPVT", "EVT", "SRS", "JA", "SI", "JE")
ancova_models <- list()

for (m in measures1) {
  
  pre_var  <- paste0(m, "_Pre")
  post_var <- paste0(m, "_Post")
  
  f_str <- paste0(post_var, " ~ ", pre_var, " + WPPSI + Group")
  form  <- as.formula(f_str)
  
  model <- lm(form, data = dat5_imp1)
  ancova_models[[m]] <- model
  
  cat("\n===================\n")
  cat("ANCOVA model for:", m, "\n")
}


for (m in measures1) {
  
  cat("\n===================\n")
  cat("Residual Normality Check for:", m, "\n")
  
  model <- ancova_models[[m]]
  res   <- residuals(model)
  
  ## (1) QQ plot —— 正态性视觉检查
  ## 标准：点应大致落在红线附近；偏差越大，正态性越差
  qqnorm(res, main = paste("QQ Plot for", m))
  qqline(res, col = "red", lwd = 2)
  
  ## (2) Shapiro-Wilk Test —— 正态性统计检验
  ## 标准：p > .05 表示“没有显著偏离正态性”
  shapiro_result <- shapiro.test(res)
  print(shapiro_result)
}



## SRS子量表前提假设检验
dat5_imp2 <- complete(imp2, 5)

ancova_models_imp2 <- list()

for (m in measures2) {
  
  pre_var  <- paste0(m, "_Pre")
  post_var <- paste0(m, "_Post")
  
  f_str <- paste0(post_var, " ~ ", pre_var, " + WPPSI + Group")
  form  <- as.formula(f_str)
  
  model <- lm(form, data = dat5_imp2)
  ancova_models_imp2[[m]] <- model
  
  cat("\n=============================\n")
  cat("ANCOVA model for:", m, "\n")
  print(summary(model))
}


# 线性关系
for (m in measures2) {
  cat("\n=============================\n")
  cat("Linearity check for:", m, "\n")
  
  model <- ancova_models_imp2[[m]]
  car::crPlots(model)   
}

# 回归斜率同质性
res_slope_imp2 <- tibble(
  Measure          = measures2,
  Pre_Group_F      = NA_real_,
  Pre_Group_p      = NA_real_,
  WPPSI_Group_F    = NA_real_,
  WPPSI_Group_p    = NA_real_
)

for (i in seq_along(measures2)) {
  
  m <- measures2[i]
  pre_var  <- paste0(m, "_Pre")
  post_var <- paste0(m, "_Post")
  
  form_slope <- as.formula(
    paste0(post_var, " ~ ", pre_var, "*Group + WPPSI*Group")
  )
  
  fit_slope <- lm(form_slope, data = dat5_imp2)
  aov_slope <- car::Anova(fit_slope, type = 3)
  rn        <- rownames(aov_slope)
  
  ## ---- Pre × Group
  idx_pre_group <- grep(
    pattern = paste0("(^", pre_var, ":Group$)|(^Group:", pre_var, "$)"),
    x       = rn
  )
  
  if (length(idx_pre_group) == 1) {
    res_slope_imp2$Pre_Group_F[i] <- aov_slope[idx_pre_group, "F value"]
    res_slope_imp2$Pre_Group_p[i] <- aov_slope[idx_pre_group, "Pr(>F)"]
  }
  
  ## ---- WPPSI × Group
  idx_wppsi_group <- grep(
    pattern = "(^WPPSI:Group$)|(^Group:WPPSI$)",
    x       = rn
  )
  
  if (length(idx_wppsi_group) == 1) {
    res_slope_imp2$WPPSI_Group_F[i] <- aov_slope[idx_wppsi_group, "F value"]
    res_slope_imp2$WPPSI_Group_p[i] <- aov_slope[idx_wppsi_group, "Pr(>F)"]
  }
}

res_slope_imp2


# 残差正态性
for (m in measures2) {
  
  cat("\n=============================\n")
  cat("Residual Normality Check for:", m, "\n")
  
  model <- ancova_models_imp2[[m]]
  res   <- residuals(model)
  
  ## QQ plot
  qqnorm(res, main = paste("QQ Plot for", m))
  qqline(res, col = "red", lwd = 2)
  
  ## Shapiro-Wilk
  shapiro_result <- shapiro.test(res)
  print(shapiro_result)   # p > .05 → 正态性可接受
}






## 合并 ANCOVA 结果
ancova_all <- bind_rows(
  res1_ancova %>% select(Outcome, F_value, P_value, Partial_Eta2, Df),
  res2_ancova %>% select(Outcome, F_value, P_value, Partial_Eta2, Df)
)

## 设定 Outcome 的顺序，并排序
ancova_all <- ancova_all %>%
  mutate(
    Outcome = factor(
      Outcome,
      levels = c(
        "Joint Attention",
        "Social Initiating",
        "Joint Engagement",
        "PPVT",
        "EVT",
        "SRS",
        "Social Awareness",
        "Social Cognition",
        "Social Communication",
        "Social Motivation",
        "RRB"
      )
    )
  ) %>%
  arrange(Outcome)

## 在这里单独覆盖 Joint Engagement,用线性回归模型的参数而非ANCOVA
ancova_all <- ancova_all %>%
  mutate(
    # JE 行改成回归模型交互项结果
    F_value = if_else(
      Outcome == "Joint Engagement",
      8.47,        
      F_value
    ),
    P_value = if_else(
      Outcome == "Joint Engagement",
      0.005, #  p 值
      P_value
    ),
    # JE 不给 Partial Eta2
    Partial_Eta2 = if_else(
      Outcome == "Joint Engagement",
      NA_real_,     # 
      Partial_Eta2
    ),
    # df1
    Df1 = if_else(
      Outcome == "Joint Engagement",
      4,
      1
    ),
    # df2
    Df2 = if_else(
      Outcome == "Joint Engagement",
      51.75,
      Df
    )
  )

## 加显著性星号 + 格式化
table_data_ancova <- ancova_all %>%
  mutate(
    F_fmt   = sprintf("%.2f", F_value),
    # df 显示成 “df1, df2”
    df_fmt  = paste0(Df1, ", ", sprintf("%.2f", Df2)),
    p_fmt   = sprintf("%.3f", P_value),
    # η² 对 JE 行留空
    eta_fmt = if_else(
      is.na(Partial_Eta2),
      "",
      sprintf("%.3f", Partial_Eta2)
    ),
    sig = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01  ~ "**",
      P_value < 0.05  ~ "*",
      TRUE            ~ ""
    ),
    p_disp = paste0(p_fmt, sig)
  ) %>%
  select(
    Outcome,
    F   = F_fmt,
    df  = df_fmt,
    p   = p_disp,
    Partial_eta2 = eta_fmt
  )

## 用 gt 制表
gt_tbl_ancova <- table_data_ancova %>%
  gt() %>%
  cols_label(
    Outcome      = "Outcome",
    F            = "F",
    df           = "df",
    p            = "p",
    Partial_eta2 = "Partial \u03b7\u00b2"  # Partial η²
  ) %>%
  cols_align(
    align = "left",
    columns = c(Outcome)
  ) %>%
  cols_align(
    align = "center",
    columns = c(F, df, p, Partial_eta2)
  ) %>%
  tab_options(
    column_labels.border.top.style    = "solid",
    column_labels.border.top.color    = "black",
    column_labels.border.top.width    = px(2),
    
    column_labels.border.bottom.style = "solid",
    column_labels.border.bottom.color = "black",
    column_labels.border.bottom.width = px(2),
    
    table_body.border.bottom.style = "solid",
    table_body.border.bottom.color = "black",
    table_body.border.bottom.width = px(2),
    
    table_body.hlines.style = "none",     
    table_body.vlines.style = "none",     
    table.border.left.style   = "none",
    table.border.right.style  = "none",
    table.border.top.style    = "none"    
  )

gt_tbl_ancova








########### Regression Analyses ###################
## 思路为：数据分析证明干预组有干预效果，
## 所以根据研究假设，选取实验组为分析样本，
## 尝试检验社交动机指标能否预测前后测其他指标的变化量。
## 所以我们先取一个插补集作为对象，进行试点分析，
## 以pre为控制变量，用逐步回归的方式尝试加入JE JA SI作为预测变量
## 然后综合考虑系数显著性 AIC BIC R方，确认最佳模型
## 最后在所有插补集上跑最优模型，pool合并，确认最终的模型参数

## 在 imp1 里取第 5 个插补集
dat5 <- complete(imp1, 5)

## 提取实验组
dat5_g1 <- dat5[dat5$Group == 1, ]


## 逐步回归
run_step_change <- function(outcome, data) {
  pre_var  <- paste0(outcome, "_Pre")
  post_var <- paste0(outcome, "_Post")
  
  # 
  df <- data %>%
    mutate(
      dY   = .data[[post_var]] - .data[[pre_var]],
      dJA  = JA_Post - JA_Pre,
      dSI  = SI_Post - SI_Pre,
      dJE  = JE_Post - JE_Pre
    )
  
  ## 基础模型：只有 ΔY（截距模型）
  base_form <- dY ~ 1
  
  ## 完整模型：ΔY ~ ΔJA + ΔSI + ΔJE
  full_form <- dY ~ dJA + dSI + dJE
  
  base_mod <- lm(base_form, data = df)
  
  ## stepwise
  step_mod <- step(
    base_mod,
    scope     = list(lower = base_form, upper = full_form),
    direction = "both",
    trace     = TRUE  
  )
  
  # stepwise 路径（每一步的 AIC/增删变量）
  path_anova <- step_mod$anova
  
  list(
    final_model = step_mod,
    step_path   = path_anova
  )
}


## 对 ΔPPVT / ΔEVT / ΔSRS 分别跑逐步回归
res_PPVT_change <- run_step_change("PPVT", dat5_g1)
res_EVT_change  <- run_step_change("EVT",  dat5_g1)
res_SRS_change  <- run_step_change("SRS",  dat5_g1)

## 查看结果

# 最终模型的系数与显著性
summary(res_PPVT_change$final_model)
summary(res_EVT_change$final_model)
summary(res_SRS_change$final_model)

# step 过程每一步模型的 AIC / 增删哪个变量
res_PPVT_change$step_path
res_EVT_change$step_path
res_SRS_change$step_path

### 以下仅关注EVT的模型，进行前提假设检验
model <- res_EVT_change$final_model   
resid <- residuals(model)     #残差 


## 检查线性关系
## - 看 Residuals vs Fitted 图
## - 标准：图中点应随机散布，无弧形或系统性模式
plot(model, which = 1)
# 若曲线是平坦随机云状 → 线性假设成立


## 残差的正态性、方差齐性、线性关系、零均值
## - 使用 QQ 图 + Shapiro-Wilk 检验，检验正态性
## - 标准：
##     QQ 图：点接近对角线
##     Shapiro：p > .05
plot(model, which = 2)
y.rst<-rstandard(model)
y.fit<-predict(model)
plot(y.fit~y.rst)
shapiro.test(resid)
# p > .05 → 残差基本服从正态分布



## 3. 检查同方差性（异方差检验）

##     BP Test：p > .05 
bptest(model)
# p > .05 → 则满足同方差性


## 检查残差独立性
## - 使用 Durbin–Watson test
## - 标准：
##     DW ≈ 2 → 残差独立
##     DW < 1.5 或 > 2.5 → 可能存在序列相关
dwtest(model)
# 期望 DW 值接近 2


## 检查多重共线性
## - 使用 VIF（variance inflation factor）
## - 标准：
##     VIF < 5 → 完全可接受
##     VIF < 10 → 勉强接受
##     VIF ≥ 10 → 严重共线性，需处理
vif(model)
# 由于这里模型只含一个预测变量，无需检验


## 强影响点分析
influence.measures(model)



### 由于模型bptest显著，说明模型不满足同方差性，使用异方差稳健标准误修正模型
coeftest(model, vcov = vcovHC(model, type = "HC3"))

linearHypothesis(model, "dSI = 0", vcov = vcovHC(model, type = "HC3")) #修正后模型显著性


### 在所有数据集层面进行线性回归模型计算

## ΔSRS ~ 仅截距

fit_dSRS <- with(
  imp1,
  {
    dSRS <- SRS_Post - SRS_Pre
    lm(dSRS ~ 1, subset = Group == "1")
  }
)

pool_dSRS <- pool(fit_dSRS)
summary_dSRS <- summary(pool_dSRS, conf.int = TRUE)
summary_dSRS


## ΔPPVT ~ 仅截距

fit_dPPVT <- with(
  imp1,
  {
    dPPVT <- PPVT_Post - PPVT_Pre
    lm(dPPVT ~ 1, subset = Group == "1")
  }
)

pool_dPPVT <- pool(fit_dPPVT)
summary_dPPVT <- summary(pool_dPPVT, conf.int = TRUE)
summary_dPPVT


## ΔEVT ~ ΔSI


fit_dEVT_dSI_robust <- with(
  imp1,
  {
    dEVT <- EVT_Post - EVT_Pre
    dSI  <- SI_Post  - SI_Pre
    
    dat_g1 <- data.frame(dEVT, dSI, Group)
    dat_g1 <- subset(dat_g1, Group == 1 | Group == "1")
    
    mod <- lm(dEVT ~ dSI, data = dat_g1)
    
    # 在每个插补集上，用 HC3 生成稳健系数表
    lmtest::coeftest(mod, vcov. = sandwich::vcovHC(mod, type = "HC3"))
  }
)

## pool
pool_dEVT_dSI_robust <- pool(fit_dEVT_dSI_robust)


summary(pool_dEVT_dSI_robust, conf.int = TRUE)


#只含一个因子，模型的F等于t方
# 汇总结果
sum_robust <- summary(pool_dEVT_dSI_robust, conf.int = TRUE)

# 抽取 dSI）
row_dSI <- sum_robust[sum_robust$term == "dSI", ]

t_dSI  <- row_dSI$statistic   # t 值
df_dSI <- row_dSI$df          # df
p_dSI  <- row_dSI$p.value     # p 值）

# 对应的整体模型 F 检验（1个预测量 → F = t^2）
F_model <- t_dSI^2

F_model
df_dSI
p_dSI
## 这里得到的就是模型显著性

## 得到r方
fit_dEVT_dSI_lm <- with(
  imp1,
  {
    dEVT <- EVT_Post - EVT_Pre
    dSI  <- SI_Post  - SI_Pre
    
    dat_g1 <- data.frame(dEVT, dSI, Group)
    dat_g1 <- subset(dat_g1, Group == 1 | Group == "1")
    
    lm(dEVT ~ dSI, data = dat_g1)
  }
)

# 提取每个插补集的 R² 和 adjusted R²
R2_vals    <- sapply(fit_dEVT_dSI_lm$analyses, function(mod) summary(mod)$r.squared)
adjR2_vals <- sapply(fit_dEVT_dSI_lm$analyses, function(mod) summary(mod)$adj.r.squared)

# pool
R2_pool    <- mean(R2_vals)
adjR2_pool <- mean(adjR2_vals)

R2_pool
adjR2_pool








######### 折线图

#
plot_dat <- mydata %>%
  select(Group, PPVT_Pre, PPVT_Post) %>%
  gather(key = "Time", value = "Value", PPVT_Pre, PPVT_Post) %>%
  mutate(
    Time  = ifelse(Time == "PPVT_Pre", "Pre", "Post"),
    Time  = factor(Time, levels = c("Pre","Post")),
    Group = factor(Group, levels = c(1,0),
                   labels = c("Intervention","Control"))
  ) %>%
  group_by(Group, Time) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SE   = sd(Value, na.rm = TRUE) / sqrt(sum(!is.na(Value))),
    .groups = "drop"
  )

plot_ppvt <- ggplot(plot_dat,
                    aes(Time, Mean,
                        group = Group,
                        linetype = Group,
                        shape = Group)) +
  geom_line(size = 1.1) +
  geom_point(size = 2.4) +
  geom_errorbar(aes(ymin = Mean - 1.96*SE,
                    ymax = Mean + 1.96*SE),
                width = 0.06, linewidth = 0.6) +
  scale_linetype_manual(values = c("Intervention"="solid",
                                   "Control"="71")) +
  scale_shape_manual(values = c(16,16)) +
  labs(y = "PPVT", x = NULL) +
  theme_classic(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 16, color = "black"),
    axis.text.y  = element_text(size = 16, color = "black"),
    axis.line    = element_line(size = 0.6),
    axis.ticks   = element_line(size = 0.6),
    legend.position = c(0.88, 0.80),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 13),
    legend.title = element_blank(),
    plot.margin = margin(t = 10, r = 70, b = 10, l = 10),
    legend.key.width = unit(1.1, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(linetype = c("solid","44"))
    )
  )

plot_ppvt <- plot_ppvt +
  scale_y_continuous(
    limits = c(60, 95),
    breaks = seq(60, 95, by = 5)
  )

plot_ppvt


#
plot_dat <- mydata %>%
  select(Group, EVT_Pre, EVT_Post) %>%
  gather(key = "Time", value = "Value", EVT_Pre, EVT_Post) %>%
  mutate(
    Time  = ifelse(Time == "EVT_Pre", "Pre", "Post"),
    Time  = factor(Time, levels = c("Pre","Post")),
    Group = factor(Group, levels = c(1,0),
                   labels = c("Intervention","Control"))
  ) %>%
  group_by(Group, Time) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SE   = sd(Value, na.rm = TRUE) / sqrt(sum(!is.na(Value))),
    .groups = "drop"
  )

plot_evt <- ggplot(plot_dat,
                   aes(Time, Mean,
                       group = Group,
                       linetype = Group,
                       shape = Group)) +
  geom_line(size = 1.1) +
  geom_point(size = 2.4) +
  geom_errorbar(aes(ymin = Mean - 1.96*SE,
                    ymax = Mean + 1.96*SE),
                width = 0.06, linewidth = 0.6) +
  scale_linetype_manual(values = c("Intervention"="solid",
                                   "Control"="71")) +
  scale_shape_manual(values = c(16,16)) +
  labs(y = "EVT", x = NULL) +
  theme_classic(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 16, color = "black"),
    axis.text.y  = element_text(size = 16, color = "black"),
    axis.line    = element_line(size = 0.6),
    axis.ticks   = element_line(size = 0.6),
    legend.position = c(0.88, 0.80),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 13),
    legend.title = element_blank(),
    plot.margin = margin(t = 10, r = 70, b = 10, l = 10),
    legend.key.width = unit(1.1, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(linetype = c("solid","44"))
    )
  )

plot_evt


sig_label <- "*"

y_post_top <- plot_dat %>%
  filter(Time == "Post") %>%
  summarise(top = max(Mean + 1.96*SE, na.rm = TRUE)) %>%
  pull(top)

y_bracket <- y_post_top + 1.2

# 
y_limits <- c(60, 95)                 #  y 轴范围
yrange   <- diff(y_limits)
tick     <- yrange * 0.03             # 3% 的 y 轴高度
lift     <- yrange * 0.015            # 星号离括号的距离

plot_evt_sig <- plot_evt +
  scale_y_continuous(
    limits = y_limits,
    breaks = seq(60, 95, by = 5),
    expand = expansion(mult = c(0.02, 0.10))  # 顶部留白
  ) +
  annotate("segment", x = 1.90, xend = 2.10,
           y = y_bracket, yend = y_bracket, linewidth = 0.6) +
  annotate("segment", x = 1.90, xend = 1.90,
           y = y_bracket, yend = y_bracket - tick, linewidth = 0.6) +
  annotate("segment", x = 2.10, xend = 2.10,
           y = y_bracket, yend = y_bracket - tick, linewidth = 0.6) +
  annotate("text", x = 2,
           y = y_bracket + lift, label = sig_label, size = 6)

plot_evt_sig <- plot_evt_sig +
  scale_y_continuous(
    limits = c(60, 95),
    breaks = seq(60, 95, by = 5)
  )


plot_evt_sig




#
plot_dat <- mydata %>%
  select(Group, SRS_Pre, SRS_Post) %>%
  gather(key = "Time", value = "Value", SRS_Pre, SRS_Post) %>%
  mutate(
    Time  = ifelse(Time == "SRS_Pre", "Pre", "Post"),
    Time  = factor(Time, levels = c("Pre","Post")),
    Group = factor(Group, levels = c(1,0),
                   labels = c("Intervention","Control"))
  ) %>%
  group_by(Group, Time) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SE   = sd(Value, na.rm = TRUE) / sqrt(sum(!is.na(Value))),
    .groups = "drop"
  )

plot_srs <- ggplot(plot_dat,
                   aes(Time, Mean,
                       group = Group,
                       linetype = Group,
                       shape = Group)) +
  geom_line(size = 1.1) +
  geom_point(size = 2.4) +
  geom_errorbar(aes(ymin = Mean - 1.96*SE,
                    ymax = Mean + 1.96*SE),
                width = 0.06, linewidth = 0.6) +
  scale_linetype_manual(values = c("Intervention"="solid",
                                   "Control"="71")) +
  scale_shape_manual(values = c(16,16)) +
  labs(y = "SRS", x = NULL) +
  theme_classic(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 16, color = "black"),
    axis.text.y  = element_text(size = 16, color = "black"),
    axis.line    = element_line(size = 0.6),
    axis.ticks   = element_line(size = 0.6),
    legend.position = c(0.88, 0.80),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 13),
    legend.title = element_blank(),
    plot.margin = margin(t = 10, r = 70, b = 10, l = 10),
    legend.key.width = unit(1.1, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(linetype = c("solid","44"))
    )
  )

plot_srs



#
plot_dat <- mydata %>%
  select(Group, SI_Pre, SI_Post) %>%
  gather(key = "Time", value = "Value", SI_Pre, SI_Post) %>%
  mutate(
    Time  = ifelse(Time == "SI_Pre", "Pre", "Post"),
    Time  = factor(Time, levels = c("Pre","Post")),
    Group = factor(Group, levels = c(1,0),
                   labels = c("Intervention","Control"))
  ) %>%
  group_by(Group, Time) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SE   = sd(Value, na.rm = TRUE) / sqrt(sum(!is.na(Value))),
    .groups = "drop"
  )

plot_si <- ggplot(plot_dat,
                  aes(Time, Mean,
                      group = Group,
                      linetype = Group,
                      shape = Group)) +
  geom_line(size = 1.1) +
  geom_point(size = 2.4) +
  geom_errorbar(aes(ymin = Mean - 1.96*SE,
                    ymax = Mean + 1.96*SE),
                width = 0.06, linewidth = 0.6) +
  scale_linetype_manual(values = c("Intervention"="solid",
                                   "Control"="71")) +
  scale_shape_manual(values = c(16,16)) +
  labs(y = "USI", x = NULL) +
  theme_classic(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 16, color = "black"),
    axis.text.y  = element_text(size = 16, color = "black"),
    axis.line    = element_line(size = 0.6),
    axis.ticks   = element_line(size = 0.6),
    legend.position = c(0.88, 0.80),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 13),
    legend.title = element_blank(),
    plot.margin = margin(t = 10, r = 70, b = 10, l = 10),
    legend.key.width = unit(1.1, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(linetype = c("solid","44"))
    )
  )

plot_si

sig_label <- "***"   

# Post 上方基准高度（取 Post 两组CI上界最大值）
y_post_top <- plot_dat %>%
  filter(Time == "Post") %>%
  summarise(top = max(Mean + 1.96*SE, na.rm = TRUE)) %>%
  pull(top)

y_bracket <- y_post_top + 0.4  

# 
tick <- yrange * 0.03
lift <- yrange * 0.015

plot_si_sig <- plot_si +
  scale_y_continuous(
    expand = expansion(mult = c(0.02, 0.10))
  ) +
  annotate("segment", x = 1.90, xend = 2.10,
           y = y_bracket, yend = y_bracket, linewidth = 0.6) +
  annotate("segment", x = 1.90, xend = 1.90,
           y = y_bracket, yend = y_bracket - tick, linewidth = 0.6) +
  annotate("segment", x = 2.10, xend = 2.10,
           y = y_bracket, yend = y_bracket - tick, linewidth = 0.6) +
  annotate("text", x = 2,
           y = y_bracket + lift, label = sig_label, size = 6)

plot_si_sig

#
plot_dat <- mydata %>%
  select(Group, JA_Pre, JA_Post) %>%
  gather(key = "Time", value = "Value", JA_Pre, JA_Post) %>%
  mutate(
    Time  = ifelse(Time == "JA_Pre", "Pre", "Post"),
    Time  = factor(Time, levels = c("Pre","Post")),
    Group = factor(Group, levels = c(1,0),
                   labels = c("Intervention","Control"))
  ) %>%
  group_by(Group, Time) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SE   = sd(Value, na.rm = TRUE) / sqrt(sum(!is.na(Value))),
    .groups = "drop"
  )

plot_ja <- ggplot(plot_dat,
                  aes(Time, Mean,
                      group = Group,
                      linetype = Group,
                      shape = Group)) +
  geom_line(size = 1.1) +
  geom_point(size = 2.4) +
  geom_errorbar(aes(ymin = Mean - 1.96*SE,
                    ymax = Mean + 1.96*SE),
                width = 0.06, linewidth = 0.6) +
  scale_linetype_manual(values = c("Intervention"="solid",
                                   "Control"="71")) +
  scale_shape_manual(values = c(16,16)) +
  labs(y = "SIJA", x = NULL) +
  theme_classic(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 16, color = "black"),
    axis.text.y  = element_text(size = 16, color = "black"),
    axis.line    = element_line(size = 0.6),
    axis.ticks   = element_line(size = 0.6),
    legend.position = c(0.88, 0.80),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 13),
    legend.title = element_blank(),
    plot.margin = margin(t = 10, r = 70, b = 10, l = 10),
    legend.key.width = unit(1.1, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(linetype = c("solid","44"))
    )
  )

plot_ja <- plot_ja +
  scale_y_continuous(
    limits = c(4, 18),
    breaks = seq(4, 18, by = 2)
  )

plot_ja
#
plot_dat <- mydata %>%
  select(Group, JE_Pre, JE_Post) %>%
  gather(key = "Time", value = "Value", JE_Pre, JE_Post) %>%
  mutate(
    Time  = ifelse(Time == "JE_Pre", "Pre", "Post"),
    Time  = factor(Time, levels = c("Pre","Post")),
    Group = factor(Group, levels = c(1,0),
                   labels = c("Intervention","Control"))
  ) %>%
  group_by(Group, Time) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SE   = sd(Value, na.rm = TRUE) / sqrt(sum(!is.na(Value))),
    .groups = "drop"
  )

plot_je <- ggplot(plot_dat,
                  aes(Time, Mean,
                      group = Group,
                      linetype = Group,
                      shape = Group)) +
  geom_line(size = 1.1) +
  geom_point(size = 2.4) +
  geom_errorbar(aes(ymin = Mean - 1.96*SE,
                    ymax = Mean + 1.96*SE),
                width = 0.06, linewidth = 0.6) +
  scale_linetype_manual(values = c("Intervention"="solid",
                                   "Control"="71")) +
  scale_shape_manual(values = c(16,16)) +
  labs(y = expression(JE^"\u2020"), x = NULL)+
  theme_classic(base_size = 14) +
  theme(
    axis.title.y = element_text(size = 18),
    axis.text.x  = element_text(size = 16, color = "black"),
    axis.text.y  = element_text(size = 16, color = "black"),
    axis.line    = element_line(size = 0.6),
    axis.ticks   = element_line(size = 0.6),
    legend.position = c(0.88, 0.80),
    legend.justification = c(0, 1),
    legend.text = element_text(size = 13),
    legend.title = element_blank(),
    plot.margin = margin(t = 10, r = 70, b = 10, l = 10),
    legend.key.width = unit(1.1, "cm")
  ) +
  guides(
    linetype = guide_legend(
      override.aes = list(linetype = c("solid","44"))
    )
  )

plot_je

#

sig_label <- "**"

# Post 两组 CI 上界最大值
y_post_top <- plot_dat %>%
  filter(Time == "Post") %>%
  summarise(top = max(Mean + 1.96*SE, na.rm = TRUE)) %>%
  pull(top)

# y 轴刻度范围（你明确规定的）
y_limits <- c(0, 0.5)
yrange   <- diff(y_limits)

# 括号位置与尺寸 —— 全部用 y 轴比例
y_bracket <- y_post_top + yrange * 0.04   # 括号离误差线
tick      <- yrange * 0.03               # 括号“脚”长度（关键）
lift      <- yrange * 0.015               # 星号高度

plot_je_sig <- plot_je +
  scale_y_continuous(
    limits = y_limits,
    breaks = seq(0, 0.5, by = 0.1),
    expand = expansion(mult = c(0.02, 0.15))
  ) +
  annotate("segment", x = 1.90, xend = 2.10,
           y = y_bracket, yend = y_bracket, linewidth = 0.6) +
  annotate("segment", x = 1.90, xend = 1.90,
           y = y_bracket, yend = y_bracket - tick, linewidth = 0.6) +
  annotate("segment", x = 2.10, xend = 2.10,
           y = y_bracket, yend = y_bracket - tick, linewidth = 0.6) +
  annotate("text", x = 2,
           y = y_bracket + lift,
           label = sig_label, size = 6)

plot_je_sig







out_dir <- "C:/Users/J.N.RAN/Desktop/ASD图表"

plot_list <- list(
  Fig_EVT = plot_evt_sig,
  Fig_USI  = plot_si_sig,
  Fig_JE  = plot_je_sig,
  Fig_PPVT  = plot_ppvt,
  Fig_SIJA  = plot_ja,
  Fig_SRS  = plot_srs
)

for (nm in names(plot_list)) {
  ggsave(
    filename = file.path(out_dir, paste0(nm, ".pdf")),
    plot     = plot_list[[nm]],
    width    = 7.1,
    height   = 4.8,
    units    = "in",
    device   = cairo_pdf
  )
}




for (nm in names(plot_list)) {
  ggsave(
    filename = file.path(out_dir, paste0(nm, ".png")),
    plot     = plot_list[[nm]],
    width    = 7.1,
    height   = 4.8,
    dpi      = 300,
    device   = "png",
    type     = "cairo"
  )
}


ggsave(
  filename = file.path(out_dir, "Fig_SIJA.png"),
  plot     = plot_ja,
  width    = 7.1,
  height   = 4.8,
  units    = "in",
  dpi      = 300,
  device   = "png",
  type     = "cairo"
)


