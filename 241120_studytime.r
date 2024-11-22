
#提取2020年的person数据
install.packages("haven")
library(haven)
data <- read_dta("/Users/evan/Downloads/cfps2020person_202306.dta")

#提取2020年的adult数据中的关键变量：
#是否上学，现在上哪个阶段，目前就读学校所在地，现在上哪类小学，现在上哪类初中，现在上哪类高中
#是否公立学校，是否示范/重点学校，哪种类型的私立学校，是否在重点班
#班级排名，年级排名，非周末学习时间，周末学习时间，学业满意度，压力度
library(dplyr)
variable_list_2020 <- c("qc1", "qc3", "qs1002", "qs3", "qs4_b_1", "qs5", 
	"qs601", "qs602", "qs603", "qs604", 
	"kr425", "kr426", "qs1011", "qs1012", "qs501_b_2", "qs502" )
selected_data_2020 <- data %>% select(all_of(variable_list_2020))
head(selected_data_2020)

#报错无法找到qs4和qs501，查找相似变量，定为qs4_b_1和qs501_b_2
grep("qs4", colnames(data), value = TRUE)
grep("qs501", colnames(data), value = TRUE)

#保存提取的数据
install.packages("writexl")
library(writexl)
write_xlsx(selected_data_2020, "/Users/evan/Downloads/studyingtime_data_2020.xlsx")

#清洗excel数据，分为普通小学/普通初中/普通高中3个sheet
install.packages("readxl")
library(readxl)

data_primary_2020 <- read_excel("/Users/evan/Downloads/studyingtime_data_2020.xlsx", sheet = 2)
data_middle_2020 <- read_excel("/Users/evan/Downloads/studyingtime_data_2020.xlsx", sheet = 3)
data_high_2020 <- read_excel("/Users/evan/Downloads/studyingtime_data_2020.xlsx", sheet = 4)

# 定义一个函数来剔除异常值
remove_outliers <- function(data, column) {
  q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  # 返回剔除异常值的数据
  data %>% filter(data[[column]] >= lower_bound & data[[column]] <= upper_bound)
}

# 剔除各学习阶段的异常值
clean_data_primary_2020 <- remove_outliers(data_primary_2020, "total_st")
clean_data_middle_2020 <- remove_outliers(data_middle_2020, "total_st")
clean_data_high_2020 <- remove_outliers(data_high_2020, "total_st")

# 计算剔除异常值后的均值
mean_primary_2020 <- mean(clean_data_primary_2020$total_st, na.rm = TRUE)
mean_middle_2020 <- mean(clean_data_middle_2020$total_st, na.rm = TRUE)
mean_high_2020 <- mean(clean_data_high_2020$total_st, na.rm = TRUE)

# 计算95%置信区间
ci_primary_2020 <- t.test(clean_data_primary_2020$total_st, conf.level = 0.95)$conf.int
ci_middle_2020 <- t.test(clean_data_middle_2020$total_st, conf.level = 0.95)$conf.int
ci_high_2020 <- t.test(clean_data_high_2020$total_st, conf.level = 0.95)$conf.int

results_2020 <- list(
  mean_primary_2020 = mean_primary_2020,
  ci_primary_2020 = ci_primary_2020,
  mean_middle_2020 = mean_middle_2020,
  ci_middle_2020 = ci_middle_2020,
  mean_high_2020 = mean_high_2020,
  ci_high_2020 = ci_high_2020
)

# 输出所有结果
results_2020

#输出结果
$mean_primary_2020
[1] 44.6754

$ci_primary_2020
[1] 43.85343 45.49737
attr(,"conf.level")
[1] 0.95

$mean_middle_2020
[1] 54.27734

$ci_middle_2020
[1] 53.26646 55.28823
attr(,"conf.level")
[1] 0.95

$mean_high_2020
[1] 67.19117

$ci_high_2020
[1] 65.60286 68.77948
attr(,"conf.level")
[1] 0.95

#========================================================================================
#提取2018年的person数据
data_18 <- read_dta("/Users/evan/Downloads/cfps2018person_202012.dta")

#提取2018年数据中的关键变量：
#是否上学，现在上哪个阶段，目前就读学校所在地，现在上哪类小学，现在上哪类初中，现在上哪类高中
#是否公立学校，是否示范/重点学校，哪种类型的私立学校，是否在重点班
#班级排名，年级排名，非周末学习时间，周末学习时间，学业满意度，压力度
library(dplyr)
variable_list_2018 <- c("qc1", "qc3", "qs1002", "qs3", "qs4_b_1", "qs5", 
	"qs601", "qs602", "qs603", "qs604", 
	"kr425", "kr426", "qs1011", "qs1012", "qs501_b_2", "qs502" )
selected_data_2018 <- data_18 %>% select(all_of(variable_list_2020))
head(selected_data_2018)

#保存提取的数据
write_xlsx(selected_data_2018, "/Users/evan/Downloads/studyingtime_data_2018.xlsx")

#清洗excel数据，分为普通小学/普通初中/普通高中3个sheet
data_primary_2018 <- read_excel("/Users/evan/Downloads/studyingtime_data_2018.xlsx", sheet = 2)
data_middle_2018 <- read_excel("/Users/evan/Downloads/studyingtime_data_2018.xlsx", sheet = 3)
data_high_2018 <- read_excel("/Users/evan/Downloads/studyingtime_data_2018.xlsx", sheet = 4)

# 剔除各学习阶段的异常值
clean_data_primary_2018 <- remove_outliers(data_primary_2018, "total_st")
clean_data_middle_2018 <- remove_outliers(data_middle_2018, "total_st")
clean_data_high_2018 <- remove_outliers(data_high_2018, "total_st")

# 计算剔除异常值后的均值
mean_primary_2018 <- mean(clean_data_primary_2018$total_st, na.rm = TRUE)
mean_middle_2018 <- mean(clean_data_middle_2018$total_st, na.rm = TRUE)
mean_high_2018 <- mean(clean_data_high_2018$total_st, na.rm = TRUE)

# 计算95%置信区间
ci_primary_2018 <- t.test(clean_data_primary_2018$total_st, conf.level = 0.95)$conf.int
ci_middle_2018 <- t.test(clean_data_middle_2018$total_st, conf.level = 0.95)$conf.int
ci_high_2018 <- t.test(clean_data_high_2018$total_st, conf.level = 0.95)$conf.int

results_2018 <- list(
  mean_primary_2018 = mean_primary_2018,
  ci_primary_2018 = ci_primary_2018,
  mean_middle_2018 = mean_middle_2018,
  ci_middle_2018 = ci_middle_2018,
  mean_high_2018 = mean_high_2018,
  ci_high_2018 = ci_high_2018
)

# 输出所有结果
results_2018

#输出结果
$mean_primary_2018
[1] 43.42495

$ci_primary_2018
[1] 42.78611 44.06379
attr(,"conf.level")
[1] 0.95

$mean_middle_2018
[1] 56.15609

$ci_middle_2018
[1] 55.32360 56.98858
attr(,"conf.level")
[1] 0.95

$mean_high_2018
[1] 66.37524

$ci_high_2018
[1] 64.99121 67.75928
attr(,"conf.level")
[1] 0.95

#========================================================================================

#提取2016年的person数据
data_16 <- read_dta("/Users/evan/Downloads/cfps2016adult_201906.dta")

#提取2016年数据中的关键变量：
#是否上学，现在上哪个阶段，目前就读学校所在地，现在上哪类小学，现在上哪类初中，现在上哪类高中
#是否公立学校，是否示范/重点学校，哪种类型的私立学校，是否在重点班
#班级排名，年级排名，非周末学习时间，周末学习时间，学业满意度，压力度
library(dplyr)
variable_list_2016 <- c("pc1", "pc3", "ps1002", "ps3", "ps4", "ps5", 
	"ps601", "ps602", "ps603", "ps604", 
	"kr425", "kr426", "ks1011", "ks1012", "ks501", "ks502")
selected_data_2016 <- data_16 %>% select(all_of(variable_list_2016))
head(selected_data_2016)

#保存提取的数据
write_xlsx(selected_data_2016, "/Users/evan/Downloads/studyingtime_data_2016.xlsx")

#清洗excel数据，分为普通小学/普通初中/普通高中3个sheet
data_primary_2016 <- read_excel("/Users/evan/Downloads/studyingtime_data_2016.xlsx", sheet = 2)
data_middle_2016 <- read_excel("/Users/evan/Downloads/studyingtime_data_2016.xlsx", sheet = 3)
data_high_2016 <- read_excel("/Users/evan/Downloads/studyingtime_data_2016.xlsx", sheet = 4)

# 剔除各学习阶段的异常值
clean_data_primary_2016 <- remove_outliers(data_primary_2016, "total_st")
clean_data_middle_2016 <- remove_outliers(data_middle_2016, "total_st")
clean_data_high_2016 <- remove_outliers(data_high_2016, "total_st")

# 计算剔除异常值后的均值
mean_primary_2016 <- mean(clean_data_primary_2016$total_st, na.rm = TRUE)
mean_middle_2016 <- mean(clean_data_middle_2016$total_st, na.rm = TRUE)
mean_high_2016 <- mean(clean_data_high_2016$total_st, na.rm = TRUE)

# 计算95%置信区间
ci_primary_2016 <- t.test(clean_data_primary_2016$total_st, conf.level = 0.95)$conf.int
ci_middle_2016 <- t.test(clean_data_middle_2016$total_st, conf.level = 0.95)$conf.int
ci_high_2016 <- t.test(clean_data_high_2016$total_st, conf.level = 0.95)$conf.int

results_2016 <- list(
  mean_primary_2016 = mean_primary_2016,
  ci_primary_2016 = ci_primary_2016,
  mean_middle_2016 = mean_middle_2016,
  ci_middle_2016 = ci_middle_2016,
  mean_high_2016 = mean_high_2016,
  ci_high_2016 = ci_high_2016
)

# 输出所有结果
results_2016

#输出结果
$mean_primary_2016
[1] 39.42857

$ci_primary_2016
[1] 31.93680 46.92034
attr(,"conf.level")
[1] 0.95

$mean_middle_2016
[1] 53.60451

$ci_middle_2016
[1] 51.52728 55.68173
attr(,"conf.level")
[1] 0.95

$mean_high_2016
[1] 67.53339

$ci_high_2016
[1] 66.14706 68.91971
attr(,"conf.level")
[1] 0.95

#========================================================================================

#提取2014年的person数据
data_14 <- read_dta("/Users/evan/Downloads/cfps2014adult_201906.dta")

#提取2014年数据中的关键变量：
#是否上学，现在上哪个阶段，目前就读学校所在地，现在上哪类小学，现在上哪类初中，现在上哪类高中
#是否示范/重点学校，哪种类型的学校，是否在重点班
#年级排名，班级排名，非周末学习时间，周末学习时间，学业满意度，压力度

library(dplyr)
variable_list_2014 <- c("wc01", "kr1", "kra2", "kr2", "kr3", "kr4", 
	"kr402ma", "kr402m", "kr403", 
	"kr426", "kr425", "ks1011", "ks1012", "ks501", "ks502")
selected_data_2014 <- data_14 %>% select(all_of(variable_list_2014))
head(selected_data_2014)

#保存提取的数据
install.packages("writexl")
library(writexl)
write_xlsx(selected_data_2014, "/Users/evan/Downloads/studyingtime_data_2014.xlsx")

#清洗excel数据，分为普通小学/普通初中/普通高中3个sheet
install.packages("readxl")
library(readxl)

data_primary_2014 <- read_excel("/Users/evan/Downloads/studyingtime_data_2014.xlsx", sheet = 2)
data_middle_2014 <- read_excel("/Users/evan/Downloads/studyingtime_data_2014.xlsx", sheet = 3)
data_high_2014 <- read_excel("/Users/evan/Downloads/studyingtime_data_2014.xlsx", sheet = 4)

# 剔除各学习阶段的异常值
clean_data_primary_2014 <- remove_outliers(data_primary_2014, "total_st")
clean_data_middle_2014 <- remove_outliers(data_middle_2014, "total_st")
clean_data_high_2014 <- remove_outliers(data_high_2014, "total_st")

# 计算剔除异常值后的均值
mean_primary_2014 <- mean(clean_data_primary_2014$total_st, na.rm = TRUE)
mean_middle_2014 <- mean(clean_data_middle_2014$total_st, na.rm = TRUE)
mean_high_2014 <- mean(clean_data_high_2014$total_st, na.rm = TRUE)

# 计算95%置信区间
ci_primary_2014 <- t.test(clean_data_primary_2014$total_st, conf.level = 0.95)$conf.int
ci_middle_2014 <- t.test(clean_data_middle_2014$total_st, conf.level = 0.95)$conf.int
ci_high_2014 <- t.test(clean_data_high_2014$total_st, conf.level = 0.95)$conf.int

results_2014 <- list(
  mean_primary_2014 = mean_primary_2014,
  ci_primary_2014 = ci_primary_2014,
  mean_middle_2014 = mean_middle_2014,
  ci_middle_2014 = ci_middle_2014,
  mean_high_2014 = mean_high_2014,
  ci_high_2014 = ci_high_2014
)

# 输出所有结果
results_2014

#输出结果
$mean_primary_2014
[1] 35.125

$ci_primary_2014
[1] 14.97256 55.27744
attr(,"conf.level")
[1] 0.95

$mean_middle_2014
[1] 53.30278

$ci_middle_2014
[1] 51.18370 55.42186
attr(,"conf.level")
[1] 0.95

$mean_high_2014
[1] 66.34633

$ci_high_2014
[1] 65.03169 67.66098
attr(,"conf.level")
[1] 0.95

#========================================================================================

# 加载必要的包
install.packages("purrr")
library(purrr)
install.packages("readxl")
install.packages("dplyr")
# install.packages("broom")
library(readxl)
library(dplyr)
# library(broom)

# 读取初中（sheet3）和高中（sheet4）数据
data_middle_2020 <- read_excel("/Users/evan/Downloads/studyingtime_data_2020.xlsx", sheet = 3)
data_high_2020 <- read_excel("/Users/evan/Downloads/studyingtime_data_2020.xlsx", sheet = 4)

# 定义剔除异常值的函数
remove_outliers <- function(data, column) {
  q1 <- quantile(data[[column]], 0.25, na.rm = TRUE)
  q3 <- quantile(data[[column]], 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  # 返回剔除异常值的数据
  data %>% filter(data[[column]] >= lower_bound & data[[column]] <= upper_bound)
}

# 按所在地类别计算剔除异常值后的平均值和置信区间
group_analysis <- function(data, location_var, study_time_var) {
  # 剔除异常值
  clean_data <- remove_outliers(data, study_time_var)
  
  # 按所在地进行分组
  summary <- clean_data %>%
    group_by(!!sym(location_var)) %>%
    summarise(
      mean_study_time = mean(!!sym(study_time_var), na.rm = TRUE),
      sample_size = n(),
      ci = if (n() > 1 && var(!!sym(study_time_var), na.rm = TRUE) > 0) {
        list(t.test(!!sym(study_time_var), conf.level = 0.95)$conf.int)
      } else {
        list(c(NA, NA))
      }
    ) %>%
    mutate(
      ci_lower = map_dbl(ci, 1),
      ci_upper = map_dbl(ci, 2)
    ) %>% select(-ci)
  
  return(summary)
}

# 计算初中和高中不同所在地学校的学习时长平均值和置信区间
middle_summary_20 <- group_analysis(data_middle_2020, "qs1002", "total_st")
high_summary_20 <- group_analysis(data_high_2020, "qs1002", "total_st")

# 输出结果
middle_summary_20
high_summary_20

> middle_summary_20
# A tibble: 5 × 5
  qs1002 mean_study_time sample_size ci_lower ci_upper
   <dbl>           <dbl>       <int>    <dbl>    <dbl>
1     -1            26             1     NA       NA  
2      1            61.5          64     57.1     65.8
3      2            55.4         157     52.8     58.0
4      3            55.3         289     53.5     57.0
5      4            52.3         460     50.9     53.8
> high_summary_20
# A tibble: 5 × 5
  qs1002 mean_study_time sample_size ci_lower ci_upper
   <dbl>           <dbl>       <int>    <dbl>    <dbl>
1     -1            46             2     NA       NA  
2      1            65.6          39     59.3     71.9
3      2            67.5         144     64.5     70.5
4      3            67.8         308     65.7     69.9
5      4            64.4          39     58.2     70.6

#========================================================================================

# 读取初中（sheet3）和高中（sheet4）数据
data_middle_2018 <- read_excel("/Users/evan/Downloads/studyingtime_data_2018.xlsx", sheet = 3)
data_high_2018 <- read_excel("/Users/evan/Downloads/studyingtime_data_2018.xlsx", sheet = 4)

# 计算2018年初中和高中不同所在地学校的学习时长平均值和置信区间
middle_summary_2018 <- group_analysis(data_middle_2018, "qs1002", "total_st")
high_summary_2018 <- group_analysis(data_high_2018, "qs1002", "total_st")

# 输出结果
middle_summary_2018
high_summary_2018

> middle_summary_2018
# A tibble: 4 × 5
  qs1002 mean_study_time sample_size ci_lower ci_upper
   <dbl>           <dbl>       <int>    <dbl>    <dbl>
1      1            63.3          72     59.9     66.7
2      2            58.8         167     56.4     61.3
3      3            56.5         333     55.1     58.0
4      4            54.2         553     53.1     55.3
> high_summary_2018
# A tibble: 4 × 5
  qs1002 mean_study_time sample_size ci_lower ci_upper
   <dbl>           <dbl>       <int>    <dbl>    <dbl>
1      1            65.4          35     59.3     71.5
2      2            66.2         176     63.6     68.7
3      3            67.3         344     65.4     69.1
4      4            62.4          59     57.7     67.1

#========================================================================================

# 读取初中（sheet3）和高中（sheet4）数据
data_middle_2016 <- read_excel("/Users/evan/Downloads/studyingtime_data_2016.xlsx", sheet = 3)
data_high_2016 <- read_excel("/Users/evan/Downloads/studyingtime_data_2016.xlsx", sheet = 4)

# 计算2016年初中和高中不同所在地学校的学习时长平均值和置信区间
middle_summary_2016 <- group_analysis(data_middle_2016, "ps1002", "total_st")
high_summary_2016 <- group_analysis(data_high_2016, "ps1002", "total_st")

# 输出结果
middle_summary_2016
high_summary_2016

> middle_summary_2016
# A tibble: 5 × 5
  ps1002 mean_study_time sample_size ci_lower ci_upper
   <dbl>           <dbl>       <int>    <dbl>    <dbl>
1     -8            55.2         129     52.0     58.3
2      1            31             1     NA       NA  
3      2            58.7           6     44.4     72.9
4      3            55.8          35     51.4     60.2
5      4            49.7          73     46.3     53.0
> high_summary_2016
# A tibble: 5 × 5
  ps1002 mean_study_time sample_size ci_lower ci_upper
   <dbl>           <dbl>       <int>    <dbl>    <dbl>
1     -8            67.1         222     64.7     69.5
2      1            63.0          22     55.5     70.5
3      2            72.5         111     69.2     75.7
4      3            66.5         232     64.2     68.7
5      4            65.3          48     60.1     70.5

#========================================================================================

# 读取初中（sheet3）和高中（sheet4）数据
data_middle_2014 <- read_excel("/Users/evan/Downloads/studyingtime_data_2014.xlsx", sheet = 3)
data_high_2014 <- read_excel("/Users/evan/Downloads/studyingtime_data_2014.xlsx", sheet = 4)

# 计算2014年初中和高中不同所在地学校的学习时长平均值和置信区间
middle_summary_2014 <- group_analysis(data_middle_2014, "kra2", "total_st")
high_summary_2014 <- group_analysis(data_high_2014, "kra2", "total_st")

# 输出结果
middle_summary_2014
high_summary_2014

> middle_summary_2014
# A tibble: 4 × 5
   kra2 mean_study_time sample_size ci_lower ci_upper
  <dbl>           <dbl>       <int>    <dbl>    <dbl>
1     1            63.6           6     48.4     78.8
2     2            55.1          15     46.2     64.1
3     3            52.1          50     48.2     56.1
4     4            53.0         109     50.3     55.7
> high_summary_2014
# A tibble: 4 × 5
   kra2 mean_study_time sample_size ci_lower ci_upper
  <dbl>           <dbl>       <int>    <dbl>    <dbl>
1     1            71.0          54     66.1     75.8
2     2            67.7         187     65.2     70.3
3     3            65.2         365     63.4     67.0
4     4            65.2          76     61.2     69.3
