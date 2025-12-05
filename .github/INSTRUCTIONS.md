# 📋 2025-ML 学习班编码与提交规范

---

## 📑 目录

- [编码风格指南](#编码风格指南)
- [文件组织结构](#文件组织结构)
- [提交规范](#提交规范)
- [文档撰写规范](#文档撰写规范)
- [协作工作流](#协作工作流)
- [常见问题](#常见问题)

---

## 🎯 编码风格指南

### R 语言编码规范

#### 基础原则
- **范式**：严格遵循 `tidyverse` 风格，代码应当清晰、优雅、高效
- **管道操作符**：使用现代管道 `|>` （推荐）或 `%>%` （兼容旧版本）
- **禁止事项**：
  - ❌ 避免基础 R 子集操作（如 `df[df$col > 5, ]`），改用 `filter()`
  - ❌ 严格禁止使用 `for`、`while` 循环和 `apply` 族函数
  - ❌ 避免已弃用的包（如 `reshape2`、`plyr`）

#### 核心包要求

| 功能领域 | 推荐包 | 说明 |
|--------|------|------|
| 数据操作 | `dplyr`, `tidyr`, `forcats` | 必用，禁用基础 R 子集 |
| 迭代 | `purrr` | **必须**使用 `map`、`map_dfr` 等，严禁 `for` 循环 |
| 可视化 | `ggplot2` | 专业出版级别的图表 |
| 机器学习 | `mlr3` 生态 | 不使用 `caret` |
| 统计建模 | `parsnip`, `workflows` | 现代建模框架 |

#### 代码示例

**✅ 推荐写法**
```r
# 使用管道和现代函数
模型数据 <- 患者数据 |>
  filter(年龄 > 18, !is.na(bmi)) |>
  mutate(
    bmi_分类 = case_when(
      bmi < 18.5 ~ "偏瘦",
      bmi < 25 ~ "正常",
      bmi < 30 ~ "超重",
      TRUE ~ "肥胖"
    ),
    年龄组 = cut(年龄, breaks = c(18, 30, 45, 60, 100))
  ) |>
  select(patient_id, bmi_分类, 年龄组, 诊断结果)

# 使用 purrr 迭代
结果列表 <- 模型列表 |>
  map_dfr(~cross_validate(data = 数据集, learner = .x), .id = "模型名称")
```

**❌ 避免**
```r
# 不要这样写
for (i in 1:nrow(数据)) {
  if (数据[i, "age"] > 18) {
    # 循环操作
  }
}

# 不要基础 R 子集
患者 <- 数据[数据$诊断 == "糖尿病", ]
```

### Python 编码规范

#### 基础原则
- **风格**：遵循"整洁"(Tidy) 哲学，使用 **方法链式调用**
- **类型提示**：所有函数必须包含类型注解
- **库选择**：使用现代 `pandas`、`scikit-learn`（不用旧版本）

#### 核心包要求

| 功能领域 | 推荐包 | 说明 |
|--------|------|------|
| 数据操作 | `pandas` | 使用方法链式调用 |
| 可视化 | `seaborn` (objects 接口) | 现代美学 |
| 机器学习 | 现代 `scikit-learn` | 避免已弃用的 API |
| 深度学习 | `PyTorch`, `TensorFlow` | 按需选择 |

#### 代码示例

**✅ 推荐写法**
```python
from pandas import read_csv
from sklearn.preprocessing import StandardScaler
from sklearn.model_selection import cross_val_score

# 方法链式调用
患者_处理 = (
    read_csv("patient_data.csv")
    .query("age > 18 and bmi.notna()")
    .assign(
        bmi_category=lambda df: pd.cut(
            df["bmi"],
            bins=[0, 18.5, 25, 30, float("inf")],
            labels=["underweight", "normal", "overweight", "obese"]
        )
    )
    .select_dtypes(include=["number"])
)

# 使用 list comprehension 和 map
结果 = [cross_val_score(model, X, y) for model in 模型列表]
```

### 变量命名规范

#### 命名格式
- **格式**：严格使用 **`snake_case`**（蛇形命名法）
- **禁止**：❌ camelCase、PascalCase、UPPERCASE

#### 语义清晰性

| 类型 | 示例 | 说明 |
|-----|-----|------|
| 数据框 | `patient_data`, `blood_test_results` | 描述性名称，避免 `df`, `data`, `temp` |
| 数值变量 | `bmi_score`, `systolic_bp` | 包含单位/指标说明 |
| 分类变量 | `treatment_group`, `disease_status` | 明确分类含义 |
| 列表/向量 | `patient_ids`, `model_names` | 复数形式表示集合 |
| 函数 | `calculate_bmi()`, `validate_data()` | 动词开头 |
| 模型 | `logistic_model`, `rf_classifier` | 包含模型类型 |
| 图表 | `bmi_distribution_plot`, `roc_curve` | 描述性后缀 `_plot` 或 `_curve` |

#### 医学数据缩写约定

允许的医学缩写（保持可读性）：
- `bmi` - 体质指数 (Body Mass Index)
- `bp` - 血压 (Blood Pressure)
- `hr` - 心率 (Heart Rate)
- `fbc` - 全血细胞计数 (Full Blood Count)
- `glucose` - 血糖
- `hba1c` - 血红蛋白 A1c

**示例**：
```r
patient_data <- tibble(
  patient_id = c(1001, 1002, 1003),
  age_years = c(45, 52, 38),
  bmi_score = c(24.5, 28.2, 22.1),
  systolic_bp = c(120, 135, 118),
  treatment_group = c("A", "B", "A"),
  disease_status = factor(c("控制", "活跃", "缓解"))
)
```

---

## 📁 文件组织结构

### 目录层级规范

```
2025-ML/
├── 📄 README.md                          # 项目总览
├── 📄 INSTRUCTIONS.md                    # 本文件
├── 📄 LICENSE                            # 许可证
├── 📄 .gitignore                         # Git 忽略配置
│
├── 📁 01_ref_book/                       # 参考资料库
│   ├── 📚 数学基础/
│   ├── 📚 代码基础/
│   ├── 📚 机器学习/
│   └── 📚 深度学习/
│
├── 📁 02_proj_code/                      # 项目代码（核心）
│   ├── 01_机器学习框架/
│   │   ├── framework_demo.R              # 演示代码
│   │   ├── framework_demo.Rmd            # R Markdown 文档
│   │   └── README.md                     # 子章节说明
│   ├── 02_数据预处理/
│   ├── 03_机器学习评价体系/
│   ├── ... (其他章节)
│   └── 16_深度学习总结与项目实战/
│
├── 📁 99_others/                         # 其他资源
│   ├── 📝 配置文件/
│   └── 📊 辅助资料/
│
└── 📁 .github/
    ├── workflows/                        # CI/CD 工作流
    └── ISSUE_TEMPLATE/                   # 问题模板
```

### 代码文件命名规范

#### 标准格式
```
[序号]_[模块名称]_[功能描述].[扩展名]
```

#### 示例

| 文件名 | 描述 |
|-------|------|
| `01_data_loading_preprocessing.R` | 第 01 讲数据加载与预处理 |
| `02_eda_visualization.Rmd` | 第 02 讲 EDA 与可视化报告 |
| `03_model_evaluation_metrics.py` | 第 03 讲模型评估指标（Python） |
| `05_svm_kernel_methods.R` | 第 05 讲支持向量机与核方法 |
| `helpers.R` | 工具函数库 |
| `test_data_validation.R` | 数据验证测试函数 |

---

## 💾 提交规范

### Git 提交信息格式

遵循 **Conventional Commits** 规范：

```
<type>(<scope>): <subject>

<body>

<footer>
```

#### 类型 (type)

| 类型 | 说明 | 示例 |
|-----|------|------|
| `feat` | 新功能/新内容 | `feat(lecture-05): add SVM kernel methods implementation` |
| `fix` | 修复 bug | `fix(lecture-03): correct ROC curve calculation` |
| `docs` | 文档更新 | `docs(README): update learning materials section` |
| `refactor` | 代码重构 | `refactor(data-pipeline): simplify preprocessing steps` |
| `style` | 代码风格（无功能改变） | `style: format R code with styler` |
| `test` | 添加/修改测试 | `test(model-evaluation): add cross-validation tests` |
| `perf` | 性能优化 | `perf: improve data loading speed by 30%` |
| `chore` | 构建配置、依赖更新 | `chore: update mlr3 packages` |

#### 范围 (scope)

- 讲座编号：`lecture-01`, `lecture-12` 等
- 模块名：`data-pipeline`, `model-evaluation`, `visualization` 等
- 文件名或功能：`svm-kernel`, `cross-validation`, `eda` 等

#### 主题 (subject)

- 使用**英文**或**中文**（项目团队约定）
- 简明扼要，**不超过 50 字符**
- 以**祈使句**表达（如 `add`, `fix`, `update` 而非 `added`, `fixed`）
- **首字母小写**（除非必要）

#### 提交体 (body)

- 说明**做了什么**和**为什么做**
- 限制每行 **72 字符**
- 包含**技术细节**或**配置参数**

#### 示例提交

```
feat(lecture-05): implement SVM with RBF kernel

- Add kernel parameter tuning functionality
- Include cross-validation for hyperparameter optimization
- Add visualization of decision boundaries
- Include performance metrics (ROC-AUC, F1-score)

This addresses the need for comprehensive SVM implementation
in the ML framework course.

Closes #25
```

### 分支管理

#### 分支命名规范

```
<type>/<feature-name>
```

| 分支类型 | 命名示例 | 说明 |
|---------|--------|------|
| 功能分支 | `feature/lecture-05-svm` | 新增讲座内容 |
| 修复分支 | `fix/cross-validation-bug` | 修复已知问题 |
| 文档分支 | `docs/update-readme` | 文档更新 |
| 发布分支 | `release/v1.0` | 版本发布 |

#### 工作流

1. 从 `main` 分支拉取最新代码
2. 创建功能分支：`git checkout -b feature/lecture-05-svm`
3. 定期提交有意义的变更
4. 完成后，创建 Pull Request (PR)
5. 通过代码审查后合并回 `main`

---

## 📝 文档撰写规范

### Markdown 文档规范

#### 标题层级

```markdown
# 第一级标题（仅用于文件标题）
## 第二级标题（章节）
### 第三级标题（子章节）
#### 第四级标题（细节）
```

#### 结构模板

```markdown
# [讲座编号] [讲座名称]

## 概述
- **学习目标**：列出 3-5 个核心学习目标
- **前置知识**：列出必要的先修知识
- **预期时间**：估计学习/讲座时间

## 理论基础
### 核心概念
[详细说明]

### 数学原理
[必要时给出数学公式]

## 代码实现
### 环境配置
```r
# 必要的库
library(tidyverse)
library(mlr3)
library(ggplot2)
```

### 演示案例
[完整代码示例]

## 应用场景
- 场景 1：[描述]
- 场景 2：[描述]

## 常见陷阱
1. **陷阱 1**：描述问题及解决方案
2. **陷阱 2**：描述问题及解决方案

## 参考资源
- 教科书：[引用]
- 在线资源：[URL]
- 论文：[引用]

## 练习题
1. 题目 1：[描述]
2. 题目 2：[描述]
```

### R Markdown 文档规范

#### YAML 头部模板

```yaml
---
title: "[讲座编号] [讲座名称]"
subtitle: "[副标题]"
author: "[作者名]"
date: "`r Sys.Date()`"
output:
  html_document:
    theme: flatly
    highlight: tango
    toc: true
    toc_depth: 3
    number_sections: true
    fig_width: 10
    fig_height: 6
---
```

#### 代码块标记

```r
# ✅ 推荐：包含标签和选项
{r chunk_01_data_loading, echo=TRUE, warning=FALSE, message=FALSE}
数据 <- read_csv("patient_data.csv")
```

### 代码注释规范

#### 注释风格

```r
# 单行注释：解释逻辑
# 使用完整句子，首字母大写

# 多行注释：
# 第一行描述总体目标
# 第二行提供具体实现细节
# 第三行说明潜在影响

# ---- 分隔符 ----
# 用于分隔不同的逻辑段落

# TODO: 待完成功能
# FIXME: 已知问题需要修复
# NOTE: 重要提醒
# WARNING: 警告信息
```

#### 函数文档

```r
# 计算患者 BMI 指数
#
# 此函数根据身高和体重计算体质指数。
#
# @param 体重_kg 患者体重，单位为千克
# @param 身高_cm 患者身高，单位为厘米
#
# @return 数值向量，BMI 值
#
# @examples
# calculate_bmi(70, 175)
#
# @export
calculate_bmi <- function(体重_kg, 身高_cm) {
  (体重_kg / (身高_cm / 100)^2)
}
```

---

## 🤝 协作工作流

### 代码审查流程

#### 提交 Pull Request (PR)

1. **PR 标题格式**
   ```
   [讲座编号] [功能简述]
   ```
   示例：`[Lecture-05] Implement SVM with cross-validation`

2. **PR 描述模板**
   ```markdown
   ## 描述
   [简要说明改动内容]
   
   ## 相关类型
   - [ ] 新功能
   - [ ] 修复 bug
   - [ ] 文档更新
   - [ ] 代码重构
   
   ## 测试方法
   1. 步骤 1
   2. 步骤 2
   
   ## 截图/输出（如适用）
   [附加截图或输出结果]
   
   ## 检查清单
   - [ ] 代码遵循风格指南
   - [ ] 包含注释和文档
   - [ ] 测试通过
   - [ ] 无冲突
   ```

#### 代码审查要点

审查者应检查以下方面：

- ✅ 代码风格是否遵循本指南
- ✅ 变量命名是否清晰
- ✅ 是否有不必要的循环或低效操作
- ✅ 文档是否完整
- ✅ 测试覆盖率是否充分
- ✅ 是否有安全隐患

### 测试规范

#### 单元测试

```r
# tests/test_data_validation.R
library(testthat)
source("R/data_validation.R")

test_that("validate_bmi() 正确识别异常值", {
  结果 <- validate_bmi(c(-5, 50, NA))
  expect_equal(结果$有效, c(FALSE, TRUE, FALSE))
})

test_that("calculate_bmi() 返回正确结果", {
  expect_equal(calculate_bmi(70, 175), 22.86, tolerance = 0.01)
})
```

#### 运行测试

```bash
# 运行所有测试
Rscript -e 'testthat::test_dir("tests")'

# 运行特定文件
Rscript -e 'testthat::test_file("tests/test_data_validation.R")'
```

---

## ❓ 常见问题

### Q1: 如何开始一个新的讲座项目？

**A:** 按以下步骤操作：

1. 在 `02_proj_code/` 下创建新目录：`0X_讲座名称`
2. 创建以下文件：
   - `README.md` - 讲座说明
   - `XX_demo.R` 或 `XX_demo.py` - 演示代码
   - `XX_demo.Rmd` - R Markdown 文档（可选）
   - `helpers.R` - 工具函数（如需要）
3. 按本指南撰写代码和文档
4. 提交 PR，请求代码审查

### Q2: 如何处理医学数据中的缺失值？

**A:** 推荐做法：

```r
# 识别和报告缺失模式
患者_处理 <- 患者 |>
  mutate(across(everything(), ~is.na(.), .names = "{.col}_missing")) |>
  # 根据缺失机制选择处理方法
  filter(if_all(ends_with("_missing"), ~!.))  # 完全删除

# 或使用多重填补（更推荐）
library(mice)
患者_imputed <- mice(患者, m = 5, method = "pmm")
```

### Q3: 如何为医学数据创建高质量可视化？

**A:** 使用 ggplot2 的最佳实践：

```r
患者 |>
  ggplot(aes(x = bmi_score, y = systolic_bp, color = treatment_group)) +
  geom_point(size = 3, alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE, formula = y ~ x) +
  facet_wrap(~disease_status) +
  scale_color_brewer(palette = "Set1", name = "治疗组") +
  labs(
    title = "血压与 BMI 的关系",
    x = "体质指数 (kg/m²)",
    y = "收缩压 (mmHg)",
    caption = "数据来源：2025年患者数据集"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold"),
    legend.position = "bottom"
  )
```

### Q4: 如何运行全部测试确保代码质量？

**A:** 使用以下命令：

```bash
# R 项目测试
Rscript -e 'testthat::test_dir("tests", reporter = "progress")'

# Python 项目测试
pytest tests/ -v --cov=src

# 检查代码风格
Rscript -e 'styler::style_dir("R")'
```

### Q5: 遇到 Git 冲突如何解决？

**A:** 基本步骤：

```bash
# 1. 查看冲突文件
git status

# 2. 手动编辑冲突部分（搜索 <<<<<<, ======, >>>>>> 标记）

# 3. 标记已解决
git add <resolved-file>

# 4. 提交合并
git commit -m "fix: resolve merge conflicts in lecture-05 code"
```

---

## 📚 参考资源

- **Tidyverse 风格指南**：[style.tidyverse.org](https://style.tidyverse.org)
- **mlr3 官方书籍**：[mlr3book.mlr-org.com](https://mlr3book.mlr-org.com)
- **PEP 8 Python 风格**：[python.org/dev/peps/pep-0008](https://www.python.org/dev/peps/pep-0008/)
- **R Markdown 指南**：[rmarkdown.rstudio.com](https://rmarkdown.rstudio.com)
- **Git 官方文档**：[git-scm.com/doc](https://git-scm.com/doc)

---

**最后更新**：2025-12-05  
**维护者**：2025-ML 学习班  
**版本**：v1.0
