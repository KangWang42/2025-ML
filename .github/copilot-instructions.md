# Copilot Instructions

## Repository Snapshot
- Course repository for Sun Yat-sen University ML study group; code lives under `02_proj_code`, reference PDFs under `01_ref_book`. Most directories map 1:1 with lecture numbers.
- Scripts are per-lecture demonstrators, not a unified package. Expect heterogeneous styles (base R, tidyverse, mlr3) and duplicated setup code.
- Python notebooks exist, but R examples are primary. Be cautious editing `.ipynb`; prefer leaving them untouched unless explicitly asked.
- Large binaries (PDF, PPTX, ZIP) are tracked intentionally. Avoid modifying or adding heavy assets unless requested.

## Key Code Areas
- `04_决策树和随机森林/决策树(最新版）.R` shows the expected flow: download UCI data, factor conversion, `set.seed(1234)`, `rpart` modeling, ROC plots via `pROC`.
- `03_机器学习评价体系/第三讲-交叉验证(1).R` demonstrates evaluation workflows with `caret::train`, repeated CV, LOOCV, and bootstrap sampling; mirrors how new evaluation scripts should look.
- `06_支持向量机与核方法/svm.R` is the SVM template: use `e1071::tune.svm`, compute `confusionMatrix`, collect metrics into a data frame, and render plots with `ggplot2`.
- `07_无监督学习/01 code.RMD` is the Quarto pattern: tidyverse pipelines, `mlr3verse` tasks, `factoextra` visualizations, and commentary in Chinese. Follow chunk labels and keep `library()` calls grouped at top of sections.

## Coding Conventions
- Stick with R 4.5 idioms. If adding new scripts, mirror existing random seeds (`set.seed(123 or 1234)`), explicit factor level labels, and Chinese comments.
- Keep `install.packages()` calls commented or moved to setup notes unless absolutely required; most scripts already assume packages are preinstalled.
- Favor tidyverse pipes `|>`/`%>%` plus explicit `library()` imports at top. When extending mlr3 content, use `as_task_*`, `lrn()`, `tune()`, and `msrs()` as shown in `01 code.RMD`.
- When reporting metrics, construct small tibbles (see `svm.R`) instead of printing raw objects. Avoid `print()/cat()` in new code unless replicating didactic console output.

## Data & External Resources
- Many examples fetch open datasets (`mlbench::PimaIndiansDiabetes`, UCI CSV URLs). Preserve those sources and explain any replacements inline.
- Local CSV/TXT files under `01_ref_book/09_others/**` are lecture datasets. Reference them relatively and avoid relocating.
- Visuals generated inside scripts rely on `ggplot2`, `factoextra`, `gridExtra`, `umap`, and `pROC`. Keep plotting calls lightweight and reproducible.

## Workflow Expectations
- There is no central build or test harness. Validate new R scripts interactively via the R console; note verification steps in PR descriptions if tests are manual.
- Quarto documents target PDF output with XeLaTeX. If you add chunks, ensure they knit without requiring internet access beyond the existing dataset downloads.
- Keep filenames bilingual (English + Chinese) consistent with existing lecture folders; do not rename without instructor approval.
- Before committing, check for accidental modifications to binary course materials and avoid adding rendered outputs (PDF/PNG) unless asked.

## Getting Started Quickly
- For supervised examples, copy the pattern from `决策树(最新版）.R` or `svm.R`: load data → preprocess factors → split with `caret::createDataPartition` → fit model → gather metrics/plots.
- For unsupervised or mlr3 workflows, start from `07_无监督学习/01 code.RMD` and adjust tasks/learners while preserving chunk structure and narrative commentary.
- Use `setwd()` sparingly; rely on relative paths from repo root when reading/writing files.
- Document any non-trivial steps in Chinese to match existing materials, keeping explanations concise.
