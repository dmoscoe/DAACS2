# DAACS  
CUNY SPS MSDS capstone project: Diagnostic Assessment and Achievement of College Skills (DAACS)

### Final paper  
final paper.docx  
final paper.pdf  

### Tidying data  
generate_tidy_files_210921.R  
(Depends on:  
merge.mids.R  
prediction_functions_DMnotes.R  
roc.R)  

### Construct models  
ec_wgu_base_bin_SuperLearners.Rmd  
ec_wgu_daacs_bin_SuperLearners.Rmd  

### Generate CV.SuperLearner objects  
ec_wgu_base_bin_CVpSuperLearners.Rmd  
ec_wgu_daacs_bin_CVpSuperLearners.Rmd  

### Collect model predictions and actual values  
preds_and_actuals_211026_1324.Rmd  

### Generate metrics for each model  
*AUC, accuracy, etc.*  
binary_metrics_211027_1352.Rmd

### Are actual counts of pos acad outcomes independent of predicted counts?  
chi square test of independence positive academic outcomes.Rmd  
chi square test of independence positive academic outcomes.xlsx

### Do s's with higher/lower predicted pos outcomes actually attain more/fewer pos outcomes?  
t test difference of means across top bottom split.Rmd  