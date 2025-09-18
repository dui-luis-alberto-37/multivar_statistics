setwd("/home/nightwing/Tic's/5nto/multivar_statistics/partials/1er")
source('../../mylibrary/RegresionMultiple.R')
datos = read.csv('../../data/VinoPinotNoir.csv')

datos

# 7)
m1 = lm('Calidad ~ .', datos)
m1

# 8)

R2_test(m1)
# R2 = 0.720717
# R2_ajd = 0.6666623


m2 = lm('Calidad ~ Aroma + Sabor', datos)
m2

R2_test(m2)

# R2 = 0.6585515 
# R2_ajd = 0.6390402

# Parece que quitamos algo importante

#
source('../../mylibrary/RegresionMultiple.R')
intervalos_conf_beta(m1)[,'Sabor']
intervalos_conf_beta(m2)[,'Sabor']

# izq beta_value        der       long 
# 0.538472   1.169873   1.801273   1.262801 

# izq beta_value        der       long 
# 0.5803295  1.1701664  1.7600034  1.1796738 


# en el segundo modelo hubo menos inscertidumbre
install.packages('olsrr')
library('olsrr')
ols_step_forward_aic(m1, details = T)

# Final Model Output 
# ------------------
#   
#   Model Summary                          
# ---------------------------------------------------------------
#   R                       0.839       RMSE                 1.098 
# R-Squared               0.704       MSE                  1.207 
# Adj. R-Squared          0.678       Coef. Var            9.338 
# Pred R-Squared          0.638       AIC                124.978 
# MAE                     0.868       SBC                133.166 
# ---------------------------------------------------------------
#   RMSE: Root Mean Square Error 
# MSE: Mean Square Error 
# MAE: Mean Absolute Error 
# AIC: Akaike Information Criteria 
# SBC: Schwarz Bayesian Criteria 
# 
# ANOVA                                
# -------------------------------------------------------------------
#   Sum of                                              
# Squares        DF    Mean Square      F         Sig. 
# -------------------------------------------------------------------
#   Regression    108.935         3         36.312    26.925    0.0000 
# Residual       45.853        34          1.349                     
# Total         154.788        37                                    
# -------------------------------------------------------------------
#   
#   Parameter Estimates                                    
# ----------------------------------------------------------------------------------------
#   model      Beta    Std. Error    Std. Beta      t        Sig      lower     upper 
# ----------------------------------------------------------------------------------------
#   (Intercept)     6.467         1.333                  4.852    0.000     3.759     9.176 
# Sabor     1.200         0.275        0.603     4.364    0.000     0.641     1.758 
# Fuerza    -0.602         0.264       -0.217    -2.278    0.029    -1.140    -0.065 
# Aroma     0.580         0.262        0.307     2.213    0.034     0.047     1.113 
---------------------------------------------------------------------------------------