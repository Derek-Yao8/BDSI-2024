coxfit = coxph(Surv(survival_time, death)~age_at_diagnosis+stage+
                 p_Tumor+
                 p_Cytotoxic_T+
                 p_Macrophage+
                 p_T_Helper+
                 k_Tumor+  
                 k_Cytotoxic_T+   
                 k_Macrophage+
                 k_T_Helper+
                 k_Tumor_CytotoxicT+
                 k_Tumor_Macrophage+
                 k_Tumor_THelper+
                 k_CytotoxicT_Macrophage+
                 k_CytotoxicT_THelper+
                 k_Macrophage_THelper, data=filtered_modData_ovarian)
coxfit

test.ph = cox.zph(coxfit)
test.ph
ggcox <- ggcoxzph(test.ph)
print(ggcox)
print(ggcox[[1]])
print(ggcox[[2]])
print(ggcox[[3]])
print(ggcox[[4]])
print(ggcox[[5]])
print(ggcox[[6]])
print(ggcox[[7]])
print(ggcox[[8]])
print(ggcox[[9]])
print(ggcox[[10]])
print(ggcox[[11]])
print(ggcox[[12]])
print(ggcox[[13]])
print(ggcox[[14]])
print(ggcox[[15]])
print(ggcox[[16]])