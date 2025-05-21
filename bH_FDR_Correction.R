#Adjust P-values for SZ and EA PGS using Benjamini-Hochberg procedure.
#Raw P-values from primary linear mixed effects models. 

p_sz_admit <-#
p_sz_discharge<-#
p_sz_change<-#

p_ea_admit<-#
p_ea_discharge<-#
p_ea_change<-#

all_pvals <- c(p_sz_admit, p_sz_discharge, p_sz_change,
               p_ea_admit, p_ea_discharge, p_ea_change)

p.adjust(all_pvals, method = "BH")


