library(RODBC)
library(tidyverse)
osteo=odbcConnectAccess2007("osteoporosis.accdb")
sqlTables(osteo)$TABLE_NAME

bmd=sqlFetch(osteo,"BMD")
data1=sqlFetch(osteo,"T_TTLDATA")
data2=sqlFetch(osteo,"T_’ñ‹Ÿ")
basic=sqlFetch(osteo,"Š³ŽÒŠî–{î•ñ")
oldlab=sqlFetch(osteo,"ÌŒŒEÌ”Aƒf[ƒ^")
newlab=sqlFetch(osteo,"ÌŒŒEÌ”Aƒf[ƒ^iVj")
drugday=sqlFetch(osteo,"–òÜ“Š—^—ð")
drugname=sqlFetch(osteo,"–òÜ–¼")

dis=data1%>%select(starts_with('Total'))%>%distinct()%>%drop_na()
dis=c(t(dis))
dis
newlab%>%select(starts_with('Int'))%>%filter_all(any_vars(.%in%dis))
newlab=newlab%>%mutate_all(as.character)%>%mutate_all(na_if,'')

drug1=drugday%>%filter(–òÜ==1)%>%filter(“Š—^“ú”>=365)%>%
         group_by(ID)%>%arrange(ID,“Š—^“ú”)%>%slice(n())%>%
         select(2,3,4,5,7)
drug2=drugday%>%filter(–òÜ==2)%>%filter(“Š—^“ú”>=365)%>%
         group_by(ID)%>%arrange(ID,“Š—^“ú”)%>%slice(n())%>%
         select(2,3,4,5,7)

join%>%filter_all(any_vars(!ID%in%full$ID))
dataset1=dataset%>%filter_all(any_vars(ID%in%drug1$ID))
dataset2=dataset%>%filter_all(any_vars(ID%in%drug2$ID))
dataset1=dataset1%>%mutate(EDIROL='yes')
dataset2=dataset2%>%mutate(ALFAROL='yes')
dataset12=list(dataset,dataset1,dataset2)%>%reduce(full_join)
dataset12=dataset12%>%replace_na(list(EDIROL='no',ALFAROL='no'))
