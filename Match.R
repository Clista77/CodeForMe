srl1=read.csv('SRL1.csv')
srl2=read.csv('SRL2.csv')
edc=read.csv('EDC.csv')
srl1.distinct=srl1 %>% distinct(DHLA, AA, EPA, DHA,.keep_all=T)
srl2.distinct=srl2 %>% distinct(DHLA, AA, EPA, DHA,.keep_all=T)
srl12.inner=inner_join(srl1,srl2,by='ID')
dim(srl12.inner)
dim(srl12 %>% distinct(DHLA, AA, EPA, DHA))
dim(srl1)
dim(srl2)
dim(edc)
srl12=union(srl1,srl2)
dim(srl12)

dim(srl1 %>% distinct(DHLA, AA, EPA, DHA))
dim(srl2 %>% distinct(DHLA, AA, EPA, DHA))
dim(srl12 %>% distinct(DHLA, AA, EPA, DHA))
dim(intersect(srl1,srl2))

srl3=inner_join(srl1,srl2,by=c('DHLA', 'AA', 'EPA', 'DHA'))

edc=read.csv('EDC.csv')
edc11=edc1[,-1]
inter1=intersect(srl11,edc11)