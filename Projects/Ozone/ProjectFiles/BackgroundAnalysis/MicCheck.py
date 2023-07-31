# -*- coding: utf-8 -*-
"""
Created on Wed Jun 12 09:55:37 2019

@author: nitsu
"""
import pandas as pd
import statistics as stat
df=pd.read_csv("Ozone Data 2014.csv")
df=df[df['RH']<=100];df=df[df['WindSpeed']<=100];df=df[df['Ozone']>=0]
df.corr()
Grant=df[df['Latitude']==38.325623]
SWIC=df[df['Latitude']==38.310413]
Planetarium=df[df['Latitude']==38.632801]
#df.plot("Temp","Ozone",kind='scatter',title='Test')
#scatter(df["RH"],df["Ozone"])
#Grant.boxplot("Ozone")
#Planetarium.boxplot("Ozone")
varis= ['Ozone','Temp','Press','RH','WindSpeed','WindDir','Rainfall']
for x in varis:
    Gstat=stat.mean(Grant[x]);Sstat=stat.mean(SWIC[x]);Pstat=stat.mean(Planetarium[x])
    gsSD=((stat.stdev(Grant[x])**2/len(Grant))+(stat.stdev(SWIC[x])**2/len(SWIC)))**.5
    gpSD=((stat.stdev(Grant[x])**2/len(Grant))+(stat.stdev(Planetarium[x])**2/len(Planetarium)))**.5
    psSD=((stat.stdev(Planetarium[x])**2/len(Planetarium))+(stat.stdev(SWIC[x])**2/len(SWIC)))**.5
    gsZ=(Gstat-Sstat)/gsSD
    gpZ=(Gstat-Pstat)/gpSD
    psZ=(Pstat-Sstat)/psSD 
    if gsZ<-1.96 or gsZ>1.96:
        print('Grant and SWIC:',x)
        print(gsZ)
        print(Gstat,'to',Sstat)
        show(boxplot([Grant[x],SWIC[x]]))
        print()
    if gpZ<-1.96 or gpZ>1.96:
        print('Grant and Planetarium:',x)
        print(gpZ)
        print(Gstat,'to',Pstat)
        show(boxplot([Grant[x],Planetarium[x]]))
        print()
    if psZ<-1.96 or psZ>1.96:
        print('Planetarium and SWIC:',x)
        print(psZ)
        print(Pstat,'to',Sstat)
        show(boxplot([Planetarium[x],SWIC[x]]))
        print()
        
hist(SWIC['Ozone'])
hist(Grant['Ozone'])
NumPy