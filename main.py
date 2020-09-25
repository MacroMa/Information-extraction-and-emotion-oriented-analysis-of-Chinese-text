import numpy as np
import synonyms
import pandas as pd
from snownlp import SnowNLP
# import csv
# import sklearn
# from sklearn.preprocessing import PolynomialFeatures
# from sklearn.linear_model import LinearRegression
# import matplotlib.pyplot as plt
# import matplotlib.ticker as ticker
# import matplotlib as mpl
# from scipy import interpolate
# from statsmodels.tsa.ar_model import AR

data_long = np.loadtxt("data_test1.csv",str,delimiter=",", skiprows=1)
data_long

("国际劳工组织")
cixing=[]
for i in range(0,len(data_long)):
    cixing.append(synonyms.seg(data_long[i]))




test=synonyms.nearby("人脸")
test[0]
print("识别: %s" % (synonyms.nearby("识别")))
print("NOT_EXIST: %s" % (synonyms.nearby("NOT_EXIST")))
synonyms.display("金融")
synonyms.v()
print(1)


cixiangliang=[]
for i in range(0,len(data_long)):
    try:
        cixiangliang.append(synonyms.v (data_long[i]))
    except:
        cixiangliang.append(-1)



ciqinggan=[]
for i in range(0,len(data_long)):
    s = SnowNLP(data_long[i])
    ciqinggan.append(s.sentiments)

test=pd.DataFrame(data=cixing)
test.to_csv('cixing.csv')

test=pd.DataFrame(data=ciqinggan)
test.to_csv('ciqinggan.csv')

test=pd.DataFrame(data=cixiangliang)
test.to_csv('cixiangliang.csv')

cixiangliang_shape=[]
for i in range(0,len(data_long)):
    if isinstance(cixiangliang[i],int):
        test=-1
    else:
        test=[]
        for j in range(0,len(cixiangliang[i])):
            test.append(cixiangliang[i][j])
    cixiangliang_shape.append(test)

test=pd.DataFrame(data=cixiangliang_shape)
test.to_csv('cixiangliang_shape.csv')



data2=np.loadtxt("data_test2.csv",str,delimiter=",", skiprows=1)
data2
ciqinggan2=[]
for i in range(0,len(data2)):
    s = SnowNLP(data2[i])
    ciqinggan2.append(s.sentiments)

test=pd.DataFrame(data=ciqinggan2)
test.to_csv('juziqinggan.csv')

test=pd.DataFrame(data=data2)
test.to_csv('data2.csv',encoding="GBK")