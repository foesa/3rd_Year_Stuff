import random
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import math
def stoch_sim(n: int):
    subjectList = [1,2,3,4,5,6,7,8,9,10]
    pickedList = list(np.random.choice(subjectList,n,False))
    testList = list(np.random.choice(subjectList,3,False))
    onTest = 0
    state = 0
    for i in pickedList:
        if i in testList:
            onTest = onTest+1
        if onTest==2:
            state = 1
            return state
    return state
def dataGrab():
    q1 = []
    q2 = []
    q3 = []
    datafile = open("final2020.txt","r")
    for i in datafile:
        vals = i.split(" ")
        q1.append(int(vals[0]))
        q2.append(int(vals[1]))
        q3.append(int(vals[2]))
    df = pd.DataFrame.from_records([q1, q2, q3])
    df = df.transpose()
    data1 = pd.DataFrame(df[0].value_counts())
    data1.columns = ["Counts"]
    data1["Prob"] = data1["Counts"] / 1000
    data2 = pd.DataFrame(df[1].value_counts())
    data2.columns = ["Counts"]
    data2["Prob"] = data2["Counts"] / 1000
    data3 = pd.DataFrame(df[2].value_counts())
    data3.columns = ["Counts"]
    data3["Prob"] = data3["Counts"] / 1000
    print(data1,"\n \n",data2,"\n \n",data3)
    return ((q1,data1),(q2,data2),(q3,data3))
def pmf_make(data):
    pmf = plt.bar(data.index.values, data["Prob"])
    plt.xlabel("X")
    plt.ylabel("P(X)")
    plt.show()
def stoch_alter(n,prevTest,probs):
    subjectList = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    alterList = []
    for i in subjectList:
        if i not in prevTest:
            alterList.append(i)
    pickedList = list(np.random.choice(subjectList, n, False,probs))
    testList = list(np.random.choice(subjectList, 3, False,probs))
    onTest = 0
    state = 0
    alterWay = []
    if n <= 3:
        alterWay = list(np.random.choice(prevTest,n,False))
    else:
        alterWay= list(np.random.choice(prevTest,3,False))
        alterWay.append(np.random.choice(alterList,n-3,False))
    for i in pickedList:
        if i in testList:
            onTest = onTest + 1
        if onTest == 2:
            state = 1
            return state
    return state, testList
def main():
    results = [0,0,0,0,0,0,0,0,0,0]
    specRes = []
    for i in range(1,11):
        for s in range(10000):
            val = stoch_sim(i)
            if val == 1:
                results[i-1] = results[i-1]+1
            if i == 7:
                specRes.append(val)
    print(specRes)
    for i in range(len(specRes)):
        specRes[i] = (specRes[i]-(49/60))**2
    total = sum(specRes)
    print(total/10000)
def probalter(prob,s,prevTest):
    total = 0
    for i in prevTest:
        prev = prob[i-1]
        prob[i-1] = prob[i-1] * s
        total = total + (prob[i-1] - prev)
    dec = total/7
    for i in range(len(prob)):
        if i+1 not in prevTest:
            prob[i] = prob[i]-dec
    print(sum(prob))
    return prob
def meVar(dicter):
    means = []
    vars = []
    for item in dicter.items():
        mean = sum(item[1])/len(item[1])
        var = 0
        for items in item[1]:
            cur = (items-mean)**2
            var = cur + var
        var = var/len(item[1])
        means.append((item[0],mean))
        vars.append(var)
    return means,vars
def errorBars(means,vars,m2,v2):
    plt.clf()
    ms = [i[0] for i in means]
    nms = [int(i[1]) for i in means]
    ms2 = [i[0] for i in m2]
    nms2 = [int(i[1]) for i in m2]
    plt.errorbar(ms2,nms2,v2,fmt='ro',ecolor='green',capsize=5,barsabove=False,label="Q3")
    #plt.errorbar(ms, nms, vars, fmt='o', ecolor='black', capsize=5, barsabove=False,label="Q2")
    plt.xlabel("Test score Q1")
    plt.ylabel("Mean Q3 ")
    plt.title("Q3 conditioned on Question 1")
    plt.legend()
    plt.show()
def fails():
    vals = []
    for i in range(11):
        val = (math.comb((10-i,4))/math.comb(10,4)) + ((i*(math.comb(10-i,3)))/math.comb(10,4))
        vals.append((i,val))
if __name__ == '__main__':
    init_probs = [.1, .1, .1, .1, .1, .1, .1, .1, .1, .1]
    q1,q2,q3 = dataGrab()
    bin2 = {}
    bin3 = {}
    for i in range(len(q1[0])):
        if q1[0][i] in bin2:
            bin2[q1[0][i]] = bin2[q1[0][i]] + [q2[0][i]]
            bin3[q1[0][i]] = bin3[q1[0][i]] + [q3[0][i]]
        else:
            bin2[q1[0][i]] = [q2[0][i]]
            bin3[q1[0][i]] = [q3[0][i]]
    means,vars = meVar(bin2)
    m2,var2 = meVar(bin3)
    vars = list(map(lambda x: (math.sqrt(x)),vars))
    var2 = list(map(lambda x: (math.sqrt(x)),var2))
    f1 = pd.DataFrame.from_records(means,columns=['Q1 mean','Q2 mean'])
    f2 = pd.DataFrame.from_records(m2,columns=['Q1 mean','Q2 mean'])
    f1['Variance'] = vars
    f2['Variance'] = var2
    print(f1)
    print(f2)
    print(m2,var2)
    errorBars(means,vars,m2,var2)