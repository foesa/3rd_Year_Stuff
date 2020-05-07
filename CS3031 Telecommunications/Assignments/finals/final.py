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
def stoch_alter(n,probs,testList):
    subjectList = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    pickedList = list(np.random.choice(subjectList, n, False,probs))
    onTest = 0
    state = 0
    for i in pickedList:
        if i in testList:
            onTest = onTest + 1
        if onTest == 2:
            state = 1
            return state,testList
    return state, testList
def stoch_alter2(n,prevTest,probs,testList):
    subjectList = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    alterList = []
    for i in subjectList:
        if i not in prevTest:
            alterList.append(i)
    alterWay = []
    if n <= 3:
        alterWay = list(np.random.choice(prevTest, n, False))
    else:
        alterWay = list(np.random.choice(prevTest, 3, False))
        alterWay =  alterWay + (list(np.random.choice(alterList, n - 3, False)))
    onTest = 0
    state = 0
    for i in alterWay:
        if i in testList:
            onTest = onTest + 1
        if onTest == 2:
            state = 1
            return state,testList
    return state, testList
def main():
    results = [0,0,0,0,0,0,0,0,0,0]
    specRes = []
    means = []
    for i in range(100):
        specRes = []
        for s in range(10000):
            val = stoch_sim(7)
            specRes.append(val)
        if(len(specRes) != 0):
            mean = sum(specRes) / len(specRes)
            means.append(mean)
    print(len(means))
    plt.scatter(means,np.arange(0,100,1))
    plt.axvline(x=.8096,label = "x = .8096")
    plt.axvline(x=.8250, label = "x = .8250")
    plt.axvline(x=0.8173, color="r",label = "mean = .8173")
    plt.xlabel("Mean value")
    plt.title("Means vs 95% confidence intervals")
    plt.legend()
    plt.show()
    for i in range(len(specRes)):
        specRes[i] = (specRes[i]-(49/60))**2
    total = sum(specRes)
    print(total/10000)
    print(mean)
def probalter(prob,s,prevTest):
    total = 0
    for i in prevTest:
        prev = prob[i-1]
        prob[i-1] = prob[i-1] * s
        total = total + (prob[i-1] - prev)
    dec = total/7
    for i in range(len(prob)):
        if i+1 not in prevTest:
            prob[i] = prob[i] - dec
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
    plt.errorbar(ms, nms, vars, fmt='o', ecolor='black', capsize=5, barsabove=False,label="Q2")
    plt.xlabel("Test score Q1")
    plt.ylabel("Mean Q2")
    plt.title("Q2 v Q3 conditioned on Question 1")
    plt.legend()
    plt.show()
def fails():
    vals = []
    v2 = []
    for i in range(11):
        val = (math.comb(10-i,4)/math.comb(10,4)) + ((i*(math.comb(10-i,3)))/math.comb(10,4))
        vals.append((i,val))
    for i in range(11):
        val = (math.comb(10 - i, 3) / math.comb(10, 3)) + ((i * (math.comb(10 - i, 2))) / math.comb(10, 3))
        v2.append((i, val))
    print(vals)
    print(v2)
    plt.bar([i[0] for i in v2], [i[1] for i in v2], ecolor="green",label="3 subjects")
    plt.bar([i[0] for i in vals],[i[1] for i in vals],ecolor="black",label="4 subjects")
    plt.xlabel("N - number of studied subjects")
    plt.ylabel("Chance of Failure")
    plt.title("N vs Failure chance")
    plt.xticks(np.arange(0,11,1))
    plt.yticks(np.arange(0.0,1.05,.05))
    plt.legend()
    plt.show()
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
    print(means)
    m2,var2 = meVar(bin3)
    vars = list(map(lambda x: (math.sqrt(x)),vars))
    var2 = list(map(lambda x: (math.sqrt(x)),var2))
    '''way1 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    way2 = [0, 0, 0, 0, 0, 0, 0, 0, 0, 0]
    init_probs = [.1, .1, .1, .1, .1, .1, .1, .1, .1, .1]
    subjectList = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
    n_probs = init_probs
    pTest = list(np.random.choice(subjectList,3,False))
    for j in range(100):
        init_probs = [.1, .1, .1, .1, .1, .1, .1, .1, .1, .1]
        n_probs = init_probs
        for i in range(10):
            testList = list(np.random.choice(subjectList, 3, False))
            for s in range(1, 11):
                s1, t1 = stoch_alter(s, n_probs, testList)
                s2, t2 = stoch_alter2(s, pTest, n_probs, testList)
                way1[s - 1] = way1[s - 1] + s1
                way2[s - 1] = way2[s - 1] + s2
            pTest = testList
            n_probs = probalter(n_probs, 1.15, pTest)
        print(way1, way2)
    numbers = [1,2,3,4,5,6,7,8,9,10]
    way1 = list(map(lambda x: x/1000,way1))
    way2 = list(map(lambda x: x/1000,way2))
    plt.bar(numbers, way2, color="r")
    plt.bar(numbers, way1)
    plt.xlabel("Subjects studied")
    plt.ylabel("Pass Rate")
    plt.show()'''