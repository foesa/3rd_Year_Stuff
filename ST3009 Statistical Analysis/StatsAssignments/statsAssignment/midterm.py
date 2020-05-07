import random

import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from collections import Counter
# id:0.548:0.5-0.314:2-0.614:2-0
def graphMaker():
    dataFile = open("statsData", "r")
    user0Vals = []
    user1vals = []
    user2vals = []
    user3vals = []
    firstLine = True;
    for i in dataFile:
        if (not firstLine):
            vals = i.split(" ")
            user0Vals.append(int(vals[0]))
            user1vals.append(int(vals[1]))
            user2vals.append(int(vals[2]))
            user3vals.append(int(vals[3]))
        firstLine = False
    user_0 = 0.27616531089096
    user_1 = 0.21892050946049
    user_2 = 0.19773040565225
    user_3 = 0.3071837739963

    df = pd.DataFrame.from_records([user0Vals, user1vals, user2vals, user3vals])
    df = df.transpose()
    data = pd.DataFrame(df[0].value_counts())
    data.columns = ["Counts"]
    data["Prob"] = data["Counts"] / 1000

    df['Indicator1'] = 0
    df['Indicator2'] = 0
    df['Indicator3'] = 0
    df['Indicator4'] = 0

    df.loc[df[0] > 10, 'Indicator1'] = 1
    df.loc[df[1] > 10, 'Indicator2'] = 1
    df.loc[df[2] > 10, 'Indicator3'] = 1
    df.loc[df[3] > 10, 'Indicator4'] = 1

   # pmf = plt.bar(data.index.values, data["Prob"])
   # plt.xlabel("X")
   # plt.ylabel("P(X)")
  #  plt.show()
    u0_stats = df['Indicator1'].agg(['mean', 'std'])
    u1_stats = df['Indicator2'].agg(['mean', 'std'])
    u2_stats = df['Indicator3'].agg(['mean', 'std'])
    u3_stats = df['Indicator4'].agg(['mean', 'std'])

    p_Zn = (u0_stats['mean'] * user_0) + (u1_stats['mean'] * user_1) + (u2_stats['mean'] * user_2) \
           + (u3_stats['mean'] * user_3)  # current guess how to solve it
    print((p_Zn))
    print(u0_stats,'\n',u1_stats,'\n',u2_stats,'\n',u3_stats)
    return([u0_stats['mean'],u1_stats['mean'],u2_stats['mean'],u3_stats['mean']])

def stochastic_sim(means):
    order = np.random.choice(['user0','user1','user2','user3'],10000,
                     p=[0.27616531089096,0.21892050946049,
                        0.19773040565225,0.3071837739963])
    Zlist = []
    znList = []
    for s in range(1000):
        for i in order:
            if i == 'user0':
                Zlist.append(np.random.choice([0, 1], 1, p=[1 - means[0], means[0]])[0])
            elif i == 'user1':
                Zlist.append(np.random.choice([0, 1], 1, p=[1 - means[1], means[1]])[0])
            elif i == 'user2':
                Zlist.append(np.random.choice([0, 1], 1, p=[1 - means[2], means[2]])[0])
            elif i == 'user3':
                Zlist.append(np.random.choice([0, 1], 1, p=[1 - means[3], means[3]])[0])
        zFrame = pd.DataFrame(Zlist)
        znList.append(zFrame[0].agg(['mean']))
        Zlist = []
        print(s)
    print(sum(znList)/1000)

def gradient_descent():
    function = lambda x: (x**2) -1
    xvals = []
    yvals = []
    for i in range(-10,10,1):
        xvals.append(i)
    yvals = list(map(function,xvals))
    plt.plot(xvals,yvals)
    print(xvals,'\n',yvals)

    startX = 5
    learningRate = float(input('Enter Learning Rate: '))
    precision = 0.000001
    prev_size = 1
    max_iters = 1000
    cur_Iter = 0
    func = lambda x: 2*x
    derX =[]
    while prev_size > precision and cur_Iter < max_iters:
        prev_x = startX
        inter = float(func(prev_x))
        startX = startX - learningRate * float(func(prev_x))
        prev_size = abs(startX-prev_x)
        cur_Iter = cur_Iter+1
        derX.append(startX)
        print("Iteration", cur_Iter, "\nX value is", startX)

    derY = list(map(function,derX))
    plt.scatter(derX,derY,c='g')
    plt.xlabel('X')
    plt.ylabel('F(X)')
    plt.show()
    print("The local minimum occurs at", startX)

def gradient_descentRandom():
    function = lambda x: (x ** 2) - 1
    xvals = []
    yvals = []
    for i in range(-10, 10, 1):
        xvals.append(i)
    yvals = list(map(function, xvals))
    plt.plot(xvals, yvals)
    print(xvals, '\n', yvals)

    startX = 5
    precision = 0.000001
    prev_size = 1
    max_iters = 1000
    cur_Iter = 0
    derX = []
    while cur_Iter < max_iters:
        randomX = random.uniform(startX-1,startX+1)
        if function(randomX) < function(startX):
            startX = randomX
            derX.append(startX)
        cur_Iter = cur_Iter + 1
    derY = list(map(function, derX))
    plt.scatter(derX, derY, c='g')
    plt.xlabel('X')
    plt.ylabel("F(X)")
    plt.show()
    print("The local minimum occurs at", startX)


