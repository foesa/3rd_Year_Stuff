import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from collections import Counter

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
    # plt.show()
    u0_stats = df['Indicator1'].agg(['mean', 'std'])
    u1_stats = df['Indicator2'].agg(['mean', 'std'])
    u2_stats = df['Indicator3'].agg(['mean', 'std'])
    u3_stats = df['Indicator4'].agg(['mean', 'std'])

    p_Zn = (u0_stats['mean'] * user_0) + (u1_stats['mean'] * user_1) + (u2_stats['mean'] * user_2)
    (u3_stats['mean'] * user_3)  # current guess how to solve it
    print((p_Zn))
    print(u0_stats,'\n',u1_stats,'\n',u2_stats,'\n',u3_stats)
    return([u0_stats['mean'],u1_stats['mean'],u2_stats['mean'],u3_stats['mean']])


def stochastic_sim(means):
    order = np.random.choice(['user0','user1','user2','user3'],10000,
                     p=[0.27616531089096,0.21892050946049,0.19773040565225,0.3071837739963])
    Zlist = []
    for i in order:
        if i == 'user0':
            Zlist.append(np.random.choice([0,1],1,p=[1-means[0],means[0]])[0])
        elif i == 'user1':
            Zlist.append(np.random.choice([0, 1], 1, p=[1 - means[1], means[1]])[0])
        elif i == 'user2':
            Zlist.append(np.random.choice([0, 1], 1, p=[1 - means[2], means[2]])[0])
        elif i == 'user3':
            Zlist.append(np.random.choice([0, 1], 1, p=[1 - means[3], means[3]])[0])
    zFrame = pd.DataFrame(Zlist)
    p_Zn = zFrame[0].agg(['mean'])
    print(p_Zn['mean'])

means = graphMaker()
stochastic_sim(means)