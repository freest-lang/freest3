import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

conversion = {
    'ns': 0.001,
    'us': 1,
    'ms': 1000,
    's': 1000 * 1000
} 

MAX_TIME = 10*1000*1000

def uniform_time(v):
    if v == 'timeout':
        return MAX_TIME
    ps = v.split(" ")
    return float(ps[0]) * conversion[ps[1]]
    
columns_in_csv = ['Version', 'Nodes T1', 'Nodes T2', 'Depth', 'Seed', 'Time']

def load_dataframe(name, positive):
    df = pd.read_csv("run_{}.log".format(name), sep=";")
    df.columns = ['Version', 'Nodes T1', 'Nodes T2', 'Depth', 'Seed', 'Time']
    df['Bisimilarity'] = positive
    return df
    
def main():
    df = pd.concat([load_dataframe("positives", "Bisimilar"), 
                   load_dataframe("negatives", "Not Bisimilar")])
                   
    d = df[ df['Time'] == "timeout" ]
    print(d)

    df['Nodes'] = df['Nodes T1'] + df['Nodes T2']
    df['NodesDiff'] = np.abs(df['Nodes T1'] - df['Nodes T2'])
    df['Time'] = df['Time'].map(uniform_time)

    print("Time max:", df['Time'].max())
    print("Time min:", df['Time'].min())
    
    print("Nodes max:", df['Nodes'].max())
    print("Depth max:", df['Depth'].max())
    
    
    select = df[ df['Version'] == "B0" ]
    select2 = select[ select['Nodes'] <= 500 ]
    print("Mean:", select2['Time'].mean())
    
    for v in df['Version'].unique():
    
        select = df[ df['Version'] == v ]
    
        plt.figure(figsize=(4,4))
        markers = {"Bisimilar": "s", "Not Bisimilar": "X"}
        ax = sns.scatterplot(x="Nodes", y="Time",
                        hue="Bisimilarity",
                        #palette="ch:r=-.2,d=.3_r",
                        #hue_order=clarity_ranking,
                        #sizes=(1, 1), 
                        x_jitter=True,
                        linewidth=0,
                        style="Bisimilarity",
                        markers=markers,
                        data=select)
        ax.set_yscale('log')
        ax.set_xscale('log')
        plt.ylabel("Time (µs)")
        plt.xlabel("Total number of nodes of both types")
        plt.ylim(bottom=20, top=MAX_TIME * 1.1)
        plt.tight_layout()
        plt.savefig("nodes_time_{}.pdf".format(v))    

    plt.figure(figsize=(5,5))
    markers = {"Bisimilar": "s", "Not Bisimilar": "X"}
    ax = sns.scatterplot(x="NodesDiff", y="Time",
                    hue="Bisimilarity",
                    #palette="ch:r=-.2,d=.3_r",
                    #hue_order=clarity_ranking,
                    #sizes=(1, 1), 
                    linewidth=0,
                    style="Bisimilarity",
                    markers=markers,
                    data=df)
    ax.set_yscale('log')
    #ax.set_xscale('log')
    #plt.ylim(bottom=20, top=MAX_TIME * 1.1)
    plt.savefig("nodes_time_diff.pdf".format(v))

    select = df[ df['Version'] == "B0" ]

    plt.figure(figsize=(4,4))    
    ax = sns.boxenplot(x="Bisimilarity", y="Time",
                   data=select)
    ax.set_yscale("log")
    plt.ylabel("Time (µs)")
    sns.despine(left=True)
    plt.tight_layout()
    plt.savefig("distribution_boxplot.pdf")    
    
    


    
    
if __name__ == '__main__':
    main()

