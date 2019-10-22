import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

conversion = {
    'ns': 0.001,
    'us': 1,
    'ms': 1000,
    's': 1000 * 1000
} 

def uniform_time(v):
    ps = v.split(" ")
    return float(ps[0]) * conversion[ps[1]]
    

def main():
    df = pd.read_csv("run.log", sep=";")
    df.columns = ['Version', 'Nodes T1', 'Nodes T2', 'Depth', 'Seed', 'Time']

    df['Time'] = df['Time'].map(uniform_time)

    print(df)
    print("Time max:", df['Time'].max())
    print("Time min:", df['Time'].min())
    
    print("Nodes max:", df['Nodes'].max())
    print("Depth max:", df['Depth'].max())
    
    sns.violinplot(x="Version", y="Time", hue="Version",
                   data=df)
    sns.despine(left=True)
    plt.savefig("distribution_violin.pdf")
    
    plt.figure()
    ax = sns.swarmplot(x="Version", y="Time", hue="Depth",
                   data=df)
    ax.set_yscale("log")
    ax.get_legend().set_visible(False)
    sns.despine(left=True)
    plt.savefig("distribution_swarm.pdf")

    plt.figure()    
    sns.boxplot(x="Version", y="Time", hue="Version",
                   data=df)
    sns.despine(left=True)
    plt.savefig("distribution_boxplot.pdf")
    
    
if __name__ == '__main__':
    main()

