import pandas as pd
import matplotlib.pyplot as plt 
import matplotlib
import os
import datetime
#from pytz import common_timezones, all_timezones

import pandas as pd

def doubleyaxis( df,savepath ):
  dfs = pd.DataFrame(df)
  temp = dfs[["Power","Temperature"]]
  temp["Power"] =  temp["Power"]/1000 
  temp.index = pd.to_datetime(dfs["timestamp"])
  
  ax = temp.plot(secondary_y ='Temperature',mark_right=False,x_compat=True)
  ax.set_ylabel("Power (kW)",color="b")
  #ax.yaxis.label.set_color('blue')
  ax.tick_params(axis='y', colors='blue')
  ax.set_xlabel("")
  ax.right_ax.set_ylabel("Temperature (C)",color="g")
  ax.right_ax.tick_params(axis='y', colors='green')
  ax.right_ax.set_ylim(([20,35]))
  ax.set_ylim(([10,45]))
  ax.grid(True)
  #plt.show()
  fig = ax.get_figure()
  fig.savefig(savepath,bbox_inches='tight')
  return;
