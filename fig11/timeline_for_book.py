import argparse

parser = argparse.ArgumentParser(description='Provide paths for the files.')
parser.add_argument('-f', type=str, help='Path to the first file')

args = parser.parse_args()

file1_path = args.f

from datetime import date, timedelta, datetime
from matplotlib.backends.backend_agg import RendererAgg
from matplotlib.figure import Figure
from pandas import json_normalize
from PIL import Image
import matplotlib
import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import requests

# Timeline
chartdata = pd.read_csv(file1_path)  # Use the provided path
chartdata = chartdata.query('Year > 1945')
dates = pd.to_datetime(chartdata[['Year', 'Month', 'Day']])
min_date = date(np.min(dates).year - 2, np.min(dates).month, np.min(dates).day)
max_date = date(np.max(dates).year + 2, np.max(dates).month, np.max(dates).day)

# Fake date
fake_d = np.c_[0:len(dates)]

labels = chartdata['Name']
labels = ['{0:%d %b %Y}:\n{1}'.format(d, l) for l, d in zip(labels, dates)]

fig, ax = plt.subplots(figsize=(10, 28))
_ = ax.set_xlim(-25, 25)
_ = ax.set_ylim(0, 98)
_ = ax.axvline(0, ymin=0.05, ymax=.985, c='deeppink', zorder=1)

_ = ax.scatter(np.zeros(len(fake_d)), fake_d, s=120, c='palevioletred', zorder=2)
_ = ax.scatter(np.zeros(len(fake_d)), fake_d, s=30, c='darkmagenta', zorder=3)

#label_offsets = np.repeat(2.0, len(dates))
label_offsets = np.repeat(2.0, len(fake_d))
label_offsets[1::2] = -2.0
import textwrap
def wrap_text(text, max_length):
    return '\n'.join(textwrap.wrap(text, width=max_length))

for i, (l, d) in enumerate(zip(labels, fake_d)):
    align = 'right'
    if i % 2 == 0:
        align = 'left'
    wrapped_label = wrap_text(l, 60)  # Adjust the line length as needed
    _ = ax.text(label_offsets[i], d, wrapped_label, ha=align, fontfamily='serif',
                fontweight='bold', color='royalblue', fontsize=11)

stems = np.repeat(2.0, len(fake_d))
stems[1::2] *= -1.0
x = ax.hlines(fake_d, 0, stems, color='darkmagenta')

# hide lines around chart
for spine in ["left", "top", "right", "bottom"]:
    _ = ax.spines[spine].set_visible(False)

_ = ax.set_xticks([])
_ = ax.set_yticks([])
_ = ax.set_title('Figure 11: Selected UAP Related Milestones, 1946 - 2023',
                 fontweight="bold",
                 fontfamily='sans-serif',
                 fontsize=22,
                 color='darkgreen',
                 pad=35)

ax.annotate('© 2023 Jason Miller', xy=(-25, 0))
fig.tight_layout()
fig.savefig("matplotlib_tmp.jpg")

plt.show()
