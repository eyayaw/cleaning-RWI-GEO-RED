from urllib.request import urlopen
from bs4 import BeautifulSoup
import csv

# parse labor market regions (Arbeitsmarktregionen) Kosfeld Werner (2012) from the article
html = urlopen("https://link.springer.com/article/10.1007/s13147-011-0137-8#appendices")
soup = BeautifulSoup(html, 'html.parser')
table = soup.find("table", class_="data")

header = [th.text.strip() for th in table.find('thead').find_all('th')]
header = [x.lower() for x in header]

regions = []
for lmr in table.find("tbody").find_all('tr'):
    regions.append([td.text.strip() for td in lmr.find_all('td')])

# on the footnote of the table, there were (new at that time i.e., 2012) 
# districts that the authors gave provisional district codes
provis = soup.find("div", class_="c-article-table-footer").li.text.\
split(":")[1].strip().split(", ")

for v in provis:
    print(v)

# reshape to long: member districts in each labor market region come concatnated together,
# let's separate them
out = []
for row in regions:
  for d in row[2].split(","):
    out.append([*row[:-1], d.strip()])

with open('extra/Labor-Market-Regions_Kosfeld-Werner-2012.csv', 'w') as f:
    csvWriter = csv.writer(f, delimiter=",")
    for row in [*[header], *out]:
        csvWriter.writerow(row)
