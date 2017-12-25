import lxml
from lxml import html
import requests

import pprint as pp

url = 'https://coinmarketcap.com/all/views/all/'
f = requests.get(url)

html_page = f.content
tree = html.fromstring(html_page)

#g = tree.xpath('//div[@id="currencies-all_wrapper"]//')
thead = tree.xpath('//thead')

data = {} 
col_names = []
for i1, e1 in enumerate(thead): #thead
   for i2, e2 in enumerate(e1):  #tr
      for i3, e3 in enumerate(e2): #td
         col = e3.text
         col_names.append(col)
         data[col] = []

tbody = tree.xpath('//tbody')

numeric_list = ['Market Cap', 'Price', 'Circulating Supply', 'Volume (24h)', '% 1h', '% 24h', '% 7d']

for i1, e1 in enumerate(tbody): #tbody
   for i2, e2 in enumerate(e1):   #tr
      for i3, e3 in enumerate(e2): #td
         if len(e3) == 0:
            s = str(e3.text).strip()
            if col_names[i3] in numeric_list:
               s = "".join(s.split("$"))
               s = "".join(s.split(","))
               s = "".join(s.split("%"))
               s = "".join(s.split("?"))
            
               if s == "":
                  s = -100000000
               else:
                  s = float(s)
               
            data[col_names[i3]].append(s)
         else:
            for i4, e4 in enumerate(e3):
                s = str(e4.text).strip()

                if col_names[i3] == 'Name':
                    if s == 'None':
                        continue

                if col_names[i3] in numeric_list:
                    s = "".join(s.split("$"))
                    s = "".join(s.split(","))
                    s = "".join(s.split("%"))
                    s = "".join(s.split("?"))
            
                    if s == "" or s == "Low Vol":
                        s = -100000000
                    else:
                        s = float(s)
               
                data[col_names[i3]].append(s)

#print data
sorted_list_with_vol = list(reversed([i[0] for i in sorted(enumerate(data['Volume (24h)']), key=lambda x:x[1])]))
sorted_list_with_7d = list(reversed([i[0] for i in sorted(enumerate(data['% 7d']), key=lambda x:x[1])]))

ix = 1
for i in sorted_list_with_7d:
    mc = data['Market Cap'][i]
    p = data['Price'][i]
    sevD = data['% 7d'][i]
    if mc > 30000000 and sevD > 0:
       if p < 20:
            print ix, "", data['Name'][i], " Price = ", p, " Market Cap = ", mc, " %7d = ", sevD
            ix += 1

