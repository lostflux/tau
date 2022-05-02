#!/usr/bin/env python3

def count_words():
  """
    This is a simple script to count the number of words in this directory.
    It loops over all the lines in `.all` and counts the occurrence of each word,
    then sums them up. 
  """
  total = 0
  with open("all", "r") as f:
    for line in f:
      try:
        total += int(line.strip().split()[0])
      except:
        pass
    f.close()
  if total > 0:
    with open (".total", "w") as f:
      print(f"Total words = {total}")
      f.write(f"Total words = {total}")
      f.close()

def index_pages():
  """
    Generate a friendly index of the pages.
    We create a csv and a tsv (in case one proves more convenient than the other).
  """
  docID = 0

  with open("index.csv", "w") as csv, open("index.tsv", "w") as tsv:
    while True:
      try:
        with open(f"../{docID}", "r") as doc:
          title = doc.readline().strip()
          year = doc.readline().strip()
          url = doc.readline().strip()
          print(f"Indexing: {docID}")
          csv.write(f"{docID},{year},{title},{url}\n")
          tsv.write(f"{docID}\t{year}\t{title}\t{url}\n")
          doc.close()
          docID += 1
      except:
        break
  print("Done.")



if __name__ == "__main__":
  count_words()
  index_pages()
