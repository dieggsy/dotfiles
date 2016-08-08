import sqlite3
import csv


def progBar(i, total, length=50, kind=None):
    i += 1
    n = int(i*length/total)
    percent = i/total*100
    frame = ("{0:6.2f}% |{1}{2}|".format(percent, '█'*n, ' '*(length-n)) if kind is None else
        "{0:6.2f}% [{1}{2}]".format(percent, str(kind)*n, ' '*(length-n)))
    endchar = ('\r' if i < total else ' Done!\n')
    print(frame, end=endchar)

# a comment
    
    
def write_csv(path, rows):
    with open(path, 'w') as f:
        writer = csv.writer(f)
        writer.writerows(rows)
        
