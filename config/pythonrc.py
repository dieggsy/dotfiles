# encoding: utf-8


def progBar(i, total, length=50, kind=None):
    """A nice progress bar to use with for loops."""
    i += 1
    n = int(i*length/total)
    percent = i/total*100
    frame = ("{0:6.2f}% |{1}{2}|".format(percent, '█'*n, ' '*(length-n))
             if kind is None else
             "{0:6.2f}% [{1}{2}]".format(percent, str(kind)*n, ' '*(length-n)))
    endchar = ('\r' if i < total else ' Done!\n')
    print(frame, end=endchar)
    
    
# def write_csv(path, rows):
#     "Write a list of iterables to a CSV, I think"
#     with open(path, 'w') as f:
#         writer = csv.writer(f)
#         writer.writerows(rows)
        
