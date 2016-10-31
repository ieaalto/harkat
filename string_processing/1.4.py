"""
Output:
['the', 'and', 'of', 'to', 'a', 'in', 'he', 'his', 'that', 'was']
"""


occurrences = {}

with open('english.50MB') as f:
    for line in f:
        words = line.split()
        for word in words:
            word.strip()
            word.strip('!.:,;?')
            if word != '':
                occurrences[word] = occurrences[word] + 1 if word in occurrences else 0

entries = list(occurrences.keys())
entries.sort(key=lambda x: -occurrences[x])

print(entries[:10])



