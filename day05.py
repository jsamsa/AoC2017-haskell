
def inside(idx, xs):
    return not (idx < 0 or idx >= len(xs))


testList = [0,3,0,1,-3]
count = 0
cursor = 0

f = open("day05.txt")
testList = []

for line in f:
    testList.append(int(line))
    
while (inside(cursor, testList)):
    count += 1
    offset = testList[cursor]
    if (offset > 2):
        testList[cursor] -= 1
    else:
        testList[cursor] += 1
    cursor += offset
print (count)
        
    


