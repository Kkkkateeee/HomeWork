import matplotlib.pyplot as plt


ArraySystemSortRes = []
ArrayBubbleSortRes = []
ArrayQuickSortRes = []
ArrayMergeSortRes = []

MyListSystemSortRes = []
MyListBubbleSortRes = []
MyListQuickSortRes = []
MyListMergeSortRes = []


def FileRead(path, lineNumber, resArray):

    with open(path, 'r', encoding='utf-8') as file:
        for _ in range(lineNumber):
            next(file)

        for line in file:
            parts = line.strip().split('|')

            if len(parts) > 3:
                timeStr = parts[3].strip()
                timeStr = timeStr.replace(' ms', '')

                resArray.append(timeStr)

        info = file.read()

FileRead('../Benchmarks/BenchmarkDotNet.Artifacts/ArrayBubleSort.log', 1187, ArrayBubbleSortRes)
FileRead('../Benchmarks/BenchmarkDotNet.Artifacts/ArrayMergeSort.log', 1258, ArrayMergeSortRes)
FileRead('../Benchmarks/BenchmarkDotNet.Artifacts/ArrayQuickSort.log', 1733, ArrayQuickSortRes)
FileRead('../Benchmarks/BenchmarkDotNet.Artifacts/ArraySystemSort.log', 1593, ArraySystemSortRes)

FileRead('../Benchmarks/BenchmarkDotNet.Artifacts/MyListBubbleSort.log', 643, MyListBubbleSortRes)
FileRead('../Benchmarks/BenchmarkDotNet.Artifacts/MyListMergeSort.log', 1542, MyListMergeSortRes)
FileRead('../Benchmarks/BenchmarkDotNet.Artifacts/MyListQuickSort.log', 2322, MyListQuickSortRes)
FileRead('../Benchmarks/BenchmarkDotNet.Artifacts/MyListSystemSort.log', 1718, MyListSystemSortRes)

X = [10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000, 90000, 100000]
X1 = [10000, 20000, 30000, 40000, 50000, 60000, 70000, 80000]
X2 = [10000, 20000, 30000, 40000]

ArrayBubbleSortRes = [float(value.replace(',', '')) / 1000 for value in ArrayBubbleSortRes]
ArrayMergeSortRes = [float(value.replace(',', '')) / 1000 for value in ArrayMergeSortRes]
ArrayQuickSortRes = [float(value.replace(',', '')) / 1000 for value in ArrayQuickSortRes]
ArraySystemSortRes = [float(value.replace(',', '')) / 1000 for value in ArraySystemSortRes]

"""
Due to the impossibility of making full measurements, 
the MyListBubbleSortRes and MyListMergeSortRes arrays 
have trailing zeros that need to be removed
"""

MyListBubbleSortRes = [float(value.replace(',', '')) / 1000 for value in MyListBubbleSortRes]
MyListBubbleSortRes.pop()  
MyListBubbleSortRes.pop()
MyListBubbleSortRes.pop()
MyListBubbleSortRes.pop()
MyListBubbleSortRes.pop()
MyListBubbleSortRes.pop()
MyListMergeSortRes = [float(value.replace(',', '')) / 1000 for value in MyListMergeSortRes]
MyListMergeSortRes.pop()
MyListMergeSortRes.pop()
MyListQuickSortRes = [float(value.replace(',', '')) / 1000 for value in MyListQuickSortRes]
MyListSystemSortRes = [float(value.replace(',', '')) / 1000 for value in MyListSystemSortRes]

plt.figure(figsize=(10, 6))

plt.plot(X, ArrayBubbleSortRes, label='Array Bubble Sort')
plt.plot(X, ArrayMergeSortRes, label='Array Merge Sort')
plt.plot(X, ArrayQuickSortRes, label='Array Quick Sort')
plt.plot(X, ArraySystemSortRes, label='Array Systrm Sort')

# plt.plot(X2, MyListBubbleSortRes, label='MyList Bubble Sort')
# plt.plot(X1, MyListMergeSortRes, label='MyList Merge Sort')
# plt.plot(X, MyListQuickSortRes, label='MyList Quick Sort')
# plt.plot(X, MyListSystemSortRes, label='List System Sort')

# plt.plot(X, ArrayBubbleSortRes, label='Array Bubble Sort')
# plt.plot(X2, MyListBubbleSortRes, label='MyList Bubble Sort')

# plt.plot(X, ArrayMergeSortRes, label='Array Merge Sort')
# plt.plot(X1, MyListMergeSortRes, label='MyList Merge Sort')

# plt.plot(X, ArrayQuickSortRes, label='Array Quick Sort')
# plt.plot(X, MyListQuickSortRes, label='MyList Quick Sort')

plt.title('List sorts vs Array sorts performance')
plt.xlabel('number of elements')
plt.ylabel('time, sec')
plt.yscale('log')
plt.legend()
plt.savefig("Array Performance.png")
plt.show()