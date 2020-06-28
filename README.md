# TIarDict

Iaramaz Dictionary - Simple & Bad :-) separate chaining Hash Table 
with [Fastest Hash](http://www.sanmayce.com/Fastest_Hash/).

```
$ ./benchmark.exe
Key size - 253
--------
IarDict - Insert 1000000 random ...
      Ticks - 454
       Keys - 1000000
   Capacity - 1000000
Used blocks - 631957
Load factor - 1
Ticks with SetValues - 516
Ticks with free - 813
.......................................
TDictionary - Insert 1000000 random ...
Ticks - 984
Keys - 1000000
Capacity - 2097152
Ticks wiht free - 1140

....................
Integer keys test...
      Ticks - 1047
       Keys - 65536
   Capacity - 77980
Used blocks - 38147
Load factor - 0.840420620671967
Ticks with free - 1063

.................................
IarDict - Insert French words ...
Ticks - 0
Keys - 13408
Used blocks - 8787
Find insert order ...
Ticks - 0
Keys - 0
Ticks - 15
Ticks with free - 15
```
