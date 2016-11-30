# bisecting-kmeans

Building:
```sh
sbt clean compile assembly
```
Running:
```sh
java -cp target/scala-2.11/*.jar me.tongfei.util.BisectingKMeans INPUTFILE OUTPUTFILE MAXCLUSTERSIZE
```
