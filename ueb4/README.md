# main memory map/reduce with mpi (c++)

Die Eingabe für den Map/Reduce-Process sind die Dateinamen der Dateien, deren
Word-Count berechnet werden soll. Die Daten sollen im Reduce-Schritt auf den
verschiedenen Prozessen zusammengeführt werden.

Allen Prozessen stehen durch MPI die Eingabeparameter zur Verfügung. Jeder
Prozess wählt sich aus den vorhandenen Dateinahmen anhand seiner Prozess-Id
Dateien für seinen Map-Schritt aus. Die Dateien werden der Reihe nach auf die
Prozesse verteilt. Wir beginnen bei dem Prozess mit der Id 0 und verteilen
aufsteigend weiter. Gibt es mehr Dateien als Prozesse, werden die überschüssigen
Dateien wieder bei Prozess 0 beginnend weiterverteilt (Modulo Anzahl Prozesse).

Jeder Prozess zählt im Map-Schritt die Anzahl der Wörter in seinen Dateien und
führt die Ergebnisse lokal zusammen.

Im Reduce-Schritt werden die Daten auf die Combiner verteilt. Jeder Prozess
ermittelt per Hash-Funktion welcher Prozess für welches seiner Wörter
verantwortlich ist. Wurden alle Daten aufgeteilt, sendet jeder Prozess die Daten
an die entsprechenden Threads. Alle vorhanden Prozesse nehmen am Combining teil.
Auch Prozesse, die keine Wörter gezählt haben. Insgesamt werden n \* n - 1 viele
Ergebnisse verschickt unter den Prozessen.a

Sobald ein Prozess alle Daten erhalten hat und sie mit seinen lokalen Daten
zusammengeführt hat, werden die Daten auf die Festplatte geschrieben und der
Reduce-Schritt wird damit beendet.

Unklar ist, warum für die Zuordnung der Dateinamen zu Prozessen eine Hash-Funktion
verwendet werden sollte. Gehen wir davon aus, dass ein Word-Count nur einmal
ausgeführt wird, ist es unerheblich, ob wir eine Hash-Funktion benutzen oder nicht.
Werden mehrere Word-Counts gleichzeitig ausgeführt, könnte ein Hashing der Dateinamen
für eine gleichmäßigere Auslastung der einzelnen Prozesse sorgen. Die Größe der Dateien
ist jedoch der eigentliche Faktor für die Berechnungszeit, das Problem nicht wirklich löst.
