<a rel="license" href="http://creativecommons.org/licenses/by-nc/3.0/ch/"><img alt="Creative Commons Lizenzvertrag" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/3.0/ch/88x31.png" /></a><br/>

# Eine Analyse des Schweizer Poststellennetzes
Gemäss Art. 33 Abs. 4 der Postverordnung ([VPG](https://www.admin.ch/opc/de/classified-compilation/20112357/index.html#a36)) muss die Post *"...gewährleisten, dass 90 Prozent der ständigen Wohnbevölkerung zu Fuss oder mit öffentlichen Verkehrsmitteln eine Poststelle oder Postagentur innerhalb von 20 Minuten erreichen können. Bietet die Post einen Hausservice an, so gelten für die betroffenen Haushalte 30 Minuten.*" 

**Erfüllt das heutige Poststellennetz diese Anforderungen?** – Dieser Frage sind wir mit einem datengetriebenen Ansatz und viel *Open Data* nachgegangen.

## Vorgehen
Der zur Analyse verwendete R-Code ist unter [post_erreichbarkeit.R](https://github.com/gfzb/DiePost/blob/master/post_erreichbarkeit.R) abgelegt. Das grobe Vorgehen wird nachfolgend kurz skizziert.


**A. Aufbereiten Geodaten der Schweizer Poststellen, Postagenturen und Hausservices**

Datensatz: [Zugangspunkte Post](https://swisspost.opendatasoft.com/explore/dataset/zugangspunkte-post/) aus dem [Open Data Portal](https://swisspost.opendatasoft.com/pages/home/) der Post
>Der Datensatz umfasst Angebots- und Geodaten zu allen Postservices innerhalb der Schweiz. Zur Überprüfung der Erreichbarkeitsauflagen werden aus dem Datensatz die beiden Servicetypen *Filiale* (Poststellen und Agenturen) und *Hausservice* verwendet (Total 3498 Angebote). Da das Thema des Poststellenabbaus seit einigen Jahren immer wieder (vgl. dazu u.a. [SRF vom 28.11.2017](https://www.srf.ch/news/schweiz/service-abbau-poststellen-schwund-der-kampf-der-landbevoelkerung) und [NZZ vom 30.11.2017](https://www.nzz.ch/schweiz/staenderat-will-moeglichst-keine-veraenderungen-bei-den-poststellen-ld.1334240)) für Gesprächsstoff sorgt, wird versucht, auf Basis der Angebotsdaten zusätzlich zwischen *traditionellen Poststellen* und *Postagenturen* zu unterscheiden.

**B. Aufbereiten der Schweizer Postadressen**

Datensätze: [Hausnummer und Hauskey](https://swisspost.opendatasoft.com/explore/dataset/hausnummer-und-hauskey/), [Strassenbezeichnungen](https://swisspost.opendatasoft.com/explore/dataset/strassenbezeichnungen/), [PLZ-Verzeichnis](https://swisspost.opendatasoft.com/explore/dataset/plz-verzeichnis/), [politische Gemeinden](https://swisspost.opendatasoft.com/explore/dataset/politische-gemeinden/) aus dem [Open Data Portal](https://swisspost.opendatasoft.com/pages/home/) der Post
>Durch Mergen der vier Open-Data-Datensätze der Post lassen sich rund 1,8 Mio. Schweizer Adressen generieren. Diese dienen als Ausgangspunkt für weitere Analysen. Ob es sich dabei wirklich um *sämtliche* Schweizer Postadressen handelt, kann *nicht* beurteilt werden.

**C. Ziehen einer gewichteten Zufallsstichprobe von Schweizer Postadressen**

Datensatz: [Ständige Wohnbevölkerung der Schweizer Gemeinden](https://www.pxweb.bfs.admin.ch/pxweb/de/px-x-0102010000_101/px-x-0102010000_101/px-x-0102010000_101.px) vom Bundesamt für Statistik ([BFS](https://www.bfs.admin.ch/bfs/de/home/statistiken/bevoelkerung.html))
>Um im weiteren Verlauf den Aufwand in Grenzen halten zu können, soll nur ein Teil der Postadressen weiterverwendet werden. Da anzunehmen ist, dass die Anzahl Personen pro Postadresse von Siedlungstyp und Wohnstruktur abhängig ist und sich diese in der Schweiz regional unterscheiden, wird, um die Wohnbevölkerung möglichst gut abzubilden, für jede Gemeinde die mittlere Anzahl Personen pro Postadresse berechnet. Diese Masszahl fliesst beim Ziehen der Zufallsstichprobe als Gewichtungsfaktor ein. Die Stichprobe umfasst 2'500 der rund 1,8 Mio. Postadressen.

**D. Berechnen der Entfernungen zwischen sämtlichen Adressen und Postangeboten**

Link: [Google Maps Geocoding API](https://developers.google.com/maps/documentation/geocoding/intro?hl=en)
>Mit Hilfe der API von Google Maps werden alle Postadressen geocodiert (ca. 1,4 Prozent müssen von Hand nachcodiert werden). Da nun die Koordinaten von Start und Ziel bekannt sind, können die Distanzen zwischen allen Adressen und Postangeboten berechnet werden. Diese dienen jedoch nur der Vorselektion der für die Simulation der Reisezeiten relevanten Poststellen.

**F. Eruieren der Wegzeit zwischen jeder Adresse und den fünf nächstgelegenen Postangeboten**

Link: [Google Maps Distance Matrix API](https://developers.google.com/maps/documentation/distance-matrix/intro?hl=en)
>Für jede Adresse wird nun mit Hilfe der Distance Matrix API von Google Maps eruiert, wie lange man zu Fuss und mit dem öffentlichen Verkehr zu den fünf nächstgelegenen Postangeboten unterwegs ist. Für weitere Analysen werden zudem die Fahrzeiten mit Velo und Auto abgefragt. Die Reisezeiten wurden für Dienstag, den 05. Dezember 2018, um 9:30 Uhr 'simuliert'. Es werden die Standardeinstellungen der API verwendet.

## Resultate
### Erreichbarkeit von Poststellen und Postagenturen
Die grafische Aufbereitung und Auswertung der Daten lässt vermuten, dass die Post heute ihre bundesrätlichen Erreichbarkeitsvorgaben weitgehend erfüllt. So erreichen gut 90 Prozent der ständigen Wohnbevölkerung zu Fuss oder mit öffentlichen Verkehrsmitteln eine Poststelle oder Postagentur in mindestens **20 Minuten**. Rund die Hälfte der Schweizer Haushalte tut dies **in weniger als elf Minuten**. 

<p align="center">
<img src="https://github.com/gfzb/DiePost/blob/master/img/3_poststellen.png" width="600px" >
</p>

### Erreichbarkeit des Hausservice
Ähnlich verhält es sich bei der Erreichbarkeit des Hausservice. Unsere Zahlen deuten daraufhin, dass 90 Prozent aller Haushalte, die näher an einem Hausservice als einer Poststelle bzw. Postagentur wohnen, diesen in **unter 28 Minuten** erreichen.

<p align="center">
<img src="https://github.com/gfzb/DiePost/blob/master/img/4_hausservice.png" width="600px" >
</p>

### Allgemeine Erreichbarkeit
Daraus ergibt sich folgendes Gesamtbild: **Eine Mehrheit der Schweizer Haushalte (ca. 60 Prozent) erreicht heute die traditionelle Poststelle als ersten Postservice**. Dass diese vor allem in dichter besiedelten Regionen stehen dürften, wird dann klar, wenn man sich vor Augen führt, dass es heute - zusammengenommen - fast doppelt so viele Agenturen und Hausservices gibt wie traditionelle Poststellen (ca. 1200).

<p align="center">
<img src="https://github.com/gfzb/DiePost/blob/master/img/1_typ.png" width="600px" >
</p>

Die Anreisedauer ändert sich beim Zusammenführen aller drei Typen nur unwesentlich. Nach wie vor fällt auf, dass zwar viele Haushalte sehr schnell bei einer Poststelle sind, einige wenige jedoch sehr viel Zeit einrechnen müssen. 

<p align="center">
<img src="https://github.com/gfzb/DiePost/blob/master/img/2_dauer.png" width="600px" >
</p>
