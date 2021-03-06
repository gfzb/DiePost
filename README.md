<a rel="license" href="http://creativecommons.org/licenses/by-nc/3.0/ch/"><img alt="Creative Commons Lizenzvertrag" style="border-width:0" src="https://i.creativecommons.org/l/by-nc/3.0/ch/88x31.png" /></a><br/>

# Eine Analyse des Schweizer Poststellennetzes
Gemäss Art. 33 Abs. 4 der Postverordnung ([VPG](https://www.admin.ch/opc/de/classified-compilation/20112357/index.html#a36)) muss die Post *"...gewährleisten, dass 90 Prozent der ständigen Wohnbevölkerung zu Fuss oder mit öffentlichen Verkehrsmitteln eine Poststelle oder Postagentur innerhalb von 20 Minuten erreichen können. Bietet die Post einen Hausservice an, so gelten für die betroffenen Haushalte 30 Minuten.*" 

**Erfüllt das heutige Poststellennetz diese Anforderungen?** – Dieser Frage sind wir anlässlich unseres [firmeninternen Open-Data-Hackdays](https://www.gfzb.ch/) mit einem datengetriebenen Ansatz und viel *Open Data* nachgegangen.

## Idee
Wir generieren auf Basis der Open-Data-Datensätze der Post eine **Distanz-Matrix zwischen möglichst repräsentativen Adressen von Schweizer Haushalten und den verschiedenen Postdienstleistungen**. Danach eruieren wir mit Hilfe der Distance Matrix API von Google Maps, **wie lange die Reise zu den nächstgelegenen Postangeboten** dauert.

## Vorgehen
Der zur Analyse verwendete R-Code ist unter [post_erreichbarkeit.R](https://github.com/gfzb/DiePost/blob/master/post_erreichbarkeit.R) abgelegt. Das Vorgehen wird nachfolgend kurz skizziert.


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
>Mit Hilfe der API von Google Maps werden alle Postadressen geocodiert ([2'500 Abfragen/Tag sind kostenlos](https://developers.google.com/maps/pricing-and-plans/#details), ca. 1,4 Prozent müssen von Hand nachcodiert werden, ). Da nun die Koordinaten von Start und Ziel bekannt sind, können die Distanzen zwischen allen Adressen und Postangeboten berechnet werden. Diese dienen jedoch nur der Vorselektion der für die Simulation der Reisezeiten relevanten Poststellen.

**E. Eruieren der Wegzeit zwischen jeder Adresse und den fünf nächstgelegenen Postangeboten**

Link: [Google Maps Distance Matrix API](https://developers.google.com/maps/documentation/distance-matrix/intro?hl=en)
>Für jede Adresse wird nun mit Hilfe der Distance Matrix API von Google Maps eruiert, wie lange man zu Fuss und mit dem öffentlichen Verkehr zu den fünf nächstgelegenen Postangeboten unterwegs ist. Für weitere Analysen werden zudem die Fahrzeiten mit Velo und Auto abgefragt. Die Reisezeiten wurden für Dienstag, den 05. Dezember 2018, um 9:30 Uhr 'simuliert'. Es werden die Standardeinstellungen der API verwendet. Die getätigte Abfrage (ca. 50'000 Anforderungen) kostet bei den aktuellen [API-Preisen von Google](https://developers.google.com/maps/pricing-and-plans/#details) rund 25 Franken.

## Resultate
### Erreichbarkeit von Poststellen und Postagenturen
Die grafische Aufbereitung und Auswertung der Daten lässt vermuten, **dass die Post heute ihre bundesrätlichen Erreichbarkeitsvorgaben erfüllt**. So erreichen gut 90,6 Prozent der ständigen Wohnbevölkerung zu Fuss oder mit öffentlichen Verkehrsmitteln eine Poststelle oder Postagentur in mindestens **20 Minuten**. Rund die Hälfte der Schweizer Haushalte schafft dies **in weniger als elf Minuten**. 

Die **Eidgenössische Postkommission PostCom** weist in ihrem [Jahresbericht 2016](https://www.postcom.admin.ch/inhalte/PDF/Jahresberichte/POC-01-2017_TB2016_DE_RZ.pdf) eine Erreichbarkeit (<= 20 Min.) von **94,3 Prozent** aus. Die Differenz dürfte in erster Linie mit den Unterschieden bei der *Fallauswahl* (Vollerhebung vs. Sample), der *Gewichtung der Adressen* (wahrscheinlich Adress- bzw. Individualebene vs. Gemeindeebene), der gewählten *Quelle zur Bestimmung der Reisezeiten* (Verbindungsauskunft SBB vs. Distance Matrix API von Google Maps) und der von uns gewählten *Methode zur Eruierung der nächstgelegenen Postdienstleistung* (Räumliche Nähe ist nicht in allen Fällen mit kurzer Reisezeit assoziiert) zu erklären sein.

<p align="center">
<img src="https://github.com/gfzb/DiePost/blob/master/img/3_poststellen.png" width="600px" >
</p>

### Erreichbarkeit des Hausservice
Ähnlich verhält es sich bei der Erreichbarkeit des Hausservice. Unsere Zahlen deuten daraufhin, dass rund **93,3 Prozent** aller Haushalte, die näher an einem Hausservice als einer Poststelle bzw. Postagentur wohnen, diesen in **unter 30 Minuten** erreichen.

<p align="center">
<img src="https://github.com/gfzb/DiePost/blob/master/img/4_hausservice.png" width="600px" >
</p>

### Allgemeine Erreichbarkeit
Daraus ergibt sich folgendes Gesamtbild: **Eine Mehrheit der Schweizer Haushalte (ca. 60 Prozent) erreicht heute die traditionelle Poststelle als ersten Postservice**. Dass diese vor allem in dichter besiedelten Regionen stehen dürften, wird dann klar, wenn man sich vor Augen führt, dass es heute - zusammengenommen - fast doppelt so viele Agenturen und Hausservices gibt wie traditionelle Poststellen (ca. 1200).

<p align="center">
<img src="https://github.com/gfzb/DiePost/blob/master/img/1_typ.png" width="600px" >
</p>

Die Anreisedauer ändert sich beim Zusammenführen aller drei Typen nur unwesentlich. Nach wie vor fällt auf, dass zwar viele Haushalte sehr schnell eine postalischen Zugangspunkt erreichen, einige wenige jedoch sehr viel Zeit dafür einrechnen müssen. 

<p align="center">
<img src="https://github.com/gfzb/DiePost/blob/master/img/2_dauer.png" width="600px" >
</p>

Eine Aufschlüsselung der Anreisezeiten nach Kantonen, die aufgrund der teils tiefen Fallzahlen mit Vorsicht zu geniessen ist, gibt der Vermutung weiter Auftrieb, dass regional erhebliche Unterschiede bestehen. Für politischen Zündstoff ist gesorgt. Wer seinen Kanton genauer untersucht haben möchen, darf sich gerne melden. ;-)

<p align="center">
<img src="https://github.com/gfzb/DiePost/blob/master/img/6_kantone.png" width="600px" >
</p>

Und zuguterletzt noch der angekündigte Vergleich der Transportarten, wobei nur traditionelle Poststellen als Zielorte in Frage kommen.  

<p align="center">
<img src="https://github.com/gfzb/DiePost/blob/master/img/7_transportart.png" width="600px" >
</p>
