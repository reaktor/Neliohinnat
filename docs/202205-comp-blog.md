---
output:
  pdf_document: default
  html_document: default
---
# Asuntojen hinnoista, taas

Reaktorin [Kannattaako Kauppa](https://kannattaakokauppa.fi/) julkaistiin [ensimmäisen kerran](https://www.reaktor.com/blog/asuntojen-trendit-ja-miten-niista-tehdaan-luotettavia-ennusteita/) [keväällä 2015](https://louhos.github.io/news/2015/05/07/asuntohintojen-muutokset/). Viime vuonna sen tilastollistan mallia [päivitettiin](https://www.reaktor.com/blog/how-to-estimate-housing-prices-with-demographic-data/) ensimmäistä kertaa merkittävästi. Uusi malli on herkempi vuosikohtaisille muutoksille, ja siellä missä kauppoja on riittävästi malli hyväksyy kauppojen keskihinnan sellaisenaan. Mallin ennusteet siis tarjoavat aukkojen ja epävarmuuksien osalta täydennetyn, koko maan ja kaikki vuodet kattavan arvion asuntojen hinnoista, postinumerotasolla. 

Kun hintoja on kokonaiselta vuosikymmeneltä, tekee mieli hahmotella kokonaiskuvaa. Keskittyminenkö aina vain jatkuu? Vaikuttiko korona, ja jos vaikutti, menikö vaikutus jo ohi? Mitä muuta tapahtuu?

Niille jotka eivät jaksa lukea, voi paljastaa tärkeimmät nyt, eli:

* Tiivistyminen jatkuu ja on jopa kiihtynyt. Erityisesti Itä-Suomi näyttää kartoissa kovin tummalta. 
* N. 15% hinnanvaihtelusta menee kuitenkin toiseen piikkiin, jota voisi kutsua lähiöistymiseksi ja järvisuomettumiseksi. Korona lisäsi tätä, eikä vaikutus näytä vielä kadonneen. 
* Ukrainan sota, inflaatio ja korkojen nousu ei näy vielä --- mukana ei ole v. 2022 tietoja.

Takaisin asiaan... mallista siis meillä on taulukko (tai kuten sanotaan, matriisi) hintoja vuosille 2010--2021, kaikille postinumeroille. Tai koska emme oikeasti tiedä todellisia hintoja kauppojen äärellisen määrän takia, meillä on näitä taulukoita satoja, kukin edustaa yhtä mahdollista maailmaa asuntojen hintojen osalta. 

Teimme hintataulukoille ns. pääkomponenttianalyysin, jossa selitämme hinnanmuutoksia muutamalla, lopulta vain parilla hahmolla eli komponentilla vuosien ja postinumeroiden yli. (Teknisemmille lukijoille: tämä siis jokaiselle posteriorinäytteelle erikseen, ja ennen SVD:tä poistettiin vuosi- ja postinumerokeskiarvot.) Menetelmän etu on, että emme oleta trendejä tai urbanisaatiota, vaan aika pitkälti se mitä näkyy tulee suoraan datasta. 

Ja todellakin, ensimmäinen löytyvä komponentti on keskittyminen: hinnat nousevat kaupungeissa, eniten keskustoissa, ja laskevat maaseudulla, etenkin Itä-Suomessa. 

![](../../figs/map-X1.png){width=32%}
![](../../figs/map-X1-sw.png){width=32%}
![](../../figs/map-X1-cap.png){width=32%}

Kehitys on lisäksi vuosien osalta monotonista, ja jopa nopeutunut noin kolmena viimeisenä vuotena.

![](../../figs/yearly-X1.png){width=49%}
![](../../figs/yearly-X1-diff.png){width=49%}

Keskittyminen selittää n. 75% (68--82%; 80% luottamusväli) postinumeroittaisten vuosikeskihintojen vaihtelusta. Toiseksi vahvin komponentti selittää n. 16% (9--23%), ja lopuista on hankalampi ottaa selvää. 

Toinen komponentti on vuosinäkymältään mielenkiintoinen:

![](../../figs/yearly-X2.png){width=49%}

Se oli korkealla aikasarjan alussa, v. 2010, ja koronavuosina 2020--2021, sekä mahdollisesti myös v. 2016. Kun tätä komponenttia katsoo kartoilla, voi todeta sen vastaavan hintojen nousua lähiöissä keskustojen ulkopuolella, Tampere--Helsinki-tien varressa, sekä mahdollisesti Järvi-Suomessa. 

![](../../figs/map-X2.png){width=32%}
![](../../figs/map-X2-sw.png){width=32%}
![](../../figs/map-X2-cap.png){width=32%}

Lähiöistymiskomponentin vaikutus on kuitenkin keskittymiskehitykseen verrattuna niin pieni, että laskuun se ei ole hintoja vielä ainakaan pääkaupunkiseudulla kääntänyt. 

Kun lähiöistymiskehitys laitetaan y-akselille ja keskittymiskehitys x-akselille ja plotataan kuvaan vuodet epävarmuuksineen, saadaan aikaan tällainen yhteenveto kymmenen vuoden hintakehityksestä:

![](../../figs/yearly-X1X2.png){width=60%}

Lähiöistymisen syitä on vaikea tietää, ja sen voimakkuuskin on epävarmaa. Se sopii kuitenkin tarinaan koronan aiheuttamasta etätöiden lisääntymisestä ja kaipuusta väljempiin asuinolosuhteisiin. Toinen näkökulma samaan asiaan voi olla viime aikoina uutisoitu pienten asuntojen hinnanlasku; pieniä asuntojahan on enemmän keskustoissa. 

Alkuvuodesta Ukrainan sodan tarjontavaikutukset etenkin energian osalta, ja toisaalta maan pinnalle palaava finanssipolitiikka voivat muuttaa asuntomarkkinoita. Näihin palaamme ensi keväänä. 









