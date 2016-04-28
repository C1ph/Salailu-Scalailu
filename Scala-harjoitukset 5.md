Scala-harjoitukset 5, tehtävä 1.

import scala.language.postfixOps

trait KoristellenTulostettava {

  def koristettava: String
  def koristemerkki: Char
  def reunusta = println(koristemerkki + koristettava + koristemerkki)
  def alleviivaa = println(koristettava + "\n" + koristemerkki.toString * koristettava.length)
  def ylleviivaa = println(koristemerkki.toString * koristettava.length + "\n" + koristettava)
  def ymparoi = {
    val viivaus = koristemerkki.toString * (koristettava.length + 2)
    println(viivaus)
    reunusta
    println(viivaus)
  }
}

class Kuka(var nimi: String) extends KoristellenTulostettava {
  override def koristettava = nimi
  override val koristemerkki = '*'
}

class Luku(var arvo: Int) extends KoristellenTulostettava {
  override def koristettava = " " + arvo * 2 + " "
  override def koristemerkki = math.abs(arvo)%256 toChar
}

class Huutaen(val sana: String) extends KoristellenTulostettava {
  override def koristettava = sana.toUpperCase
  override val koristemerkki = '!'
}

val pm = new Kuka("Putin")
pm.reunusta                    // Putin
pm.nimi = "Obama"
pm.reunusta                    // Obama

println
pm.alleviivaa
println
pm.ylleviivaa
println
pm.ymparoi
println

val se = new Luku(42)
se.reunusta           // 84
se.arvo = 93
se.reunusta           // 186
se.arvo = -3301
se.reunusta           // -6602

println
se.alleviivaa
println
se.ylleviivaa
println
se.ymparoi
println

val iso = new Sanonta("Kukkuu")
iso.reunusta
println
iso.alleviivaa
println
iso.ylleviivaa
println
iso.ymparoi


Tehtävä 2.

class Opiskelija(val nimi: String, var harjoitusPisteet: Int, var koePisteet: Int) {
  def kokonaisPisteet = harjoitusPisteet + koePisteet
  override def toString = nimi + ": " + kokonaisPisteet
}

abstract class OpiskelijaJoukko {
  def vie(x: Opiskelija): Unit
  def poista(x: Opiskelija): Unit
}

class ToteutettuOpiskelijaJoukko extends OpiskelijaJoukko {
  import scala.collection.mutable.HashSet

  private val joukko = new HashSet[Opiskelija]
  def vie(x: Opiskelija) {joukko += x}
  def poista(x: Opiskelija) {joukko -= x}
  override def toString = joukko.toString
}

trait Koeleikkuri extends OpiskelijaJoukko {
  abstract override def vie(x: Opiskelija) = if (x.koePisteet >= 23) super.vie(x)
}

trait Hyvaksytyt extends OpiskelijaJoukko {
  abstract override def vie(x: Opiskelija) = if (x.kokonaisPisteet >= 30) super.vie(x)
}

trait MililleViisPlus extends OpiskelijaJoukko {
  abstract override def vie(x: Opiskelija) = {
    if ((x.nimi contains "onni") || (x.nimi contains "Onni")) x.koePisteet += 5
    super.vie(x)
  }
}

val mili = new Opiskelija("Mili Käpistelijä", 7, 36)

println(mili.kokonaisPisteet)  // 43

val kurssi = new  ToteutettuOpiskelijaJoukko

val hemmo = new Opiskelija("Hemmo Hengailija", 2, 18)
kurssi.vie(mili)
kurssi vie hemmo
kurssi vie new Opiskelija("Matteus-Anneli Lapio", 19, 23)

println(kurssi)
kurssi poista hemmo

println(kurssi)

println("\nKoeleikkurista läpi:")
val p = new ToteutettuOpiskelijaJoukko with Koeleikkuri
p vie new Opiskelija("Mili Käpistelijä", 7, 36)
p vie new Opiskelija("Matteus-Anneli Lapio", 19, 23)
p vie new Opiskelija("Oskariina Sianmaito", 0, 25)
p vie new Opiskelija("Hemmo Hengailija", 2, 18)
println(p)

println("\nHyväksytyt ilman koeleikkuria:")
val h = new ToteutettuOpiskelijaJoukko with Hyvaksytyt
h vie new Opiskelija("Mili Käpistelijä", 7, 36)
h vie new Opiskelija("Matteus-Anneli Lapio", 19, 23)
p vie new Opiskelija("Oskariina Sianmaito", 0, 25)
h vie new Opiskelija("Hemmo Hengailija", 12, 18)
println(h)

println("\nHyväksytyt ja koeleikkurista läpi kun Milille +5:")
val v = new ToteutettuOpiskelijaJoukko with Hyvaksytyt with Koeleikkuri with MililleViisPlus // Täytyy laittaa järjestyksessä!
v vie new Opiskelija("Mili Käpistelijä", 7, 36)
v vie new Opiskelija("Matteus-Anneli Lapio", 19, 23)
p vie new Opiskelija("Oskariina Sianmaito", 0, 25)
v vie new Opiskelija("Hemmo Hengailija", 12, 18)
println(v)
