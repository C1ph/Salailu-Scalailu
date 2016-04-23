Scala, harjoitukset 4, tehtävä 1 a.

object Setti extends App {
  {
    val lista = for (i <- 1 to 5; j <- i to 5; if i*j%2 != 0) yield(i*j)
    lista.foreach(println)
  }
  {
    println
    println("b")
    println
  
    val l = List(1,2,3,4,5)
    val m = Array(5,6,7,8,9)
    var a = 0; var b = 0
    l.foreach(x => {m(b)+=x; b+=1})
    m.foreach(println _)
  }
  {
    println
    println("c")
    println
  
    val a=1; val b=2; val c=3;
    { val b=4; val c=5;
      { val c=6;
        println(a +"/"+ b +"/"+ c);
      }
    println(a +"/"+ b +"/"+ c);
    }
  println(a +"/"+ b +"/"+ c)
  }
  {
    println
    println("d")
    println
    
    def f(n: String, i: Int)  {println(n + ": " + i)}
    def g(n: String)(i: Int) =  {println(n + ": " + i)}
    f("Funktio f", 100)
    g("Funktio g")(200)
  }
}

Tulostuu:

1
3
5
9
15
25

Lista toimii siis iteraattorina, jota foreach käy läpi. For -loopissa käydään läpi lukujen 1-5 kertoimet keskenään, ja palautetaan parittomat luvut.

1 b.

val l = List(1,2,3,4,5)
val m = Array(5,6,7,8,9)
var a = 0; var b = 0
l.foreach(x => {m(b)+=x; b+=1})
m.foreach(println _)

Tulostuu:

6
8
10
12
14

Jokainen listan l arvo lisätään vastaavalle paikalla olevaan Arrayssa "m" olevaan alkioon, jonka jälkeen  kaikki m-alkiot tulostetaan.

1 c.

val a=1; val b=2; val c=3;
{ val b=4; val c=5;
  { val c=6;
    println(a +"/"+ b +"/"+ c);
  }
  println(a +"/"+ b +"/"+ c);
}
println(a +"/"+ b +"/"+ c)

Sisempi arvo korvaa aina ulomman arvon, eli syvimmässä loopissa c on määritelty 6, sitä edeltävällä tasolla b on 4, ja ylimmällä a on 1. Keskitasolla b on 4, ja c on 5, ja uloimman a:n määritys on 1. Uloimmalla tasolla otetaan arvot suoraan alun määrittelyistä.

1 d.

def f(n: String, i: Int)  {println(n + ": " + i)}
def g(n : String)(i : Int) {println(n + ": " + i)}

f("yksi", 1)

g("kaksi")(2)

val h = g("kolme")(_)
h(3)

Funktio f on jaettu osiin, jolloin syntyy "funktio funktiolle". Tällöin voidaan luoda osittain täytettyjä funktioita, h:n tyyliin.

2 a.

object Kieli {

	def jos(ehto: Boolean)(tulos: => Unit) = if(ehto)tulos

	def kirjoita(p : Any){println(p)}

	def lue() : Int = readInt

	def toista(maara : Int)(p: => Unit) {for(i <- 0 to maara)p}

	def toistaEhdolla(ehto: => Boolean)(toiminto: => Unit) {while(ehto)toiminto}
}

***

import Kieli._

object Main extends App{

// Ohjelmointiesimerkki:

jos (1<2) {kirjoita("kissa")}

Kieli.kirjoita(1 + 2)
Kieli.kirjoita(1 < 2)

kirjoita("Montako onnittelua? ")
var onLkm = lue
toista (onLkm) {kirjoita("Onnea!")}

var lkm = 6
toistaEhdolla (lkm > 0) {kirjoita(lkm); lkm -= 1}

kirjoita("Mihin saakka tutkitaan lukujen parillisuutta? ")
var montako = lue

lkm = 1
toistaEhdolla (lkm <= montako) {
  jos (lkm % 2 == 0) {
    kirjoita(lkm + " on parillinen")
  }
  lkm += 1
}

kirjoita("Lasketaan lukujen summa a:sta b:hen:")
kirjoita("Anna a")
var a = lue
kirjoita("Anna b")
var b = lue

jos (a > b) {
  var apu = a
  a = b
  b = apu
}

var summa = 0
var laskuri = a
toistaEhdolla (laskuri <= b) {
  summa += laskuri
  laskuri += 1
}
kirjoita("Lukujen summa " + a + " ... " + b + " on " + summa)

}

2 b.

final class MinSek (min : Int, sek : Int){
	def this(sek : Int) = this(0, sek)
	def this() = this(0, 0)
	
	private val aika = (min * 60) + sek
	
	def +(that : MinSek) = new MinSek(aika + that.aika)
	def -(that : MinSek) = new MinSek(aika - that.aika)
	def unary_-() = new MinSek(-aika)
	def *(m : Int) = new MinSek(aika * m)
	def /(d : Int) = new MinSek(aika / d)
	
	override def toString = getMinFormat + ":" + getSekFormat
	
	private def getMin = (aika - getSek) / 60
	private def getSek = aika % 60
	
	private def getMinFormat : String = {
		val min = getMin
		if (min == 0 && getSek < 0) return "-0"
		return min.toString
	}
	private def getSekFormat : String = {
		val s = Math.abs(getSek)
		if (s < 10) return "0" + s
		return s.toString
	}
}

final class Varastointi {

	private var tallenteet = List[Tallenne]()
	
	override def toString : String = {
		var yhteensa = new MinSek
		var tulos = ""
		tallenteet.foreach { p =>
			yhteensa += p.getAika
			tulos += p + "\n"
		}
		tulos += "\nKesto yhteensä: " + yhteensa + "\n"
		return tulos
	}
	
	def lisaaTallenne (t : Tallenne) = tallenteet = tallenteet :+ t
	
	def poistaTallenne (t : Tallenne) = tallenteet diff List(t)
}

final class Tallenne (kesto : Int, private val name : String) {

	private val aika = new MinSek(kesto)
	
	def getAika = aika
	
	override def toString : String = name + " " + aika
	override def equals (that : Any) : Boolean = {
		val thatT = that.asInstanceOf[Tallenne];
		return thatT.name == name;
	}
}

val a = new Varastointi
val b = new Tallenne(987, "kookospulla")
val c = new Tallenne(60, "räyr")
a lisaaTallenne b
a lisaaTallenne c
println(a)
a poistaTallenne b
println(a)
