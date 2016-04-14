Tehtävä 1 a & b.

class MinSek (private var min: Int, private var sek: Int) {

  var tmpsek = min*60 + sek;
  min = tmpsek/60
  sek = tmpsek%60

  def this() {
    this(0,0);
  }

  def this(sek: Int) {
    this(0,sek)
  }

  def getMin() = min;
  def getSek() = sek;

  def +(arvo: MinSek) = {
  new MinSek(min + arvo.getMin, sek + arvo.getSek)
  }

  def -(arvo: MinSek) = {
  new MinSek(min - arvo.getMin, sek - arvo.getSek)
  }

  def *(arvo: Int) = {
    new MinSek(min*arvo,sek*arvo);
  }

  def / (arvo: Int) = {
    var tmpsek = min*60 + sek;
    new MinSek(tmpsek/arvo)
  }

  def unary_-() = {
    var tmpsek = min*60 + sek;
    new MinSek(tmpsek * -1);
  }

  def +=(arvo: MinSek) {
    var tmpsek = min*60 + sek;
    var tmpsek2 = arvo.getMin*60 + arvo.getSek;
    tmpsek += tmpsek2;
    min = tmpsek/60
    sek = tmpsek%60
    }

  def -=(arvo: MinSek) {
    var tmpsek = min*60 + sek;
    var tmpsek2 = arvo.getMin*60 + arvo.getSek;
    tmpsek -= tmpsek2;
    min = tmpsek/60
    sek = tmpsek%60
    }

  def +=(arvo: Int) {
    var tmpsek = min*60 + sek + arvo;
    min = tmpsek/60
    sek = tmpsek%60
    }

  def -=(arvo: Int) {
    var tmpsek = min*60 + sek - arvo;
    min = tmpsek/60
    sek = tmpsek%60
    }

  def *= (kerroin: Int) {
    var tmpsek = min*60 + sek
    tmpsek *= kerroin;
    min = tmpsek/60
    sek = tmpsek%60
  }

  def /= (jakaja: Int) {
    var tmpsek = min*60 + sek
    tmpsek /= jakaja;
    min = tmpsek/60
    sek = tmpsek%60
  }

  override def toString = {
    var str: String = "";
    if (min == 0 && sek < 0) {str+="-"}
    if (sek < 10 && sek > -10) {
      str += min + ":0" + Math.abs(sek)
      str
    } else {
      str += min + ":" +Math.abs(sek)
      str
    }
  }
}

Tehtävä 2.

object Alkuluvut extends App {

  println("Anna luku")
  var luku = readInt
  var list = scala.collection.mutable.MutableList[Int]();
  var i = 2 // i += lower
  while(list.size < luku) { //while(i < upper)
    if (isPrime(i)) {
      list += i
    }

    i += 1
  }

  println(list);

def isPrime(n: Int) : Boolean = {

	var i = 2;

	if (n == 2) {
		return true;
	}

	while (i < n) {
		if (n % i == 0) {
			 return false;
		}
		i+=1;}
   return true;
 }
}


Tehtävä 3 a.

object Sisakkain extends App {

  val a=1; val b=2; val c=3

  def f = {
    val b=20; val c=30

    def ff = {
      val c=300
      println(a+"/"+b+"/"+c) // tulostuu 1/20/300
    }

    println(a+"/"+b+"/"+c) // tulostuu 1/20/30
    ff
  }

  def g = {

    def gg = {
      f
      60
    }

    val a=40; val b=gg;
    println(a+"/"+b+"/"+c) // tulostuu 40/60/3
    f
  }

  println(a+"/"+b+"/"+c) // tulostuu 1/2/3
  f;

  { val a=1000
    g
    println(a+"/"+b+"/"+c) // tulostuu 1000/2/3
  }

}

3 b.

object Kirjasto {

  def summa(termi: (Int) => Double, lkm: Int) = {
    var s=0.0
    for (i <- 1 to lkm) s += termi(i)
    s
  }
}

object Sovellus extends App {

  def harm(i: Int) = 1.0/i
  println(  Kirjasto.summa(harm, 4) ) // tulostetaan 1/1 + 1/2 + 1/3 + 1/4


  def geom(i: Int) = 1.0/(i*i)
  println(  Kirjasto.summa(geom, 4) ) // tulostetaan 1/(1*1) + 1/(2*2) + 1/(3*3) + 1/(4*4)


  println(  Kirjasto.summa(_ +0.5, 4) ) // tulostetaan 1.5 + 2.5 + 3.5 + 4.5


  var laskuri=0.0
  def mitaMita(i: Int) = {laskuri+=1; laskuri}
  println(  Kirjasto.summa(mitaMita, 4) ) // tulostetaan 1 + 2 + 3 + 4


  var ed=1; var seur=1; var alussa=1
  def f(i: Int) = if (alussa < 3)
                     {alussa+=1; 1}
                  else
                     {val uus=ed+seur; ed=seur; seur=uus; uus}
  println(  Kirjasto.summa(f, 4) ) // tulostetaan 1 + 1 + 2 + 3 +
}
