object rationals {
  val x = new Rational(1, 2)
  x.numer
  x.denom
  println("!!!")
}

class Rational(x: Int, y: Int) {
  def numer = x
  def denom = y
}