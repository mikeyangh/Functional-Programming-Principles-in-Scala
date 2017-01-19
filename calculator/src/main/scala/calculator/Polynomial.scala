package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {
    Signal(b()*b()-4*a()*c())
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    Signal(
    if (delta() < 0) Set()
    else if (delta() == 0) Set(-b()/2/a())
    else {
      val r1 = (-b()+math.sqrt(delta()))/(2*a())
      val r2 = (-b()-math.sqrt(delta()))/(2*a())
      Set(r1, r2)
    }
    )
  }
}
