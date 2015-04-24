package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = Signal {
    Math.pow(b(), 2) - 4 * a() * c()
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = Signal {
    val d = delta()

    if (d < 0) {
      Set()
    } else {
      val x = (-b() + Math.sqrt(d)) / (2 * a())
      val y = (-b() - Math.sqrt(d)) / (2 * a())
      Set(x, y)
    }
  }
}
