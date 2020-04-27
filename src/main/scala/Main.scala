object Main {

  def main(args: Array[String]): Unit = {

    //Lab 5
    val list: List[Int] = List(1, 2, 85, 4, 5)
    println(list.min)
    println(getPrime(sieve(list.max), list))

    //Lab 6
    println(mean(list))
    println( getListLength(list))

    val s = List("simple ", "example")
    println(s.map(_.toString).flatten)
    println(s.flatMap(_.toString))


  }

  //Sieve of Eratosthenes
  def sieve(n: Int): Array[Int] = {
    val S: Array[Int] = new Array[Int](n + 1)
    S(1) = 0


    for (k <- 2 to n) {
      S(k) = 1
    }


    for (k <- 2 to n if k * k <= n) {

      if (S(k) == 1) {

        for (l <- k * k to n by k) {
          S(l) = 0
        }
      }
    }

    S
  }


  //return prime list
  def getPrime(s: Array[Int], l: List[Int]): List[Int] = {
    l.filter(elem => s(elem) == 1)
  }

  //arithmetic mean
  def mean(as: List[Int]): Float = {
    def helper(as: List[Int], accumulator_length: Int, accumulator_sum: Int): Float = {
      as match {
        case Nil => accumulator_sum.toFloat / accumulator_length
        case x :: xs => helper(xs, accumulator_length + 1, accumulator_sum + x)
      }
    }

    helper(as, 0, 0)
  }

  def getListLength (l: List[Int]): Int = {
    l.foldLeft ( 0 ) {(acc, i) => acc + 1}
  }

}
