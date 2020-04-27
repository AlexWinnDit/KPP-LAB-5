object Main {

  def main(args: Array[String]): Unit = {
    val list: List[Int] = List(1, 2, 85, 4, 5)
    println(list.min)
    println(getPrime(sieve(list.max), list))


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




  def getPrime(s: Array[Int], l: List[Int]): List[Int] = {
    l.filter(elem => s(elem) == 1)
  }

}
