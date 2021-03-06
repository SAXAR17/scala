import scala.language.postfixOps
import scala.math.BigInt.probablePrime
import scala.util.Random

//2
"crazy" * 3

//3
10 max 2

//4
BigInt(2).pow(1024)

//5
//Нужно импортировать scala.util.Random,scala.math.BigInt, scala.math.BigInt.probableInt

//6
//java.lang.string
probablePrime(100, Random) toString 36

//7
"Hello"(0)
"Hello".head
"Hello".take(1)
//последний символ
"Hello".reverse(0)
"Hello".last
"Hello".takeRight(1)

//8
"Hello".take(4) //Hell выводит  символы от начала строки до нашего числа
"Hello".drop(6) //o выводит символы от нашего числа до конца строки
"Hello".takeRight(2) // lo выводит символы от конца до нашего числа с обратной стороны
"Hello".dropRight(2) //Hel выводит символы от начала до нашего числа с конца строки

//9
def signum(num: Int) = {
  if (num > 0) print(1) else if (num < 0) print(-1) else print(0)
}
signum(10)

//10
val t = {}

//11
//for (int i = 10; i >= 0; i –) System.out.println(i)
for (i <- 10 to 0 by -1) print(i)

//12
def countdown(n: Int) = for (i <- n to 0 by -1) println(i)
countdown(5)

//13
def f13(str: String): Long = {
  var t: Long = 1
  for (i <- str) {
    t = t * i.toLong
  }
  t
}
f13("Sakharov")


//14 без применения цикла
def f14(str: String): Long = {
  var t: Long = 1
  str.foreach(t *= _.toLong)
  t
}
f14("Sakharov")

//15
def f15(s: String): Long = { //
  var t: Long = 1
  for (i <- s) {
    t *= i.toLong
  }
  t
}
f15("Sakharov")

//16 рекурсия
def f16(s: String): Long = {
  if (s.length == 1) return s.charAt(0).toLong
  else s.take(1).charAt(0).toLong * f16(s.drop(1))
}
f16("Sakharov")

//17
def f17(x: Double, n: Int): Double = {
  if (n == 0) 1
  else if (n < 0) 1 / (x - n)
  else if (n % 2 == 1) x * f17(x, n - 1)
  else {
    val y = f17(x, n / 2)
    y * y
  }
}
f17(3, 4)

//18
def df18(n: Int): Boolean = {
  var list1: List[Int] = List()
  var num: Int = n
  while (num > 0) {
    var ch: Int = num % 10
    if (list1.contains(ch)) return false
    else list1 = list1 :+ ch
    num = num / 10
  }
  return true
}

def f18(m: Int, n: Int): Int = {
  var f18: Int = 0
  for (i <- m to n) {
    if (df18(i)) f18 += i
  }
  f18
}

f18(2, 10)

//19
def f19(ls: List[Any]): List[Any] = ls flatMap {
  case i: List[_] => f19(i)
  case e => List(e)
}

f19(List(List(1),1, 2, 3, 4, 5, 6))

//21
def f21(x: List[Any], k: Int): Unit = {
  var list: List[Any] = List()
  for (i <- x) {
    for (j <- 1 to k) {
      list = list :+ i
    }
  }
  println(list)
}

f21(List(1, 2, 3, 4, 5, 6, 7, 8, 9), 2)

//24
def df24(num1: Int, num2: Int): Int = {
  if (num1 == 0 | num2 == 0) return num1 + num2
  else if (num1 > num2) return df24(num1 % num2, num2)
  else return df24(num1, num2 % num1)
}

def f24(num1: Int, num2: Int): Int = {
  num1 * num2 / df24(num1, num2)
}

f24(10, 65)

//25
def f25(n: List[Any], del: Int): Unit = {
  var res: List[Any] = List()
  var count: Int = 0
  for (i <- n) {
    count += 1
    if (count < del) res = res :+ i
    else count = 0
  }
  println(res)
}

f25(List(1, 2, 3, 4, 5, 6, 7, 8), 2)

//26
def factotial(n: Int): Int = {
  var res: Int = 1
  for (i <- 1 to n) res *= i
  res
}

def f26(n: Int, k: Int): Int = {
  if (k > n) -1
  else {
    factotial(n) / factotial(n - k)
  }
}

f26(10, 23)

//27
def prin(n: List[Any]): Unit = {
  for (i <- n) print(i + ", ")
  println()
}

def f27(n: List[Any], moveLeft: Int): Unit = {
  var ans: List[Any] = n
  if (moveLeft >= 0) {
    val left = ans.take(moveLeft)
    val right = ans.takeRight(n.length - moveLeft)
    ans = right ::: left
  } else {
    val right = ans.takeRight(-moveLeft)
    val left = ans.take(n.length + moveLeft)
    ans = right ::: left

  }
  prin(ans)
}

f27(List(1, 2, 3, 4, 5, 6, 7, 8), 3)


//28
def per(n: Int): Boolean = {
  var summa = 0
  var res: Boolean = false
  for (i <- 1 to n - 1 reverse) {
    if (n % i == 0) summa += i
  }
  if (summa == n) res = true
  res
}

def f28(n: Int): Int = {
  for (i <- 1 to n reverse) {
    if (per(i)) return i
  }
  1
}

f28(520)

//29
def f29(n: List[Any]): Unit = {
  val index = n.length - 1
  var ord: List[Any] = List()
  var even: List[Any] = List()

  for (i <- 0 to index) {
    if (i % 2 == 0) ord = ord :+ n(i)
    else even = even :+ n(i)
  }
  println(ord)
  println(even)
}

f29(List(2, 3, 2, 3, 2, 3, 2, 3, 2, 3, 2))



//31
def f31(n: List[Any]): Unit = {
  val len = n.length - 1
  var list1, list2: List[Any] = List()
  for (i <- 0 to len) {
    list1 = list1 :+ n(i).asInstanceOf[List[Any]](0)
    list2 = list2 :+ n(i).asInstanceOf[List[Any]](1)
  }
  println(list1)
  println(list2)
}

f31(List(List("Alex", 100), List("Saxar", 200)))

//30
def ddf30(n: Int): Int = {
  var sum: Int = 0
  var cn: Int = n
  while (cn > 0) {
    sum += cn % 10
    cn = cn / 10
  }
  sum
}

def df30(n: Int): Boolean = {
  var num: Int = ddf30(n)
  var ch: Int = num
  if (ch == n) return true
  while (ch < n) {
    ch *= num
    if (ch == n) return true
  }
  false
}

def f30(n: Int): Int = {
  for (i <- 1 to n reverse) {
    if (df30(i)) return i
  }
  -1
}

f30(515)