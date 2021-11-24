import scala.annotation.tailrec

/** Напишите свои решения в тестовых функциях.
 *
 * Seq(1, 2) match {
 * case head +: tail => ???
 * case Nil          => ???
 * case s            => ???
 * }
 *
 * https://www.scala-lang.org/api/2.12.0/scala/collection/Seq.html
 */
// Примечание: напишите функции с хвостовой рекурсией

object Sequence {

  /* a) Найдите последний элемент Seq.
   *
   */
  def testLastElement[A](seq: Seq[A]): Option[A] = Some(seq.drop(seq.length - 1).head)

  /* b) Объедините две Seqs (то есть Seq(1, 2) и Seq(3, 4) образуют Seq((1, 3), (2, 4))) - если Seq длиннее игнорируйте оставшиеся элементы.
   *
   */
    def testZip[A](a: Seq[A], b: Seq[A]): Seq[(A, A)] = {
    @tailrec
    def loop[A](a: Seq[A], b: Seq[A], c: Seq[(A, A)]): Seq[(A, A)] = a match {
      case ahead +: atail => b match {
        case blast +: Nil => c :+ (ahead, blast)
        case bhead +: btail => loop(atail, btail, c :+ (ahead, bhead))
      }
      case Nil => c
    }

    loop(a, b, Nil)
  }

  /* c) Проверьте, выполняется ли условие для всех элементов в Seq.
   *
   */
    def testForAll[A](seq: Seq[A])(cond: A => Boolean): Boolean = {
    @tailrec
    def loop[A](seq: Seq[A])(cond: A => Boolean): Boolean = seq match {
      case head +: tail => if (cond(head)) loop(tail)(cond) else false
      case Nil => true
    }

    loop(seq)(cond)
  }

  /* d) Проверьте, является ли Seq палиндромом
   *
   */

    def testPalindrom[A](seq: Seq[A]): Boolean = {
    @tailrec
    def loop[A](seq1: Seq[A], seq2: Seq[A]): Boolean = seq1 match {
      case head +: tail => loop(tail, seq2 = seq2 :+ head)
      case Nil => seq.equals(seq2)
    }

    loop(seq, Nil)
  }

  /* e) Реализуйте flatMap используя foldLeft.
   *
   */
  def testFlatMap[A, B](seq: Seq[A])(f: A => Seq[B]): Seq[B] = seq.foldLeft(Seq[B]())((acc, elem) => acc ++: f(elem))

  def main(args: Array[String]): Unit = {
    val seq1 = Seq(1, 2, 3, 4)
    val seq2 = Seq(1,1,5,5)
    val seq3 =  Seq(3,2,1,1,2,3)

    println(testLastElement(seq1))
    println(testZip(seq1, seq2))
    println(testForAll(seq2)(num
    => num % 2 == 0
    ))
    println(testPalindrom(seq2))
    println(testFlatMap(seq1)(num
    => Seq(num + 10))
    )
  }
}