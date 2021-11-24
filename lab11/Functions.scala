package exercise1

/** Напишите отдельные функции, решающие поставленную задачу. 
  * 
  * Синтаксис:
  *   // метод
  *   def myFunction(param0: Int, param1: String): Double = // тело
  * 
  *   // значение
  *   val myFunction: (Int, String) => Double (param0, param1) => // тело
  */
object Functions {
def square(r: Double): Double = r * r * 3.14
  /* a) Напишите функцию, которая рассчитывает площадь окружности
   *    r^2 * Math.PI
   */

  // примените вашу функцию из пункта (a) здесь, не изменяя сигнатуру
  def testCircle(r: Double): Double = square(r)

  /* b) Напишите карированную функцию которая рассчитывает площадь прямоугольника a * b.
   */
  def square1(a:Double)(b: Double): Double = a * b
  // примените вашу функцию из пукта (b) здесь, не изменяя сигнатуру
  def testRectangleCurried(a: Double, b: Double): Double = square1(a)(b)
  // c) Напишите не карированную функцию для расчета площади прямоугольника.
  def square2(a:Double, b: Double): Double = a * b
  // примените вашу функцию из пункта (c) здесь, не изменяя сигнатуру
  def testRectangleUc(a: Double, b: Double): Double = square2(a,b)

  def main(args: Array[String]): Unit ={
    println(testCircle(5))
    println(testRectangleCurried(2,3))
    println(testRectangleUc(2,3))
  }
}
