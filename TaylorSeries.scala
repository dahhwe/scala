import scala.annotation.tailrec
import scala.io.StdIn.readLine

/**
 * Объект, реализующий вычисление ряда Тейлора для функции arth(x).
 */
object TaylorSeries {

  /**
   * Точка входа в программу.
   *
   * @param args аргументы командной строки
   */
  def main(args: Array[String]): Unit = {
    println("Введите начальное значение X:")
    val xStart = BigDecimal(readLine())

    println("Введите конечное значение X :")
    val xEnd = BigDecimal(readLine())

    println("Введите шаг dx:")
    val dx = BigDecimal(readLine())

    println("Введите точность e:")
    val e = BigDecimal(readLine())

    calculateAndPrintTable(xStart, xEnd, dx, e)
  }

  /**
   * Функция для вычисления arth(x) с использованием ряда Тейлора
   *
   * @param x точка, в которой вычисляется функция
   * @param e точность вычисления ряда Тейлора
   * @return кортеж, содержащий приближенное значение функции в точке `x` и количество итераций,
   * необходимых для достижения заданной точности `e`
   */
  private def arthTaylor(x: BigDecimal, e: BigDecimal): (BigDecimal, Int) = {
    @tailrec
    def loop(n: Int, term: BigDecimal, sum: BigDecimal, iterations: Int): (BigDecimal, Int) = {
      if (term.abs <= e) (sum, iterations)
      else {
        val nextTerm = term * x * x * (2 * n - 1) / (2 * n + 1)
        loop(n + 1, nextTerm, sum + nextTerm, iterations + 1)
      }
    }

    loop(n = 1, x, x, 1)
  }

  /**
   * Функция для печати заголовка таблицы.
   */
  private def printHeader(): Unit = {
    println("x%-10s f(x)%-20s Taylor(x)%-20s TI (Taylor Iterations)%-10s".format(" ", " ", " ", " "))
  }

  /**
   * Функция для печати строки таблицы.
   *
   * @param x          точка, в которой вычисляется функция
   * @param fx         точное значение функции в точке x
   * @param taylor     приближенное значение функции в точке x, вычисленное с помощью ряда Тейлора
   * @param iterations количество итераций ряда Тейлора, необходимых для достижения заданной точности
   */
  def printRow(x: BigDecimal, fx: BigDecimal, taylor: BigDecimal, iterations: Int): Unit = {
    println(f"${x.toDouble}%-10.2f ${fx.toDouble}%-20.10f ${taylor.toDouble}%-20.10f $iterations%-10d")
  }

  /**
   * Проверка входных параметров и вычисление значений.
   *
   * @param xStart начальное значение интервала для x
   * @param xEnd   конечное значение интервала для x
   * @param dx     шаг, с которым будет изменяться x
   * @param e      точность вычисления ряда Тейлора
   */
  private def calculateAndPrintTable(xStart: BigDecimal, xEnd: BigDecimal, dx: BigDecimal, e: BigDecimal): Unit = {
    if (dx <= 0 || e <= 0) throw new IllegalArgumentException("Step size and precision must be greater than 0")
    if (xEnd < xStart) throw new IllegalArgumentException("End value must be greater than start value")

    printHeader()

    /**
     * Рекурсивно вычисляет и печатает значения функции и значения ряда Тейлора для каждого `x` на интервале.
     * Рекурсия завершается, когда достигнуто конечное значение `xEnd`.
     *
     * @param x текущее значение x для вычисления
     */
    @tailrec
    def printValues(x: BigDecimal): Unit = {
      if (x > xEnd) ()
      else {
        val (taylorValue, iterations) = arthTaylor(x, e)
        // Альтернатива функции arth
        val fx = BigDecimal(math.log((1 + x.toDouble) / (1 - x.toDouble)))
        printRow(x, fx, taylorValue, iterations)
        printValues(x + dx)
      }
    }

    printValues(xStart)
  }
}
