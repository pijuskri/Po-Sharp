package veritas

import org.reflections.Reflections

import scala.collection.convert.ImplicitConversions._
import scala.collection.immutable.ListMap

/**
 * Generates a simple coverage report indicating which case classes inheriting from [[Expr]] are being used by
 * the tests.
 */
object Coverage {
  private val reflections = new Reflections("scala")
  private var exprs: List[List[(Class[_ <: Expr], String, Int)]] = List()

  // Put names of redundant expressions here so they are ignored in the report (such as "Ident")
  private val redundantExprs: List[String] = List()

  /**
   * Calculates the case classes the given expression covers and saves it for later.
   *
   * @param expr Parsed code
   */
  def AddCoverage(expr: Expr): Unit = this.synchronized {
    this.exprs = this.exprs ::: List(GetExprs(expr))
  }

  /**
   * Returns the list of [[Expr]] case classes used in the given expression along with their counts.
   *
   * @param expr The expression
   * @return List[ClassName, Count]
   */
  private def GetExprs(expr: Expr): List[(Class[_ <: Expr], String, Int)] = {
    val exprs = GetExprClassNameTuples

    val tmp = expr.toString
      .split(",").flatMap(el =>
      el
        .replaceAll("[()]", "\n")
        .split("\n")
        .map(_.trim)
        .filterNot(el => el.isEmpty || el == "List")
        .filterNot(redundantExprs.contains(_))
    )

    exprs
      .filter(el => tmp.contains(el._2))
      .map(el => (el._1, el._2, tmp.count(_ == el._2)))
  }

  /**
   * Get the list of case classes inheriting from [[Expr]] along with their names.
   *
   * @return (Class,ClassName) tuples
   */
  private def GetExprClassNameTuples: List[(Class[_ <: Expr], String)] = {
    reflections.getSubTypesOf(classOf[Expr]).toList
      .map(el => (el, el.getName.split("\\$").last))
  }

  /**
   * Generates a map between each case class and the amount of times they were used in the tests.
   *
   * @return Map[ClassName, TimesUsed]
   */
  def CalculateCoverage(): Map[String, Int] = {
    val coverages = SumCoverages(exprs)

    val res = GetAllExprCaseClasses()
      .groupBy(identity)
      .map(el => el._1 -> 0)
      .filterNot(el => redundantExprs.contains(el._1))

    ListMap.from((res ++ coverages).toSeq.sortBy(_._2))
  }

  /**
   * Sums a list of results generated by the [[GetExprs]] the method; turns them into a map and sums the class usages.
   *
   * @param args List of `GetExprs` results
   * @return Map[ClassName, TimesUsed]
   * @note I'm bad at explaining. The difference between this and [[CalculateCoverage]] is that this method
   *       only includes used case classes in the return while the other one includes all of them (the rest just
   *       with a count of zero)
   */
  private def SumCoverages(args: List[List[(Class[_ <: Expr], String, Int)]]): Map[String, Int] = {
    args
      .flatten
      .groupBy(_._2)
      .map(el => (el._1, el._2.reduce((op, x) => (op._1, op._2, op._3 + x._3))))
      .map(el => (el._1, el._2._3))
  }

  /**
   * Gets all case classes inheriting from [[Expr]]
   *
   * @return List of class names.
   */
  private def GetAllExprCaseClasses(): List[String] = {
    classOf[Expr]
      .getDeclaredClasses
      .map(el => el.getName.split("\\$").last)
      .distinct
      .toList
  }
}