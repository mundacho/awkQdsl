// MacroController.scala

package frontend

import backend.AwkQueryAST
import backend.AwkMapExpr
import backend.ASTNormalizer
import scala.quoted._
import backend.Projection
import backend.MapExpr
import backend.ASTCompiler

case class Quoted[+T](expr: AwkQueryAST)

object Quoted:

  def apply[T](awkRootExpr: AwkQueryAST) = new Quoted[T](awkRootExpr)

  inline def quote[T](inline bodyExpr: T): Quoted[T] =
    ${ MacroController.process[T]('bodyExpr) }

object MacroController:

  def process[T](bodyRaw: Expr[T])(using Quotes, Type[T]): Expr[Quoted[T]] =
    import quotes.reflect._
    val ast = ExprParser.parseQuery(bodyRaw)
    import quotes.reflect.report
    val normalized = ASTNormalizer.normalize(ast)
    report.info(ASTCompiler.compile(normalized))
    val liftedQuery = ExprLifter.lift(normalized)
    '{ Quoted[T](${ liftedQuery }) }