// ExprParser.scala

package frontend

import backend.AwkQueryAST
import backend.AwkMapExpr
import backend.ASTNormalizer
import scala.quoted._
import backend.Projection
import backend.MapExpr
import backend.ASTCompiler

object ExprParser:

  def from[T: Type, R: Type](f: Expr[T => R])(using
      Quotes
  ): Expr[T] => Expr[R] =
    (x: Expr[T]) => '{ $f($x) }

  def computeIndex[T](expr: Expr[_])(using Quotes, Type[T]) =
    val quoted = implicitly[Quotes]
    import quoted.reflect._
    expr.asTerm match {
      case Select(Ident(_), propertyName) =>
        val tpe = TypeRepr.of[T]
        if (tpe.classSymbol.get.name == "Tuple2")
          if (propertyName == "_1") 1
          else 2
        else
          tpe.classSymbol.get.caseFields.zipWithIndex
            .find(_._1.name == propertyName)
            .map(_._2)
            .get + 1
      case Inlined(_, _, Block(_, Select(Ident(_), propertyName))) =>
        val tpe = TypeRepr.of[T]
        tpe.classSymbol.get.caseFields.zipWithIndex
          .find(_._1.name == propertyName)
          .map(_._2)
          .get + 1

    }

  def parseTuples[BaseT](expr: Expr[_])(using Quotes, Type[BaseT]) =
    expr match {
      case '{
            type t1
            type t2
            Tuple2($v1: `t1`, $v2: `t2`)
          } =>
        import quotes.reflect.report
        import quotes.reflect._
        Projection(computeIndex[BaseT](v1), computeIndex[BaseT](v2))
      case '{
            type t1
            type t2
            type t3
            Tuple3($v1: `t1`, $v2: `t2`, $v3: `t3`)
          } =>
        import quotes.reflect.report
        import quotes.reflect._
        Projection(
          computeIndex[BaseT](v1),
          computeIndex[BaseT](v2),
          computeIndex[BaseT](v3)
        )
      case '{
            $v1
          } =>
        import quotes.reflect.report
        import quotes.reflect._
        Projection(computeIndex[BaseT](v1))
      case e =>
        import quotes.reflect.report
        report.error("Expression not supported:" + e.show)
        ???
    }

  def parseLambda[T, R](using
      Quotes,
      Type[T],
      Type[R]
  ): PartialFunction[Expr[_], Projection] = { case '{ ((x: T) => $f(x): R) } =>
    import quotes.reflect._
    val myX: Expr[T] = '{ ??? }
    val res = Expr.betaReduce(from(f).apply(myX))
    res.asTerm match {
      case Inlined(_, _, Block(_, t1)) =>
        parseTuples[T](t1.asExpr)
      case _: Term =>
        import quotes.reflect.report
        report.error("Expression not supported")
        ???
    }
  }

  def parseQuery[T](using
      qctx: Quotes
  ): PartialFunction[Expr[_], AwkQueryAST] = {

    import qctx.reflect._
    {
      case expr @ '{ type t; AwkQuery.apply[`t`] } =>
        val tpe = TypeRepr.of[t]
        val name: String = tpe.classSymbol.get.name
        val proj = Projection(
          tpe.classSymbol.get.fieldMembers.zipWithIndex.map(_._2).map(_ + 1): _*
        )
        AwkQueryAST(name.toLowerCase() + ".csv", MapExpr(proj))
      case '{ type qt; type mt; ($q: AwkQuery[`qt`]).map[`mt`]($lambda) } =>
        val tpe = TypeRepr.of[qt]
        val name: String = tpe.classSymbol.get.name
        tpe.classSymbol.get.fieldMembers
        val rootQuery = parseQuery(q)
        val projection = parseLambda[qt, mt](lambda)
        AwkQueryAST(
          rootQuery.fileNameOrPath,
          (rootQuery.mapFilterExpr :+ MapExpr(projection)): _*
        )
      case expr: Expr[?] =>
        import qctx.reflect.report
        import quotes.reflect._
        report.error("Expression not supported: " + expr.asTerm)
        ???
    }
  }