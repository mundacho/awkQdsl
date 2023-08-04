// ExprLifter.scala

package frontend

import backend.AwkQueryAST
import backend.AwkMapExpr
import backend.ASTNormalizer
import scala.quoted._
import backend.Projection
import backend.MapExpr
import backend.ASTCompiler
import frontend.ExprParser

object ExprLifter:

  def liftMapFilter(
      awkRootExpr: AwkMapExpr
  )(using Quotes): Expr[AwkMapExpr] =
    awkRootExpr match {
      case MapExpr(Projection(s: _*)) =>
        val ss = Expr(s)
        '{
          MapExpr(Projection(${ ss }: _*))
        }
    }

  def lift(awkRootExpr: AwkQueryAST)(using Quotes) =
    awkRootExpr match {
      case AwkQueryAST(fileNameOrPath) =>
        '{ AwkQueryAST(${ Expr(fileNameOrPath) }) }
      case AwkQueryAST(fileNameOrPath, MapExpr(Projection(s: _*))) =>
        val ss = Expr(s)
        '{
          AwkQueryAST(
            ${ Expr(fileNameOrPath) },
            MapExpr(Projection(${ ss }: _*))
          )
        }
      case AwkQueryAST(fileNameOrPath, mexpr: _*) =>
        val res = Expr.ofSeq(mexpr.map(x => liftMapFilter(x)))
        '{
          AwkQueryAST(
            ${ Expr(fileNameOrPath) },
            ${ res }: _*
          )
        }
    }
