// AwkEdsl.scala

package backend

case class AwkQueryAST(
    fileNameOrPath: String,
    mapFilterExpr: AwkMapExpr*
)

case class Projection(idx: Int*)

sealed trait AwkMapExpr

case class MapExpr(l: Projection) extends AwkMapExpr

case class CompositeMap(exprs: List[MapExpr])