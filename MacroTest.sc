// MacroTest.sc

import frontend.Quoted.quote
import frontend.AwkQuery
import scala.quoted._
import backend.ASTCompiler._

case class Data(
    code: String,
    name: String,
    description: String,
    quantity: String
)

val q = quote {
  AwkQuery[Data].map(x => (x.name, x.quantity)).map(x => (x._2, x._1))
}

println(compile(q.expr))