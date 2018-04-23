import TinyLanguage.Expr

import scala.collection.mutable.Map

final class Machine {
  def run(expr: Expr, env: Map[String, Any]) : Expr = {
    println(expr.show)
    if(expr.isReduciable)
      run(reductionStep(expr, env), env)
    else
      expr
  }

  def reductionStep(expr: Expr, env: Map[String, Any]): Expr = {
    expr.reduce(env)
  }
}