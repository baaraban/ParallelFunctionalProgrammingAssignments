import TinyLanguage.{Expr, Statement}


final class Machine {
  def run(stat: Statement, env: Map[String, Any]): Map[String, Any] = {
    println(env)
    stat.execute(env)
    println(env)
  }

  def reduce(expr: Expr, env: Map[String, Any]) : Expr = {
    println(expr.show)
    if(expr.isReduciable)
      this.reduce(reductionStep(expr, env), env)
    else
      expr
  }

  def reductionStep(expr: Expr, env: Map[String, Any]): Expr = {
    expr.reduce(env)
  }
}