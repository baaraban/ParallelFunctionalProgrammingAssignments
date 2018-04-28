package TinyLanguage

sealed trait Expr {
  def isReduciable = this match{
    case Bool(_) => false
    case Number(_) => false
    case Error(_) => false
    case _ => true
  }

  def combine(inst: Expr, action: (Int, Int) => Boolean): Expr = this match {
    case Number(n) => inst match {
      case Number(m) => Bool(action(n, m))
      case _ => Error(this)
    }
    case _ => Error(this)
  }

  def combine(inst: Expr, action: (Int, Int) => Int): Expr = this match {
    case Number(n) => inst match {
      case Number(m) => Number(action(m, n))
      case _ => Error(this)
    }
    case _ => Error(this)
  }


  def eval(env: Map[String, Any]): Expr = this match{
    case Var(name) => {
      if(env.contains(name))
        env(name) match {
          case x: Int => Number(x)
          case x: Boolean => Bool(x)
          case x: Expr => x
          case _ => Error(this)
        }
      else
        Error(this)
    }
    case Sum(lOp, rOp) => lOp.eval(env).combine(rOp.eval(env), (x: Int, y: Int) => x + y)
    case Prod(lOp, rOp) => lOp.eval(env).combine(rOp.eval(env), (x: Int, y: Int) => x * y)
    case Less(lOp, rOp) => lOp.eval(env).combine(rOp.eval(env), (x: Int, y: Int) => x < y)
    case IfElse(condition, lOp, rOp) => condition.eval(env) match {
      case Bool(b) => if(b) lOp.eval(env) else rOp.eval(env)
      case _ => Error(this)
    }
    case Error(failed) => failed
    case _ => _
  }

  def reduce(env: Map[String, Any]): Expr = this match{
    case Var(_) => this.eval(env)
    case Sum(lOp, rOp) => {
      if(lOp.isReduciable){
        Prod(lOp.reduce(env), rOp)
      } else if (rOp.isReduciable) {
        Prod(lOp, rOp.reduce(env))
      } else {
        this.eval(env)
      }
    }
    case Prod(lOp, rOp) => if(lOp.isReduciable){
      Prod(lOp.reduce(env), rOp)
    } else if (rOp.isReduciable) {
      Prod(lOp, rOp.reduce(env))
    } else {
      this.eval(env)
    }
    case Less(lOp, rOp) => if(lOp.isReduciable){
      Less(lOp.reduce(env), rOp)
    } else if (rOp.isReduciable) {
      Less(lOp, rOp.reduce(env))
    } else {
      this.eval(env)
    }
    case IfElse(condition, lOp, rOp) => if(condition.isReduciable){
      IfElse(condition.reduce(env), lOp, rOp)
    } else if(condition.eval(env) == true){
      lOp
    } else {
      rOp
    }
    case _ => this
  }

  def inside_show: String = this match{
    case Sum(lOp, rOp) => s"(${lOp.show}+${rOp.show})"
    case _  => this.show
  }

  def show: String = this match{
    case Number(n) => n.toString
    case Bool(b) => b.toString
    case Var(name) => name
    case Sum(lOp, rOp) => s"${lOp.show}+${rOp.show}"
    case Prod(lOp, rOp) => s"${lOp.inside_show}*${rOp.inside_show}"
    case Less(lOp, rOp) => s"${lOp.show} < ${rOp.show}"
    case IfElse(condition: Expr, lOp: Expr, rOp: Expr) => s"if (${condition.show}) then ${lOp.show} else ${rOp.show}"
    case Error(expr) => s"Error in (${expr.show})"
  }
}

case class Number(n: Int) extends Expr
case class Bool(b: Boolean) extends Expr
case class Var(name: String) extends Expr
case class Sum(lOp: Expr, rOp: Expr) extends Expr
case class Prod(lOp: Expr, rOp: Expr) extends Expr
case class Less(lOp: Expr, rOp: Expr) extends Expr
case class IfElse(condition: Expr, lOp: Expr, rOp: Expr) extends Expr
case class Error(failed: Expr) extends Expr
