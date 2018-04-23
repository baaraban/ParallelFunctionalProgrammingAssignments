package TinyLanguage
import scala.collection.mutable.Map

sealed trait Expr {
  def isReduciable = this match{
    case Bool(_) => false
    case Number(_) => false
    case Error(_) => false
    case _ => true
  }

  def reduce(env: Map[String, Any]): Expr = this match{
    case Var(name) => if (env.contains(name)) Number(env(name).asInstanceOf[Int]) else Error("No key")
    case Sum(lOp, rOp) => if(lOp.isReduciable){
      Sum(lOp.reduce(env), rOp)
    } else if (rOp.isReduciable) {
      Sum(lOp, rOp.reduce(env))
    } else {
      Number(this.eval(env).asInstanceOf[Int])
    }
    case Prod(lOp, rOp) => if(lOp.isReduciable){
      Prod(lOp.reduce(env), rOp)
    } else if (rOp.isReduciable) {
      Prod(lOp, rOp.reduce(env))
    } else {
      Number(this.eval(env).asInstanceOf[Int])
    }
    case Less(lOp, rOp) => if(lOp.isReduciable){
      Less(lOp.reduce(env), rOp)
    } else if (rOp.isReduciable) {
      Less(lOp, rOp.reduce(env))
    } else {
      Bool(this.eval(env).asInstanceOf[Boolean])
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

  def eval(env: Map[String, Any]): Any = this match{
    case Number(n) => n
    case Bool(b) => b
    case Var(name) => if(env.contains(name)) env(name) else 0
    case Sum(lOp, rOp) => lOp.eval(env).asInstanceOf[Int] + rOp.eval(env).asInstanceOf[Int]
    case Prod(lOp, rOp) => lOp.eval(env).asInstanceOf[Int] * rOp.eval(env).asInstanceOf[Int]
    case Less(lOp, rOp) => lOp.eval(env).asInstanceOf[Int] < rOp.eval(env).asInstanceOf[Int]
    case IfElse(condition, lOp, rOp) => if(condition.eval(env).asInstanceOf[Boolean])  lOp.eval(env) else rOp.eval(env)
    case Error(message) => message
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
    case Error(message) => message
  }

}

case class Number(n: Int) extends Expr
case class Bool(b: Boolean) extends Expr
case class Var(name: String) extends Expr
case class Sum(lOp: Expr, rOp: Expr) extends Expr
case class Prod(lOp: Expr, rOp: Expr) extends Expr
case class Less(lOp: Expr, rOp: Expr) extends Expr
case class IfElse(condition: Expr, lOp: Expr, rOp: Expr) extends Expr
case class Error(message: String) extends Expr
