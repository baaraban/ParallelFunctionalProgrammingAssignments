package TinyLanguage

import scala.collection.mutable.Map

sealed trait Statement {
  def execute(env: Map[String, Any]): Expr = this match {
    case DoNothing() => Bool(true)
    case Assign(x: Var, exp: Expr) => {
      val result = exp.eval(env);
      if (result.isInstanceOf[Error])
        Error("Error of execution")
      else {
        env(x.name) = result;
        Bool(true);
      }
    }
    case Sequence(seq: List[Statement]) => {
      if (seq.isEmpty) {
        Bool(true)
      } else {
        val result = seq.head.execute(env)
        if (result.isInstanceOf[Bool]) {
          Sequence(seq.drop(1)).execute(env)
        } else {
          Error("Error of execution")
        }
      }
    }

    case WhileLoop(condition: Expr, job: Statement) => {
      if (condition.isInstanceOf[Bool] || condition.isInstanceOf[Less]) {
        if (condition.eval(env).asInstanceOf[Boolean]) {
          job.execute(env)
          this.execute(env)
        } else {
          Bool(true)
        }
      } else {
        Error("Unvalid condition")
      }
    }

    case IfElseStatement(condition: Expr, ifTrue: Statement, ifElse: Statement) => {
      if (condition.isInstanceOf[Bool] || condition.isInstanceOf[Less]) {
        if (condition.eval(env).asInstanceOf[Boolean]) {
          ifTrue.execute(env)
        } else {
          ifElse.execute(env)
        }
      } else {
        Error("Unvalid condition")
      }
    }
  }
}


case class DoNothing() extends Statement
case class Assign(x : Var, exp : Expr) extends Statement
case class Sequence(seq : List[Statement]) extends Statement
case class WhileLoop(condition: Expr, job: Statement) extends Statement
case class IfElseStatement(condition: Expr, ifTrue: Statement, ifElse: Statement) extends Statement
