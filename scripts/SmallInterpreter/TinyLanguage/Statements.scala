package TinyLanguage

sealed trait Statement {
  val errorKey = "errors";
  def handleError(env: Map[String, Any], er: Error) = env + (this.errorKey -> er)

  def execute(env: Map[String, Any]): Map[String, Any] = this match {
    case DoNothing() => env
    case Assign(x: Var, exp: Expr) => {
      val target: Error = Error(exp);
      exp.eval(env) match {
        case Error(_) => this.handleError(env, target)
        case _ => {
          env + (x.name -> exp.eval(env))
        }
      }
    }
    case Sequence(seq: List[Statement]) => {
      if (seq.isEmpty) {
        env
      } else {
        if(env.contains(this.errorKey)) env else Sequence(seq.drop(1)).execute(env)
      }
    }

    case WhileLoop(condition: Expr, job: Statement) => {
      val conditionResult = condition.eval(env)
      conditionResult match {
        case Bool(b) => if(b) this.execute(job.execute(env)) else env
        case _ => this.handleError(env, Error(condition))
      }
    }

    case IfElseStatement(condition: Expr, ifTrue: Statement, ifElse: Statement) => {
      val conditionResult = condition.eval(env)
      conditionResult match {
        case Bool(b) => if(b) ifTrue.execute(env) else ifElse.execute(env)
        case _ => this.handleError(env, Error(condition))
      }
    }
  }
}


case class DoNothing() extends Statement
case class Assign(x : Var, exp : Expr) extends Statement
case class Sequence(seq : List[Statement]) extends Statement
case class WhileLoop(condition: Expr, job: Statement) extends Statement
case class IfElseStatement(condition: Expr, ifTrue: Statement, ifElse: Statement) extends Statement
