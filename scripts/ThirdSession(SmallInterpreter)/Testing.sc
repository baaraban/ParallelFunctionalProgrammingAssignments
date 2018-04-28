import TinyLanguage._


var expr = new TinyLanguage.Prod(TinyLanguage.Sum(TinyLanguage.Number(1),TinyLanguage.Number(2)),
  TinyLanguage.Sum(TinyLanguage.Number(4),TinyLanguage.Number(3)))

var env = Map[String, Any]("x" -> 1, "y" -> 3)
var expr1 = new TinyLanguage.IfElse(TinyLanguage.Less(TinyLanguage.Sum(TinyLanguage.Number(1), TinyLanguage.Number(2)), TinyLanguage.Prod(TinyLanguage.Number(2), TinyLanguage.Number(3))),
  TinyLanguage.Sum(TinyLanguage.Number(9), TinyLanguage.Var("x")),
  TinyLanguage.Prod(TinyLanguage.Number(7), TinyLanguage.Number(8)))

var someVar = Var("x")
someVar.eval(env)
var whileStatement = WhileLoop(Less(someVar, Number(10)), Assign(someVar, Prod(Number(2), someVar)))
var sequenceOfStatements = Sequence(
  List[Statement](
    DoNothing(),
    Assign(someVar, Prod(Number(2), someVar)),
    Assign(someVar, Sum(Number(20), someVar))
)
)
sequenceOfStatements.execute(env)
someVar.eval(env)

expr.show
expr.eval(env)

