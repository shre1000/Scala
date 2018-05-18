
import java.lang.Math
abstract class Tree
case class Sum(l: Tree, r: Tree)  extends Tree  // addition
case class Var(n: String)         extends Tree  // variable (name)
case class Const(v: Float)       extends Tree  // constant (value)
case class prod(l1 : Tree, r1 : Tree) extends Tree // multiplication
case class sub(l2 : Tree, r2 : Tree) extends Tree // subtraction
case class div(l3 : Tree, r3 : Tree) extends Tree // division
case class Neg(r4 : Tree) extends Tree // negation
case class Sin(l5 : Tree) extends Tree // sin
case class Cos(l6 : Tree) extends Tree // cos
case class cos(l8 : Tree) extends Tree
case class sin(l9 : Tree) extends Tree

object Exprcase {
type Environment = String => Float


// Evaluate the expression t in the environment env
  def eval(t: Tree,env: Environment): Float = t match {
    case Sum(l,r)  => eval(l, env) + eval(r, env)
case Var(n)    => env(n)
    case Const(v)  => v
case prod(l1, r1) => eval(l1, env) * eval(r1, env)
case sub(l2, r2) => eval(l2, env) - eval(r2, env)
case div(l3, r3) => eval(l3, env) / eval(r3, env)
case Neg(r4) => - eval(r4, env)
case Sin(l5) => Math.sin(eval(l5, env)).toFloat
case Cos(l6) => Math.cos(eval(l6, env)).toFloat
  }

  // Determine the derivative of expression t with respect to variable v
  def derive(t: Tree, v: String): Tree = t match {
    case Sum(l,r)  => Sum(derive(l,v), derive(r,v))
    case Var(n) if (v == n) => Const(1.0f)
case prod(l1,r1)  => prod(derive(l1,v), derive(r1,v))
case div(l3,r3)  => div(derive(l3,v), derive(r3,v))
case sub(l2,r2)  => sub(derive(l2,v), derive(r2,v))
case Neg(l4)  => Neg(derive(l4, v))
case Sin(l5) => prod(cos(l5), derive(l5, v))
case cos(l5) => cos(l5)
case Cos(l6) => prod(Const(-1), prod(sin(l6), derive(l6, v)))
case sin(l6) => sin(l6)
case _         => Const(0.0f)
  }


def simplify(t: Tree): Tree = {
  val reduced = t match {
    case prod(l, r)  => prod(simplify(l), simplify(r))
    case Sum(l, r)    => Sum(simplify(l), simplify(r))
    case sub(l, r)  => sub(simplify(l), simplify(r))
    case div(l, r) => div(simplify(l), simplify(r))
case Sin(l) => Sin(simplify(l))
case Cos(l) => Cos(simplify(l))
case Neg(l) => Neg(simplify(l))
    case Var(n)       =>(t)
    case _            => t
  }

  reduced match {
    case prod(Const(1), r)         => r
    case prod(l, Const(1))         => l
    case prod(Const(0), r)         => Const(0)
    case prod(l, Const(0))         => Const(0)
    case prod(Const(l), Const(r))  => Const(l * r)
    case Sum(Const(0), r)           => r
    case Sum(l, Const(0))           => l
    case Sum(Const(l), Const(r))    => Const(l + r)
    case sub(l, Const(0))         => l
    case sub(l, r) if l == r      => Const(0)
    case sub(Const(l), Const(r))  => Const(l - r)
    case div(Const(0), r)        => Const(0)
    case div(l, Const(1))        => l
    case div(l, r) if l == r     => Const(1)
    case div(Const(l), Const(r)) => Const(l / r)

case Sin(Const(l)) => Const(Math.sin(l).toFloat)
case Sin(l) => Sin(l)
case Cos(Const(l)) => Const(Math.cos(l).toFloat)
case Cos(l) => Cos(l)
case Neg(Const(l)) => Const(Math.negateExact(l.toLong).toFloat)
case Neg(l) => Neg(l)
    case _                          => reduced
  }
}

def main(args: Array[String])
{
val env: Environment = { case "x" => 5 case "y" => 7 }

    println(
    "Begin testing expression tree operations -- Pattern-matching version")

    val c0:  Tree = Const(0.00f)
    val c1:  Tree = Const(1.0f)
    val c3:  Tree = Const(3.0f)
    val c6:  Tree = Const(6.0f)
    val c7:  Tree = Const(7.0f)
    val cm3: Tree = Const(-3.0f)
println("Expression: " + c0)
    println("Evaluation with x=5, y=7: "  + eval(c0, env))

    println("Derivative relative to x:\n" + derive(c0, "x"))
    println("Derivative relative to y:\n" + derive(c0, "y"))
println("Simplification:\n"           + simplify(c0))
    println("")

    println("Expression: "+ cm3)
    println("Evaluation with x=5, y=7: "  + eval(cm3, env))
println(" evaluation with x=5, y=7: " + eval(c3, env))
    println("Derivative relative to x:\n" + derive(cm3, "x"))
    println("Derivative relative to y:\n" + derive(cm3, "y"))
println("Simplification:\n"           + simplify(cm3))
    println("")

    val x: Tree = Var("x")
    val y: Tree = Var("y")
    val z: Tree = Var("z")  /* no value in env */

  println("Expression: "                + x)
    println("Evaluation with x=5, y=7: "  + eval(x, env))
    println("Derivative relative to x:\n" + derive(x, "x"))
    println("Derivative relative to y:\n" + derive(x, "y"))
println("Simplification:\n"           + simplify(x))
    println("")

    println("Expression: "                + z)
//  Undefined variable.  No provision currently to  handle this.
//  println("Evaluation with x=5, y=7: "  + eval(z, env))
    println("Derivative relative to x:\n" + derive(z, "x"))
    println("Derivative relative to y:\n" + derive(z, "y"))
//  println("Simplification:\n" + simplify(z))
    println("")

    val s0L: Tree = Sum(c0,c3)
    val s0R: Tree = Sum(c3,c0)
    val s1:  Tree = Sum(c7,cm3)
    val s2:  Tree = Sum(c1,y)
    val s3:  Tree = Sum(x,c3)
    val s4:  Tree = Sum(x,y)
    val s5:  Tree = Sum(s1,s0R)
    val s6:  Tree = Sum(Sum(s1,s2),Sum(s1,s4))

val n1: Tree = Neg(Sum(x, c7))
val n2: Tree = Neg(Sum(cm3, c1))

val sin1 : Tree = Sin(x)

    println("Expression: "                + s0L)
    println("Evaluation with x=5, y=7: "  + eval(s0L, env))
    println("Derivative relative to x:\n" + derive(s0L, "x"))
    println("Derivative relative to y:\n" + derive(s0L, "y"))
 println("Simplification:\n" + simplify(s0L))
    println("")

    println("Expression: "                + s0R)
    println("Evaluation with x=5, y=7: "  + eval(s0R, env))
    println("Derivative relative to x:\n" + derive(s0R, "x"))
    println("Derivative relative to y:\n" + derive(s0R, "y"))
 println("Simplification:\n" + simplify(s0R))
    println("")
println("Expression: "                + s1)
    println("Evaluation with x=5, y=7: "  + eval(s1, env))
    println("Derivative relative to x:\n" + derive(s1, "x"))
    println("Derivative relative to y:\n" + derive(s1, "y"))
println("Simplification:\n" + simplify(s1))
    println("")

    println("Expression: "                + s2)
    println("Evaluation with x=5, y=7: "  + eval(s2, env))
    println("Derivative relative to x:\n" + derive(s2, "x"))
    println("Derivative relative to y:\n" + derive(s2, "y"))
println("Simplification:\n" + simplify(s2))
    println("")

    println("Expression: "                + s3)
    println("Evaluation with x=5, y=7: "  + eval(s3, env))
println(" evaluation with x=5, y=7: " + eval(c3, env))
    println("Derivative relative to x:\n" + derive(s3, "x"))
    println("Derivative relative to y:\n" + derive(s3, "y"))
println("Simplification:\n" + simplify(s3))
    println("")

    println("Expression: "                + s4)
    println("Evaluation with x=5, y=7: "  + eval(s4, env))
    println("Derivative relative to x:\n" + derive(s4, "x"))
    println("Derivative relative to y:\n" + derive(s4, "y"))
println("Simplification:\n" + simplify(s4))
    println("")

    println("Expression: "                + s5)
    println("Evaluation with x=5, y=7: "  + eval(s5, env))
    println("Derivative relative to x:\n" + derive(s5, "x"))
    println("Derivative relative to y:\n" + derive(s5, "y"))
println("Simplification:\n" + simplify(s5))
    println("")

    println("Expression: "                + s6)
    println("Evaluation with x=5, y=7: "  + eval(s6, env))
    println("Derivative relative to x:\n" + derive(s6, "x"))
    println("Derivative relative to y:\n" + derive(s6, "y"))
 println("Simplification:\n" + simplify(s6))
    println("")

    val exp: Tree = Sum(Sum(x,x),Sum(c7,y))

    println("Expression: " + exp)
    println("Evaluation with x=5, y=7: "  + eval(exp, env))
    println("Derivative relative to x:\n" + derive(exp, "x"))
    println("Derivative relative to y:\n" + derive(exp, "y"))
    println("")
println("expression" + n1)  
println("evaluation" + eval(n1, env))



val c20: Tree = Cos(c3)
val s20: Tree = Sin(c20)
val s21: Tree = Sin(x)



 
val exp1: Tree = sub(Cos(Sum(x, Const(1))), Sin(Const(5)))
val exp2: Tree = sub(prod(Const(4), Const(4)), div(Const(4), Const(2)))
val exp3: Tree = prod(Cos(sub(x, y)), Neg(Const(5)))
val exp4: Tree = div(Sum(x,y),div(x,y))
val exp5: Tree =  prod(Neg(Const(5)), div(x, Neg(Const(3))))
val exp6: Tree = Sin(Cos(Sum(div(x, y), Neg(x))))
val exp7: Tree = Cos(Neg(div(x, Sum(Const(5), x))))

println("Expression: " + exp1)
    println("Evaluation with x=5, y=7: "  + eval(exp1, env))
    println("Derivative relative to x:\n" + derive(exp1, "x"))
    println("Derivative relative to y:\n" + derive(exp1, "y"))
    println("")

println("Expression: " + exp2)
    println("Evaluation with x=5, y=7: "  + eval(exp2, env))
    println("Derivative relative to x:\n" + derive(exp2, "x"))
    println("Derivative relative to y:\n" + derive(exp2, "y"))
    println("")

println("Expression: " + exp3)
    println("Evaluation with x=5, y=7: "  + eval(exp3, env))
    println("Derivative relative to x:\n" + derive(exp3, "x"))
    println("Derivative relative to y:\n" + derive(exp3, "y"))
    println("")

println("Expression: " + exp4)
    println("Evaluation with x=5, y=7: "  + eval(exp4, env))
    println("Derivative relative to x:\n" + derive(exp4, "x"))
    println("Derivative relative to y:\n" + derive(exp4, "y"))
    println("")

println("Expression: " + exp5)
    println("Evaluation with x=5, y=7: "  + eval(exp5, env))
    println("Derivative relative to x:\n" + derive(exp5, "x"))
    println("Derivative relative to y:\n" + derive(exp5, "y"))
    println("")

println("Expression: " + exp6)
    println("Evaluation with x=5, y=7: "  + eval(exp6, env))
    println("Derivative relative to x:\n" + derive(exp6, "x"))
    println("Derivative relative to y:\n" + derive(exp6, "y"))
    println("")

println("Expression: " + exp7)
    println("Evaluation with x=5, y=7: "  + eval(exp7, env))
    println("Derivative relative to x:\n" + derive(exp7, "x"))
    println("Derivative relative to y:\n" + derive(exp7, "y"))
    println("")



}
}
