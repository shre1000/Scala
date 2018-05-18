
import java.lang.Math
abstract class Tree {
  type Environment = String => Float

  // Evaluate this expression in environment env
  def eval(env: Environment): Float

  // Determine the derivative of this expression with respect to variable v
  def derive(v: String): Tree

  // Simplify this expression by replacing constant subexpressions by a 
  // constant
def simplify: Tree

}

/* Addition operator subclass Sum */

class Sum(l: Tree, r: Tree) extends Tree {

  def eval(env: Environment) = l.eval(env) + r.eval(env)

  def derive(v: String)      = new Sum(l.derive(v), r.derive(v))
def simplify = new Sum(l.simplify, r.simplify)


  // Override definition of toString from Any
  override def toString = "Sum(" + l + "," + r + ")"
}

class sub(l: Tree, r: Tree) extends Tree {

  def eval(env: Environment) = l.eval(env) + r.eval(env)

  def derive(v: String)      = new sub(l.derive(v), r.derive(v))

  // Override definition of toString from Any
  override def toString = "sub(" + l + "," + r + ")"

def simplify = new sub(l.simplify, r.simplify)

}
class prod(l: Tree, r: Tree) extends Tree {

  def eval(env: Environment) = l.eval(env) + r.eval(env)

  def derive(v: String)      = new prod(l.derive(v), r.derive(v))

  // Override definition of toString from Any
  override def toString = "prod(" + l + "," + r + ")"

def simplify = new prod(l.simplify, r.simplify)

}

class div(l: Tree, r: Tree) extends Tree {

  def eval(env: Environment) = l.eval(env) + r.eval(env)

  def derive(v: String)      = new div(l.derive(v), r.derive(v))

  // Override definition of toString from Any
  override def toString = "div(" + l + "," + r + ")"

def simplify = new div(l.simplify, r.simplify)

}
class Neg(l: Tree) extends Tree {

  def eval(env: Environment) = - l.eval(env)

  def derive(v: String)      = new Neg(l.derive(v))

  // Override definition of toString from Any
  override def toString = "Neg(" + l + ")"

def simplify = new Neg(l.simplify)
}

class Sin(l: Tree) extends Tree {


  def eval(env: Environment) = Math.sin(l.eval(env)).toFloat

 
  def derive(v: String)      = new prod(new Cos1(l), new Sin(l.derive(v)))



  // Override definition of toString from Any
 override def toString = "Sin(" + l + ")"

def simplify = new Sin(l.simplify)

}


class Cos1(l: Tree) extends Tree
{
def eval(env: Environment) = l.eval(env)
def derive(v: String) = new Cos1(l.derive(v))
override def toString = "Cos(" + l + ")"

def simplify = new Cos1(l.simplify)
}

class Cos(l: Tree) extends Tree {


  def eval(env: Environment) = Math.cos(l.eval(env)).toFloat

 // Override definition of toString from Any
 
  def derive(v: String)      = new Neg(new prod(new Sin1(l), new Cos(l.derive(v))))

override def toString = "Cos("+ l + ")" 

def simplify = new Cos(l.simplify)

}

class Sin1(l: Tree) extends Tree
{
def eval(env: Environment) = l.eval(env)
def derive(v: String) = new Sin1(l.derive(v))
override def toString = "Sin(" + l + ")"

def simplify = new Sin1(l.simplify)
}

 


/* Variable (name) subclass Var */

class Var(n: String) extends Tree {

  def eval(env: Environment) = env(n)

  def derive(v: String)      = 
    if (v == n) new Const(1.0f) else new Const(0.0f)

  // Accessor for name attribute
  def getn                   = n

  override def toString = "Var(" + n + ")"

def simplify = new Var(n)
}


/* Constant (value) subclass Const */

class Const(v: Float) extends Tree {

  def eval(env: Environment) = v.toFloat

  def derive(v: String)      = new Const(0.0f)

  // Accessor for value  attribute
  def getv                   = v.toFloat

  override def toString = "Const(" + v + ")"

def simplify = new Const(v)
}


/* Main method for testing the expression Tree hierarchy  */

object ExprObj {
  
  type Environment = String => Float

  def main(args: Array[String]) {

    val env: Environment = { case "x" => 5f case "y" => 7f }

    println("Begin testing expression tree program -- Subclass version")

    val c0:  Tree = new Const(0.0f)
    val c1:  Tree = new Const(1.0f)
    val c3:  Tree = new Const(3.0f)
    val c6:  Tree = new Const(6.0f)
    val c7:  Tree = new Const(7.0f)
    val cm3: Tree = new Const(-3.0f)

    println("Expression: " + c0)
    println("Evaluation with x=5, y=7: "  + c0.eval(env))
    println("Derivative relative to x:\n" + c0.derive("x"))
    println("Derivative relative to y:\n" + c0.derive("y"))
println("Simplification\n" + c0.simplify)
    println(" ")

    println("Expression: "                + cm3)
    println("Evaluation with x=5, y=7: "  + cm3.eval(env))
    println("Derivative relative to x:\n" + cm3.derive("x"))
    println("Derivative relative to y:\n" + cm3.derive("y"))
println("Simplification\n" + cm3.simplify)
    println(" ")

    val x: Tree   = new Var("x")
    val y: Tree   = new Var("y")
    val z: Tree   = new Var("z")  /* no value in env */

    println("Expression: "                + x)
    println("Evaluation with x=5, y=7: "  + x.eval(env))
    println("Derivative relative to x:\n" + x.derive("x"))
    println("Derivative relative to y:\n" + x.derive("y"))
println("Simplification\n"            + x.simplify)
    println(" ")

    //println("Expression: "                + z)
//Undefined variable.  There is no provision currently to handle this.
//println("Evaluation with x=5, y=7: "  + z.eval(env))
    //println("Derivative relative to x:\n" + z.derive("x"))
   // println("Derivative relative to y:\n" + z.derive("y"))
//println("Simplification\n"            + z.simplify)
    println(" ")

    val s0L: Tree = new Sum(c0,c3)
    val s0R: Tree = new Sum(c3,c0)
    val s1:  Tree = new Sum(c7,cm3)
    val s2:  Tree = new Sum(c1,y)
    val s3:  Tree = new Sum(x,c3)
    val s4:  Tree = new Sum(x,y)
    val s5:  Tree = new Sum(s1,s0L)
    val s6:  Tree = new Sum(new Sum(s1,s2),new Sum(s1,s4))

    println("Expression: "                + s0L)
    println("Evaluation with x=5, y=7: "  + s0L.eval(env))
    println("Derivative relative to x:\n" + s0L.derive("x"))
    println("Derivative relative to y:\n" + s0L.derive("y"))
println("Simplification\n"            + s0L.simplify)
    println(" ")

    println("Expression: "                + s0R)
    println("Evaluation with x=5, y=7: "  + s0R.eval(env))
    println("Derivative relative to x:\n" + s0R.derive("x"))
    println("Derivative relative to y:\n" + s0R.derive("y"))
println("Simplification\n"            + s0R.simplify)
    println(" ")

    println("Expression: "                + s1)
    println("Evaluation with x=5, y=7: "  + s1.eval(env))
    println("Derivative relative to x:\n" + s1.derive("x"))
    println("Derivative relative to y:\n" + s1.derive("y"))
println("Simplification\n" + s1.simplify)
    println(" ")

    println("Expression: "                + s2)
    println("Evaluation with x=5, y=7: "  + s2.eval(env))
    println("Derivative relative to x:\n" + s2.derive("x"))
    println("Derivative relative to y:\n" + s2.derive("y"))
println("Simplification\n"            + s2.simplify)
    println(" ")

    println("Expression: "                + s3)
    println("Evaluation with x=5, y=7: "  + s3.eval(env))
    println("Derivative relative to x:\n" + s3.derive("x"))
    println("Derivative relative to y:\n" + s3.derive("y"))
println("Simplification\n"            + s3.simplify)
    println(" ")

    println("Expression: "                + s4)
    println("Evaluation with x=5, y=7: "  + s4.eval(env))
    println("Derivative relative to x:\n" + s4.derive("x"))
    println("Derivative relative to y:\n" + s4.derive("y"))
 println("Simplification\n"            + s4.simplify)
    println(" ")

    println("Expression: "                + s5)
    println("Evaluation with x=5, y=7: "  + s5.eval(env))
    println("Derivative relative to x:\n" + s5.derive("x"))
    println("Derivative relative to y:\n" + s5.derive("y"))
println("Simplification\n"            + s5.simplify)
    println(" ")

    println("Expression: "                + s6)
    println("Evaluation with x=5, y=7: "  + s6.eval(env))
    println("Derivative relative to x:\n" + s6.derive("x"))
    println("Derivative relative to y:\n" + s6.derive("y"))
println("Simplification\n"            + s6.simplify)
    println(" ")

    val exp: Tree = new Sum(new Sum(x,x),new Sum(c7,y))

    println("Expression: "                + exp)
    println("Evaluation with x=5, y=7: "  + exp.eval(env))
    println("Derivative relative to x:\n" + exp.derive("x"))
    println("Derivative relative to y:\n" + exp.derive("y"))
    println(" ")


val exp1: Tree = new sub(new Cos(new Sum(x, new Const(1))), new Sin(new Const(5)))
val exp2: Tree = new sub(new prod(new Const(4), new Const(4)), new div(new Const(4), new Const(2)))
val exp3: Tree = new prod(new Cos(new sub(x, y)), new Neg(new Const(5)))
val exp4: Tree = new div(new Sum(x,y),new div(x,y))
val exp5: Tree = new prod(new Neg(new Const(5)), new div(x, new Neg(new Const(3))))
val exp6: Tree = new Sin(new Cos(new Sum(new div(x, y), new Neg(x))))
val exp7: Tree = new Cos(new Neg(new div(x, new Sum(new Const(5), x))))

println("Expression: "                + exp1)
    println("Evaluation with x=5, y=7: "  + exp1.eval(env))
    println("Derivative relative to x:\n" + exp1.derive("x"))
    println("Derivative relative to y:\n" + exp1.derive("y"))
    println(" ")

println("Expression: "                + exp2)
    println("Evaluation with x=5, y=7: "  + exp2.eval(env))
    println("Derivative relative to x:\n" + exp2.derive("x"))
    println("Derivative relative to y:\n" + exp2.derive("y"))
    println(" ")

println("Expression: "                + exp3)
    println("Evaluation with x=5, y=7: "  + exp3.eval(env))
    println("Derivative relative to x:\n" + exp3.derive("x"))
    println("Derivative relative to y:\n" + exp3.derive("y"))
    println(" ")

println("Expression: "                + exp4)
    println("Evaluation with x=5, y=7: "  + exp4.eval(env))
    println("Derivative relative to x:\n" + exp4.derive("x"))
    println("Derivative relative to y:\n" + exp4.derive("y"))
    println(" ")

println("Expression: "                + exp5)
    println("Evaluation with x=5, y=7: "  + exp5.eval(env))
    println("Derivative relative to x:\n" + exp5.derive("x"))
    println("Derivative relative to y:\n" + exp5.derive("y"))
    println(" ")

println("Expression: "                + exp6)
    println("Evaluation with x=5, y=7: "  + exp6.eval(env))
    println("Derivative relative to x:\n" + exp6.derive("x"))
    println("Derivative relative to y:\n" + exp6.derive("y"))
    println(" ")

println("Expression: "                + exp7)
    println("Evaluation with x=5, y=7: "  + exp7.eval(env))
    println("Derivative relative to x:\n" + exp7.derive("x"))
    println("Derivative relative to y:\n" + exp7.derive("y"))
    println(" ")

}
}

