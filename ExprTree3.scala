



abstract class Tree {
  type Environment = String => Float

  // Evaluate this expression in environment env
  def eval(env: Environment): Float

  // Determine the derivative of this expression with respect to variable v
  def derive(v: String): Tree

  // Simplify this expression by replacing constant subexpressions by a 
  // constant



//def simplify: Tree
/*
{

if (t == Sum(Const(0), r)) r
if (t == Sum(l, Const(0))) l
if (t == Sum(Const(l), Const(r))) new Const(l + r)
if (t == sub(l2, Const(0))) l2
if (t == sub(l2, r2)) if (l2 == r2)  Const(0)
if (t == sub(Const(l2), Const(r2))) new Const(l2 - r2)
if (t == div(Const(0), r3)) Const(0)
if (t == div(l3, Const(13))) l3
if (t == div(l3, r3)) if (l3 == r3) new Const(13)
if (t == div(Const(l3), Const(r3))) new Const(l3 / r3)
if (t== Sin(Const(l5))) new Const(Math.sin(l5).toFloat)
if(t == Sin(l5)) new Sin(l5)
if (t == Cos(Const(l6))) new Const(Math.cos(l6).toFloat)
if (t == Cos(l6)) new Cos(l6)
if (t == Neg(Const(r4))) new Const(Math.negateExact(r4.toLong).toFloat)
if (t == Neg(r4)) new Neg(r4)
if (t == Var(n)) t
if( t == Const(v)) new Const(v) 

}*/

}


/* Addition operator subclass Sum */

class Sum(l: Tree, r: Tree) extends Tree {

  def eval(env: Environment) = l.eval(env) + r.eval(env)

  def derive(v: String)      = new Sum(l.derive(v), r.derive(v))
 // def simplify = new Sum(l.simplify, r.simplify)

 

  // Override definition of toString from Any
  override def toString = "Sum(" + l + "," + r + ")"
}

/* Variable (name) subclass Var */

class Var(n: String) extends Tree {

  def eval(env: Environment) = env(n)

  def derive(v: String)      = 
    if (v == n) new Const(1.0f) else new Const(0.0f)

  // Accessor for name attribute
  def getn                   = n

//def simplify = new Var(n.simplify)
  override def toString = "Var(" + n + ")"
}


/* Constant (value) subclass Const */

class Const(v: Float) extends Tree {

  def eval(env: Environment) = vf 

  def derive(v: String)      = new Const(0.0f)

 // def derive(v: String)      = new Const(-1.0f)

//def simplify = new Const(1.0f)
//def simplify = new Const(0.0f)


  // Accessor for value  attribute
  def getv                   = v
//def simplify = new const(v.simplify)

  override def toString = "Const(" + v + ")"
}

class prod(l1 : Tree, r1 : Tree) extends Tree
{
def eval(env: Environment) = l1.eval(env) * r1.eval(env)
def derive(v: String)      = new prod(l1.derive(v), r1.derive(v))
//def simplify = new prod(l1.simplify, r1.simplify)

}

class sub(l2 : Tree, r2 : Tree) extends Tree // subtraction
{
def eval(env: Environment) = l2.eval(env) - r2.eval(env)
def derive(v: String)      = new sub(l2.derive(v), r2.derive(v))
//def simplify = new sub(l2.simplify, r2.simplify)
}

class div(l3 : Tree, r3 : Tree) extends Tree // division
{
def eval(env: Environment) = l3.eval(env) / r3.eval(env)
def derive(v: String)      = new div(l3.derive(v), r3.derive(v))
//def simplify = new div(l3.simplify, r3.simplify)
}

class Neg(r4 : Tree) extends Tree // negation
{
def eval(env: Environment) = - r4.eval(env)
def derive(v: String)      = new Neg(r4.derive(v))
//simplify = new Neg(r4.simplify)
}

class/* Sin(l5 : Tree) extends Tree // sin
{
def eval(env: Environment) =  Math.sin(l5.eval(env)).toFloat
def derive(v: String)      = new prod(new cos(l8), l5.derive(v))
//def simplify = new Sin(l.simplify)
}
 

class Cos(l6 : Tree) extends Tree // cos
{
def eval(env: Environment) = Math.cos(l6.eval(env)).toFloat
def derive(v: String) = new prod(Const(-1.0f), new prod(sin(l9), derive(l6, v)))
//def simplify = new Cos(l1.simplify)
}
*/
/*class cos(l8 : Tree) extends Tree
{
def derive(v: String) = cos(l8)
}

class sin(l9 : Tree) extends Tree
{
def derive(v: String) = sin(l9)
}
*/



  import java.lang.Math
 // type Environment = String => Float
/* Main method for testing the expression Tree hierarchy  */


 

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
//  println("Simplification\n" + c0.simplify)
    println(" ")

    println("Expression: "                + cm3)
    println("Evaluation with x=5, y=7: "  + cm3.eval(env))
    println("Derivative relative to x:\n" + cm3.derive("x"))
    println("Derivative relative to y:\n" + cm3.derive("y"))
//  println("Simplification\n" + cm3.simplify)
    println(" ")

    val x: Tree   = new Var("x")
    val y: Tree   = new Var("y")
    val z: Tree   = new Var("z")  /* no value in env */

    println("Expression: "                + x)
    println("Evaluation with x=5, y=7: "  + x.eval(env))
    println("Derivative relative to x:\n" + x.derive("x"))
    println("Derivative relative to y:\n" + x.derive("y"))
//  println("Simplification\n"            + x.simplify)
    println(" ")

    println("Expression: "                + z)
//  Undefined variable.  There is no provision currently to handle this.
//  println("Evaluation with x=5, y=7: "  + z.eval(env))
    println("Derivative relative to x:\n" + z.derive("x"))
    println("Derivative relative to y:\n" + z.derive("y"))
//  println("Simplification\n"            + z.simplify)
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
//  println("Simplification\n"            + s0L.simplify)
    println(" ")

    println("Expression: "                + s0R)
    println("Evaluation with x=5, y=7: "  + s0R.eval(env))
    println("Derivative relative to x:\n" + s0R.derive("x"))
    println("Derivative relative to y:\n" + s0R.derive("y"))
//  println("Simplification\n"            + s0R.simplify)
    println(" ")

    println("Expression: "                + s1)
    println("Evaluation with x=5, y=7: "  + s1.eval(env))
    println("Derivative relative to x:\n" + s1.derive("x"))
    println("Derivative relative to y:\n" + s1.derive("y"))
//  println("Simplification\n" + s1.simplify)
    println(" ")

    println("Expression: "                + s2)
    println("Evaluation with x=5, y=7: "  + s2.eval(env))
    println("Derivative relative to x:\n" + s2.derive("x"))
    println("Derivative relative to y:\n" + s2.derive("y"))
//  println("Simplification\n"            + s2.simplify)
    println(" ")

    println("Expression: "                + s3)
    println("Evaluation with x=5, y=7: "  + s3.eval(env))
    println("Derivative relative to x:\n" + s3.derive("x"))
    println("Derivative relative to y:\n" + s3.derive("y"))
//  println("Simplification\n"            + s3.simplify)
    println(" ")

    println("Expression: "                + s4)
    println("Evaluation with x=5, y=7: "  + s4.eval(env))
    println("Derivative relative to x:\n" + s4.derive("x"))
    println("Derivative relative to y:\n" + s4.derive("y"))
//  println("Simplification\n"            + s4.simplify)
    println(" ")

    println("Expression: "                + s5)
    println("Evaluation with x=5, y=7: "  + s5.eval(env))
    println("Derivative relative to x:\n" + s5.derive("x"))
    println("Derivative relative to y:\n" + s5.derive("y"))
//  println("Simplification\n"            + s5.simplify)
    println(" ")

    println("Expression: "                + s6)
    println("Evaluation with x=5, y=7: "  + s6.eval(env))
    println("Derivative relative to x:\n" + s6.derive("x"))
    println("Derivative relative to y:\n" + s6.derive("y"))
//  println("Simplification\n"            + s6.simplify)
    println(" ")

    val exp: Tree = new Sum(new Sum(x,x),new Sum(c7,y))

    println("Expression: "                + exp)
    println("Evaluation with x=5, y=7: "  + exp.eval(env))
    println("Derivative relative to x:\n" + exp.derive("x"))
    println("Derivative relative to y:\n" + exp.derive("y"))
    println(" ")

val c20: Tree = new Cos(c3)
val s20: Tree = new Sin(c20)
val s21: Tree = new Sin(x)
val c15: Tree = new Sum(x, new Sum(c6, c7))

println(s21.derive("x"))
//println(c15.simplify)


 
