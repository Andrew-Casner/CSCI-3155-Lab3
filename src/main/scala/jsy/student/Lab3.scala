package jsy.student

import jsy.lab3.Lab3Like
import jsy.util.JsyApplication

object Lab3 extends JsyApplication with Lab3Like {
  import jsy.lab3.ast._

  /*
   * CSCI 3155: Lab 3
   * <Your Name>
   *
   * Partner: <Your Partner's Name>
   * Collaborators: <Any Collaborators>
   */

  /*
   * Fill in the appropriate portions above by replacing things delimited
   * by '<'... '>'.
   *
   * Replace the '???' expression with your code in each function.
   *
   * Do not make other modifications to this template, such as
   * - adding "extends App" or "extends Application" to your Lab object,
   * - adding a "main" method, and
   * - leaving any failing asserts.
   *
   * Your lab will not be graded if it does not compile.
   *
   * This template compiles without error. Before you submit comment out any
   * code that does not compile or causes a failing assert. Simply put in a
   * '???' as needed to get something  that compiles without error. The '???'
   * is a Scala expression that throws the exception scala.NotImplementedError.
   */

  /*
   * The implementations of these helper functions for conversions can come
   * Lab 2. The definitions for the new value type for Function are given.
   */

  def toNumber(v: Expr): Double = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => n
      case B(true) => 1.0
      case B(false) => 0.0
      case Undefined => Double.NaN
      case S(s) => try s.toDouble catch {case _: Throwable => Double.NaN}
      case Function(_, _, _) => Double.NaN
      case _ => throw new UnsupportedOperationException
    }
  }

  def toBoolean(v: Expr): Boolean = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => if(n == 0.0 || n == -0.0 || n.isNaN) false else true
      case B(b) => b
      case Undefined => false
      case S(s) => if(s=="") false else true
      case Function(_, _, _) => true
      case _ => throw new UnsupportedOperationException
    }
  }

  def toStr(v: Expr): String = {
    require(isValue(v))
    (v: @unchecked) match {
      case N(n) => if(n.isWhole) "%.0f" format n else n.toString
      case B(b) => b.toString
      case Undefined => "undefined"
      case S(s) => s
      case Function(_, _, _) => "function"
      case _ => throw new UnsupportedOperationException
    }
  }

  /*
   * Helper function that implements the semantics of inequality
   * operators Lt, Le, Gt, and Ge on values.
   *
   * We suggest a refactoring of code from Lab 2 to be able to
   * use this helper function in eval and step.
   */
  def inequalityVal(bop: Bop, v1: Expr, v2: Expr): Boolean = {
    require(isValue(v1))
    require(isValue(v2))
    require(bop == Lt || bop == Le || bop == Gt || bop == Ge)
    (v1, v2) match {
      case (S(_), S(_)) => bop match{
        case Lt         => toStr(v1) < toStr(v2)
        case Le         => toStr(v1) <= toStr(v2)
        case Gt         => toStr(v1) > toStr(v2)
        case Ge         => toStr(v1) >= toStr(v2)
      }
      case _            => bop match{
        case Lt         => toNumber(v1) < toNumber(v2)
        case Le         => toNumber(v1) <= toNumber(v2)
        case Gt         => toNumber(v1) > toNumber(v2)
        case Ge         => toNumber(v1) >= toNumber(v2)
      }
    }
  }


  /* Big-Step Interpreter with Dynamic Scoping */

  /*
   * Start by copying your code from Lab 2 here.
   */
  def eval(env: Env, e: Expr): Expr = {
    e match {
      /* Base Cases */
      case N(_) | B(_) | S(_) | Undefined | Function(_, _, _) => e
      case Var(x) => lookup(env, x)

      /* Inductive Cases */
      case Unary(uop, e1) => {
        uop match {
          case Neg        => N(-1 * toNumber(eval(env,e1)))
          case Not        => B(!toBoolean(eval(env,e1)))
        }
      }
      case Binary(bop, e1, e2)        => {
        bop match {
          case Plus                   => (eval(env,e1),eval(env,e2)) match {
            case (S(s), _)            => S(toStr(eval(env,e1)) + toStr(eval(env,e2)))
            case (_, S(s))            => S(toStr(eval(env,e1)) + toStr(eval(env,e2)))
            case _                    => N(toNumber(eval(env,e1)) + toNumber(eval(env,e2)))
          }
          case Minus                  => N(toNumber(eval(env,e1)) - toNumber(eval(env,e2)))
          case Times                  => N(toNumber(eval(env,e1)) * toNumber(eval(env,e2)))
          case Div                    => N(toNumber(eval(env,e1)) / toNumber(eval(env,e2)))
          case Eq                     => (eval(env,e1),eval(env,e2)) match {
            case (Function(_,_,_), _) => throw new DynamicTypeError(e)
            case (_, Function(_,_,_)) => throw new DynamicTypeError(e)
            case _                    => B(eval(env,e1) == eval(env,e2))
          }
          case Ne                     => (eval(env,e1),eval(env,e2)) match {
            case (Function(_,_,_), _) => throw new DynamicTypeError(e)
            case (_, Function(_,_,_)) => throw new DynamicTypeError(e)
            case _                    => B(!(eval(env,e1) == eval(env,e2)))
          }
          case Lt                     => B(inequalityVal(Lt, eval(env,e1), eval(env, e2)))
          case Le                     => B(inequalityVal(Le, eval(env,e1), eval(env, e2)))
          case Gt                     => B(inequalityVal(Gt, eval(env,e1), eval(env, e2)))
          case Ge                     => B(inequalityVal(Ge, eval(env,e1), eval(env, e2)))
          case And                    => if(toBoolean(eval(env,e1))) eval(env,e2) else eval(env,e1)
          case Or                     => if(toBoolean(eval(env,e1))) eval(env,e1) else eval(env,e2)
          case Seq                    => eval(env,e1); eval(env,e2)
        }
      }
      case If(e1, e2, e3)             => if(toBoolean(eval(env,e1))) eval(env,e2) else eval(env,e3)
      case ConstDecl(x, e1, e2)       => eval(extend(env,x,eval(env,e1)), e2)
      case Print(e1)                  => println(pretty(eval(env, e1))); Undefined

      // ****** Your cases here

      case Call(e1, e2)      => eval(env, e1) match {
        case Function(a,x,c) => a match {
          case None          => eval(extend(env,x,eval(env,e2)),c)
          case Some(s)       => eval(extend(extend(env,s,eval(env,e1)),x,eval(env,e2)),c)
          case _             => throw new DynamicTypeError(e)
        }
        case _               => throw new DynamicTypeError(e)
      }
      case _ => ??? // delete this line when done
    }
  }


  /* Small-Step Interpreter with Static Scoping */

  def iterate(e0: Expr)(next: (Expr, Int) => Option[Expr]): Expr = {
    def loop(e: Expr, n: Int): Expr = next(e,n) match {
      case None     => e
      case Some(ep) => loop(ep, n+1)
    }
    loop(e0, 0)
  }

  def substitute(e: Expr, v: Expr, x: String): Expr = {
    require(isValue(v))
    e match {
      case N(_) | B(_) | Undefined | S(_) => e
      case Print(e1)                      => Print(substitute(e1, v, x))
      case Unary(uop, e1)                 => Unary(uop, substitute(e1,v,x))
      case Binary(bop, e1, e2)            => Binary(bop, substitute(e1,v,x), substitute(e2,v,x))
      case If(e1, e2, e3)                 => If(substitute(e1,v,x), substitute(e2,v,x), substitute(e3,v,x))
      case Call(e1, e2)                   => Call(substitute(e1,v,x), substitute(e2,v,x))
      case Var(y)                         => if(x==y) v else e
      case Function(None, y, e1)          => if(x==y) e else Function(None, y, substitute(e1,v,x))
      case Function(Some(y1), y2, e1)     => if(x==y1 | x==y2) e else Function(Some(y1),y2,substitute(e1,v,x))
      case ConstDecl(y, e1, e2)           => if(x==y) ConstDecl(y,substitute(e1,v,x),e2) else ConstDecl(y,substitute(e1,v,x),substitute(e2,v,x))
      case _                              => throw new DynamicTypeError(e)
    }
  }

  def step(e: Expr): Expr = {
    e match {
      /* Base Cases: Do Rules */
      case Unary(uop,v1) if isValue(v1) =>
        uop match {
          case Neg => N(toNumber(v1) * -1)
          case Not => B(!toBoolean(v1))
        }
      case Binary(bop,v1,v2) if isValue(v1) && isValue(v2) =>
        bop match {
          case Plus                   => (v1,v2) match{
            case (S(_),_)             => S(toStr(v1) + toStr(v2))
            case (_,S(_))             => S(toStr(v1) + toStr(v2))
            case (_,_)                => N(toNumber(v1) + toNumber(v2))
          }
          case Minus                  => N(toNumber(v1) - toNumber(v2))
          case Times                  => N(toNumber(v1) * toNumber(v2))
          case Div                    => N(toNumber(v1) / toNumber(v2))
          case Lt                     => B(inequalityVal(Lt,v1,v2))
          case Le                     => B(inequalityVal(Le,v1,v2))
          case Gt                     => B(inequalityVal(Gt,v1,v2))
          case Ge                     => B(inequalityVal(Ge,v1,v2))
          case Eq                     => (v1,v2) match{
            case (Function(_,_,_), _) => throw new DynamicTypeError(e)
            case (_, Function(_,_,_)) => throw new DynamicTypeError(e)
            case _                    => B(v1 == v2)
          }
          case Ne                     => (v1,v2) match{
            case (Function(_,_,_), _) => throw new DynamicTypeError(e)
            case (_, Function(_,_,_)) => throw new DynamicTypeError(e)
            case _                    => B(v1 != v2)
          }
        }
      case Binary(Or,v1,e2) if isValue(v1) => if (toBoolean(v1)) v1 else e2
      case Binary(And,v1,e2) if isValue(v1) => if (toBoolean(v1)) e2 else v1
      case Binary(Seq,v1,e2) if isValue(v1) => e2
      case Print(v1) if isValue(v1)          => println(pretty(v1)); Undefined
      case If(v1,e2,e3) if isValue(v1)       => if(toBoolean(v1)) e2 else e3
      case ConstDecl(x,v1,e2) if isValue(v1) => substitute(e2,v1,x)
      case Call(v1, v2) if isValue(v1) && isValue(v2) => v1 match {
        case Function(p, x, e1) => p match{
          case None => substitute(e1, v2, x)
          case Some(f) => substitute(substitute(e1,v1,f), v2, x)
          case _ => throw new DynamicTypeError(e)
        }
        case _ => throw new DynamicTypeError(e)
      }

      /* Inductive Cases: Search Rules */
      case Unary(uop,e1) => Unary(uop,step(e1))
      case Binary(bop,e1,e2) if !isValue(e1) => Binary(bop,step(e1),e2)
      case Binary(bop,v1,e2) if isValue(v1) => Binary(bop,v1,step(e2))
      case Print(e1) => Print(step(e1))
      case If(e1,e2,e3) => If(step(e1),e2,e3)
      case ConstDecl(x,v1,e2) =>  ConstDecl(x,step(v1),e2)
      case Call(e1, e2) if isValue(e1)=> e1 match {
        case Function(_,_,_) => Call(e1, step(e2))
        case _ =>  throw new DynamicTypeError(e)
      }
      case Call(e1, e2) => Call(step(e1), e2)
      // ****** Your cases here

      /* Cases that should never match. Your cases above should ensure this. */
      case Var(_) => throw new AssertionError("Gremlins: internal error, not closed expression.")
      case N(_) | B(_) | Undefined | S(_) | Function(_, _, _) => throw new AssertionError("Gremlins: internal error, step should not be called on values.");
    }
  }


  /* External Interfaces */

  //this.debug = true // uncomment this if you want to print debugging information
  this.keepGoing = true // comment this out if you want to stop at first exception when processing a file

}