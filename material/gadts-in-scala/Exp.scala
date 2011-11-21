abstract class Exp[A] {
  def eval: A
}

case class LitInt(i: Int)                                     extends Exp[Int] {
  def eval = i
}

case class LitBool(b: Boolean)                                extends Exp[Boolean] {
  def eval = b
}

case class Add(e1: Exp[Int], e2: Exp[Int])                    extends Exp[Int] {
  def eval = e1.eval + e2.eval
}
case class Mul(e1: Exp[Int], e2: Exp[Int])                    extends Exp[Int] {
  def eval = e1.eval * e2.eval
}
case class Cond[A](b: Exp[Boolean], thn: Exp[A], els: Exp[A]) extends Exp[A] {
  def eval = if ( b.eval ) { thn.eval } else { els.eval }
}
case class Eq[A](e1: Exp[A], e2: Exp[A])                      extends Exp[Boolean] {
  def eval = e1.eval == e2.eval
}