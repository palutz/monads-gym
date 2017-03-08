sealed trait Validation[E, A] {
  def map[B](f: A => B): Validation[E, B]
  def flatMap[B](f: A => Validation[E, B]) : Validation[E, B]
  def liftFail[F](f: E => F): Validation[F, A]  // not related to Monad
}

case class Success[E, A](a: A) extends Validation[E, A] {
  def map[B](f: A => B): Validation[E, B] = new Success[E, B](f(a))
  def flatMap[B](f: A => Validation[E, B]) : Validation[E, B] = f(a)
  def liftFail[F](f: E => F): Validation[F, A]  = new Success(a)
}

case class Failure[E, A](e: E) extends Validation[E, A] {
  def map[B](f: A => B): Validation[E, B] = new Failure(e)
  def flatMap[B](f: A => Validation[E, B]) : Validation[E, B] = new Failure(e)
  def liftFail[F](f: E => F): Validation[F, A]  = new Failure(f(e))
}

class BarException extends Exception
class BazException extends Exception
class ComputeException extends Exception

class Foo { def bar: Validation[BarException, Bar] = Success[BarException, Bar]( new Bar()) }
class Bar { def baz: Validation[BazException, Baz] = Success[BazException, Baz](new Baz()) }
class Baz { def compute(a: Int) : Validation[ComputeException, Int] = Success[ComputeException, Int](a) }

def compute(a: Int, b: Int)(foo: Foo) : Validation[ComputeException, Int] = {
  //foo.bar.liftFail[ComputeException] { b => new ComputeException() }.flatMap { bar =>
//  foo.bar.liftFail { _ => new ComputeException() }.flatMap { bar =>
//    // bar.baz.liftFail[ComputeException] { be => new ComputeException() }.flatMap { baz =>
//    bar.baz.liftFail { _ => new ComputeException() }.flatMap { baz =>
//      baz.compute(a + b)
//    }
//  }
  // or with for comprehension
  for {
    bar <- foo.bar.liftFail { _ => new ComputeException() }
    baz <- bar.baz.liftFail { _ => new ComputeException() }
    c <- baz.compute(a + b)
  } yield c
  // this doesn't work: they are not the same Monad(ic) type
//  for {
//    bar <- foo.bar
//    baz <- bar.baz
//    c <- baz.compute(a)
//  } yield c
}

val barF: Failure[BarException, Bar] = new Failure(new BarException()) // barF: Failure[BarException,Bar] = Failure(A$A204$A$A204$BarException)
barF.liftFail(_ => new ComputeException()) // Validation[ComputeException,Bar] = Failure(A$A204$A$A204$ComputeException)

val bb = (new Foo()).bar.liftFail { _ => new ComputeException() }  // Validation[ComputeException, Bar] = Success(A$A204$A$A204$Bar@2884475d)

compute(2, 1)(new Foo()) match {
  case Success(x) => x.toString
  case Failure(y) => y.getMessage
} // res1: String = 3