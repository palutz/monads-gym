// 1st implementation.. not secure... (if throwing exceptions))
sealed trait Boo
case class Foo(a: Int) extends Boo { def getBar : Bar = Bar(a) }
case class Bar(a1: Int) extends Boo { def getBaz(b: Int) : Baz = Baz(a1, b) }
case class Baz(a: Int, b: Int) extends Boo { def compute() : Int = a + b }
// trying to create a Monad base trait...
//trait myMonad[M[_], A] {
//  def map[B](f: A => B): M[B]
//  def flatMap[B](f: A => M[B]): M[B]
//}
//
//sealed trait Option[A] extends myMonad[Option[A], A]

// not a real base Monad trait, just an example of how it's the structure ...
trait Monad[A] {
  def map[B](f: A => B): Monad[B]
  def flatMap[B](f: A => Monad[B]): Monad[B]
}
// simple implementation of Option monad...
sealed trait Option[A] {
  def map[B](f: A => B): Option[B]
  def flatMap[B](f: A => Option[B]) : Option[B]
}

case class Some[A](a: A) extends Option[A] {
  def map[B](f: A => B): Option[B] = Some(f(a))
  def flatMap[B](f: A => Option[B]) = f(a)
}

case class None[A]() extends Option[A] {
  def map[B](f: A => B): Option[B] = None()
  def flatMap[B](f: A => Option[B]) = None()
}

sealed trait Boo2
case class Foo2() extends Boo2 { def getBar : Option[Bar2] = Some(Bar2()) }
case class Bar2() extends Boo2 { def getBaz : Option[Baz2] = Some(Baz2()) }
case class Baz2() extends Boo2 { def compute(a: Int, b: Int) : Int = a + b }

def computeBaz2(baz2: Baz2): Int = baz2.compute(1, 2)

def computeMaybeBar2(maybeBar: Option[Bar2]): Option[Int] = {
  maybeBar.flatMap(mb =>
    mb.getBaz.map ( _.compute(1, 2))
  )
}

def computeBar2(bar: Bar2): Option[Int] = {
  bar.getBaz.map (_.compute(1, 2))
}

def computeMaybeFoo2(maybeFoo: Option[Foo2]) : Option[Int] = {
  maybeFoo.flatMap(mFoo =>
    mFoo.getBar.flatMap(mBar =>
      mBar.getBaz.map( _.compute(1, 2))
    )
  )
}
// or same implementation but with for comprehension
def computing(maybeFoo: Option[Foo2], a: Int, b: Int): Option[Int] = {
  for {
    mFoo <- maybeFoo
    mBar <- mFoo.getBar
    mBar <- mBar.getBaz
  } yield mBar.compute(a, b)
}

// playing with the Scala Option
//Some("Hello").flatMap(w => Some(w + " World")) // Some(Hello World)
//Some("Hello").map(x => Some(x + " World"))   // Some(Some(...))
// *** flatMap == map + flatten ***
//Some("Hello").map(x => Some(x + " World")).flatten  // Some(Hello World)

//val l = List(Some(1), Some(2), None)
//val r = l.flatMap(x => x.toList) // List(1,2)
//
////val o = Some(List("a", "b", "c"))
//val o = None
//val q = o.toList.flatten // List[Nothing] = List()
//
//
//Option(List(1,2,3).filter(x => x > 10))
//
//List(None, None, None).flatMap(x => x.toList) match {
//  case x :: xs => Some(xs)
//  case _ => None
//}
//
//val v : Option[List[Option[Int]]] = Some(List(Some(1),Some(2))) // Some(List(Some(1), Some(2)))
//v.toList.flatMap(x => x.collect{case Some(c) => c}).sum   // 3
//v.map(x => x.collect{case Some(c) => c})   // Some(List(1,2))