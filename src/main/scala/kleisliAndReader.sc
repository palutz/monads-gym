import scalaz.{Category, Endomorphic, Kleisli}


// trying to use kleisli functions
val f = Kleisli { (x: Int) => Some(x + 10) }
val g = Kleisli { (y: Int) => Some(y * y)}


val f1 = Some((_: Int) + 1)

val f2 = (x: Int) => Some(x + 1)


val kff = Kleisli { f2 }

val kf = Kleisli[Option, Int, Int] {
  case i if i % 2 == 0 => Some(i * 3)
}

def applyX10[Arr[_, _]: Category, A](f: Arr[A, A]) =
  List.fill(10)(Endomorphic(f))
