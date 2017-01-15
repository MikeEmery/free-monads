import cats.{Id, ~>}
import scala.collection.mutable
import cats.free.Free
import cats.free.Free.liftF
import cats.data.State

sealed trait KVStoreA[A]
case class Put[T](key: String, value: T) extends KVStoreA[Unit]
case class Get[T](key: String) extends KVStoreA[Option[T]]
case class Delete(key: String) extends KVStoreA[Unit]

type KVStore[A] = Free[KVStoreA, A]

def put[T](key: String, value: T): KVStore[Unit] =
  liftF[KVStoreA, Unit](Put[T](key, value))

def get[T](key: String): KVStore[Option[T]] =
  liftF[KVStoreA, Option[T]](Get[T](key))

def delete(key: String): KVStore[Unit] =
  liftF(Delete(key))

def update[T](key: String, f: T => T): KVStore[Unit] =
  for {
    vMaybe <- get[T](key)
    _ <- vMaybe.map(v => put[T](key, f(v))).getOrElse(Free.pure())
  } yield ()

def program1: KVStore[Option[Int]] =
  for {
    _ <- put("wild-cats", 2)
    _ <- update[Int]("wild-cats", _ + 12)
    _ <- put("tame-cats", 5)
    n <- get[Int]("wild-cats")
    _ <- delete("tame-cats")
  } yield n

def impureCompiler: KVStoreA ~> Id = new (KVStoreA ~> Id) {
  val kvs = mutable.Map.empty[String, Any]

  def apply[A](fa: KVStoreA[A]): Id[A] = fa match {
    case Put(key, value) =>
      println(s"Put($key, $value)")
      kvs(key) = value
      ()
    case Get(key) =>
      println(s"Get($key)")
      kvs.get(key).map(_.asInstanceOf[A])
    case Delete(key) =>
      println(s"Delete($key)")
      kvs.remove(key)
      ()
  }
}

val impureResult: Option[Int] = program1.foldMap(impureCompiler)

type KVStoreState[A] = State[Map[String, Any], A]
val pureCompiler: KVStoreA ~> KVStoreState = new (KVStoreA ~> KVStoreState) {
  def apply[A](fa: KVStoreA[A]): KVStoreState[A] = fa match {
    case Put(key, value) => State.modify(_.updated(key, value))
    case Get(key) => State.inspect(_.get(key).map(_.asInstanceOf[A]))
    case Delete(key) => State.modify(_  - key)
  }
}

val pureResult: (Map[String, Any], Option[Int]) = program1.foldMap(pureCompiler).run(Map.empty).value