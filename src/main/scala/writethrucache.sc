import cats.data.Coproduct
import cats.free.{Coyoneda, Free, Inject}

object KvStore {
  sealed trait KvOp[A]
  case class Get[T](key: String) extends KvOp[Option[T]]
  case class Put[T](key: String, value: T) extends KvOp[Unit]
  case class Delete[T](key: String) extends KvOp[Unit]
}

object CacheStore {
  sealed trait CacheOp[A]
  case class Get[T](key: String) extends CacheOp[Option[T]]
  case class Put[T](key: String, value: T) extends CacheOp[Unit]
  case class Delete[T](key: String) extends CacheOp[Unit]
}

type WriteThruCache[A] = Coproduct[KvStore.KvOp, CacheStore.CacheOp, A]

class KvOps[F[_]](implicit I: Inject[KvStore.KvOp, F]) {
  import KvStore._
  def get[T](key: String): Free[F, Option[T]] = Free.inject[KvOp, F](Get(key))
  def put[T](key: String, value: T): Free[F, Unit] = Free.inject[KvOp, F](Put(key, value))
  def delete[T](key: String): Free[F, Unit] = Free.inject[KvOp, F](Delete(key))
}

object KvOps {
  implicit def kvOps[F[_]](implicit I: Inject[KvStore.KvOp, F]): KvOps[F] = new KvOps[F]
}

class CacheOps[F[_]](implicit I: Inject[CacheStore.CacheOp, F]) {
  import CacheStore._
  def get[T](key: String): Free[F, Option[T]] = Free.inject[CacheOp, F](Get(key))
  def put[T](key: String, value: T): Free[F, Unit] = Free.inject[CacheOp, F](Put(key, value))
  def delete[T](key: String): Free[F, Unit] = Free.inject[CacheOp, F](Delete(key))
}

object CacheOps {
  implicit def cacheOps[F[_]](implicit I: Inject[CacheStore.CacheOp, F]): CacheOps[F] = new CacheOps[F]
}

def valueWriteOperation[T](implicit Kv: KvOps[WriteThruCache], Cache: CacheOps[WriteThruCache]): ((String, T) => Free[WriteThruCache, Unit]) = {
  (key: String, value: T)  =>
    for {
      _ <- Kv.put(key, value)
      _ <- Cache.delete(key)
      _ <- Cache.put(key, value)
    } yield ()
}

def withFallback[A[_], T](loadedValue: Option[T], fallback: => Free[A, Option[T]]): Free[A, Option[T]] = {
  if(loadedValue.isDefined) {
    Free.pure[A, Option[T]](loadedValue)
  } else {
    fallback
  }
}

def valueGetOperation[T](implicit Kv: KvOps[WriteThruCache], Cache: CacheOps[WriteThruCache]): ((String) => Free[WriteThruCache, Option[T]]) = {
  (key: String) =>
    for {
      cachedOption <- Cache.get[T](key)
      actualValue <- withFallback[WriteThruCache, T](cachedOption, fallback = Kv.get[T](key))
    } yield actualValue
}