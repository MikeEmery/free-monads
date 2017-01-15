import cats.data.Coproduct
import cats.free.Free
import cats.free.Inject
import cats.{Id, ~>}

case class Degree(private val d: Int) {
  val value: Int = d % 360
}
case class Position(x: Double, y: Double, heading: Degree)

sealed trait Instruction[A]
case class Forward(position: Position, length: Int) extends Instruction[Position]
case class Backward(position: Position, length: Int) extends Instruction[Position]
case class RotateLeft(position: Position, degree: Degree) extends Instruction[Position]
case class RotateRight(position: Position, degree: Degree) extends Instruction[Position]
case class ShowPosition(position: Position) extends Instruction[Unit]

class Moves[F[_]](implicit I: Inject[Instruction, F]) {
  def forward(pos: Position, l: Int): Free[F, Position] = Free.inject[Instruction, F](Forward(pos, l))
  def backward(pos: Position, l: Int): Free[F, Position] = Free.inject[Instruction, F](Backward(pos, l))
  def left(pos: Position, d: Degree): Free[F, Position] = Free.inject[Instruction, F](RotateLeft(pos, d))
  def right(pos: Position, d: Degree): Free[F, Position] = Free.inject[Instruction, F](RotateRight(pos, d))
  def showPosition(pos: Position): Free[F, Unit] = Free.inject[Instruction, F](ShowPosition(pos))
}

object Moves {
  implicit def moves[F[_]](implicit I: Inject[Instruction, F]): Moves[F] = new Moves[F]
}

object Computations {
  def forward(pos: Position, l: Int): Position = pos.copy(
    x = pos.x + l * math.cos(pos.heading.value * math.Pi/180.0),
    y = pos.y + l * math.sin(pos.heading.value * math.Pi/180.0)
  )

  def backward(pos: Position, l: Int): Position = pos.copy(
    x = pos.x - l*math.cos(pos.heading.value * math.Pi/180.0),
    y = pos.y - l*math.sin(pos.heading.value * math.Pi/180.0)
  )

  def left(pos: Position, d: Degree): Position = pos.copy(
    heading = Degree(pos.heading.value + d.value)
  )

  def right(pos: Position, d: Degree): Position = pos.copy(
    heading = Degree(pos.heading.value - d.value)
  )
}

object InterpreterId extends (Instruction ~> Id) {
  import Computations._
  override def apply[A](fa: Instruction[A]): Id[A] = fa match {
    case Forward(p, length) => forward(p, length)
    case Backward(p, length) => backward(p, length)
    case RotateLeft(p, degree) => left(p, degree)
    case RotateRight(p, degree) => right(p, degree)
    case ShowPosition(p) => println(s"Showing position $p")
  }
}

val startPosition = Position(0.0, 0.0, Degree(0))

sealed trait PencilInstruction[A]
case class PencilUp(position: Position) extends PencilInstruction[Unit]
case class PencilDown(position: Position) extends PencilInstruction[Unit]

class PencilActions[F[_]](implicit I: Inject[PencilInstruction, F]) {
  def pencilUp(pos: Position): Free[F, Unit] = Free.inject[PencilInstruction, F](PencilUp(pos))
  def pencilDown(pos: Position): Free[F, Unit] = Free.inject[PencilInstruction, F](PencilDown(pos))
}

object PencilActions {
  implicit def pencilActions[F[_]](implicit I: Inject[PencilInstruction, F]): PencilActions[F] = new PencilActions[F]
}

type LogoApp[A] = Coproduct[Instruction, PencilInstruction, A]

def program3(implicit M: Moves[LogoApp], P: PencilActions[LogoApp]): (Position => Free[LogoApp, Unit]) = {
  start: Position =>
    for {
      p1 <- M.forward(start, 10)
      p2 <- M.right(p1, Degree(10))
      _ <- P.pencilUp(p2)
      p3 <- M.forward(p2, 10)
      _ <- P.pencilDown(p3)
      p4 <- M.backward(p3, 20)
      - <- M.showPosition(p4)
    } yield ()
}

object PenInterpreterId extends (PencilInstruction ~> Id) {
  def apply[A](fa: PencilInstruction[A]): Id[A] = fa match {
    case PencilUp(p) => println(s"stop drawing at position $p")
    case PencilDown(p) => println(s"start drawing at position $p")
  }
}

val interpreter: LogoApp ~> Id = InterpreterId or PenInterpreterId
program3.apply(startPosition).foldMap(interpreter)