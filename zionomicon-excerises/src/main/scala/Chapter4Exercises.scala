import zio.*
import java.io.IOException

// 1. Using the appropriate effect constructor, fix the following function so that
// it no longer fails with defects when executed. Make a note of how the
// inferred return type for the function changes.

def failWithMessage(string: String) =
  ZIO.fail(new Error(string))

// 2. Using the ZIO#foldCauseZIO operator and the Cause#defects method,
// implement the following function. This function should take the effect,
// inspect defects, and if a suitable defect is found, it should recover from
// the error with the help of the specified function, which generates a new
// success value for such a defect.

def recoverFromSomeDefects[R, E, A](zio: ZIO[R, E, A])(
    f: Throwable => Option[A]
): ZIO[R, E, A] =
  zio.foldCauseZIO(
    e => {
      val result = e.defects.map(f(_)).filter(_.isDefined).head
      if (result.isDefined) {
        ZIO.succeed(result.get)
      } else {
        ZIO.unsandbox(ZIO.fail(e))
      }
    },
    x => ZIO.succeed(x)
  )

// 3. Using the ZIO#foldCauseZIO operator and the Cause#prettyPrint
// method, implement an operator that takes an effect, and returns a new
// effect that logs any failures of the original effect (including errors and
// defects), without changing its failure or success value
def logFailures[R, E, A](zio: ZIO[R, E, A]): ZIO[R, E, A] =
//   zio.tapCause(f => ZIO.succeed(print(f.prettyPrint)))
  zio.foldCauseZIO(
    f => {
      print(f.prettyPrint)
      zio
    },
    _ => zio
  )

// 4. Using the ZIO#foldCauseZIO method, which “runs” an effect to an Exit
// value, implement the following function, which will execute the specified
// effect on any failure at all:
def onAnyFailure[R, E, A](
    zio: ZIO[R, E, A],
    handler: ZIO[R, E, Any]
): ZIO[R, E, A] =
  handler.foldCauseZIO(x => zio, _ => zio)

// 5. Using the ZIO#refineOrDie method, implement the ioException func-
// tion, which refines the error channel to only include the IOException
// error.
def ioException[R, A](
    zio: ZIO[R, Throwable, A]
): ZIO[R, java.io.IOException, A] =
  zio.refineOrDie { case e: IOException => e }

// 6. Using the ZIO#refineToOrDie method, narrow the error type of the fol-
// lowing effect to just NumberFormatException.
val parseNumber: ZIO[Any, Throwable, Int] =
  ZIO.attempt("foo".toInt).refineToOrDie[NumberFormatException]

// 7. Using the ZIO#foldZIO method, implement the following two functions,
// which make working with Either values easier, by shifting the unexpected
// case into the error channel (and reversing this shifting).
def left[R, E, A, B](
    zio: ZIO[R, E, Either[A, B]]
): ZIO[R, Either[E, B], A] =
  zio.foldZIO(
    x => ZIO.fail(Left(x)),
    y =>
      y match {
        case Right(b: B) => ZIO.fail(Right(b))
        case Left(a: A)  => ZIO.succeed(a)
      }
  )

def unleft[R, E, A, B](
    zio: ZIO[R, Either[E, B], A]
): ZIO[R, E, Either[A, B]] =
  zio.foldZIO(
    x =>
      x match {
        case Right(b: B) => ZIO.succeed(Right(b))
        case Left(a: E)  => ZIO.fail(a)
      },
    y => ZIO.succeed(Left(y))
  )

// 8. Using the ZIO#foldZIO method, implement the following two functions,
// which make working with Either values easier, by shifting the unexpected
// case into the error channel (and reversing this shifting).
def right[R, E, A, B](
    zio: ZIO[R, E, Either[A, B]]
): ZIO[R, Either[E, A], B] =
  zio.foldZIO(
    y => ZIO.fail(Left(y)),
    y =>
      y match {
        case Right(b: B) => ZIO.succeed(b)
        case Left(a: A)  => ZIO.fail(Right(a))
      }
  )

def unright[R, E, A, B](
    zio: ZIO[R, Either[E, A], B]
): ZIO[R, E, Either[A, B]] =
  zio.foldZIO(
    x =>
      x match {
        case Right(b: A) => ZIO.succeed(Left(b))
        case Left(a: E)  => ZIO.fail(a)
      },
    y => ZIO.succeed(Right(y))
  )

// 9. Using the ZIO#sandbox method, implement the following function.
def catchAllCause[R, E1, E2, A](
    zio: ZIO[R, E1, A],
    handler: Cause[E1] => ZIO[R, E2, A]
): ZIO[R, E2, A] =
  zio.sandbox.foldZIO(handler(_), ZIO.succeed(_))

// 10. Using the ZIO#foldCauseZIO method, implement the following function.
def catchAllCause2[R, E1, E2, A](
    zio: ZIO[R, E1, A],
    handler: Cause[E1] => ZIO[R, E2, A]
): ZIO[R, E2, A] =
  zio.foldCauseZIO(handler(_), ZIO.succeed(_))
