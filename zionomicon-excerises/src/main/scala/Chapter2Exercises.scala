import zio.*

// 1. Implement a ZIO version of the function readFile by using the
// ZIO.attempt constructor.
def readFileZio(file: String): Task[String] = {
  val source = scala.io.Source.fromFile(file)
  ZIO.attempt(source.getLines.mkString)
}

// 2. Implement a ZIO version of the function writeFile by using the
// ZIO.attempt constructor
def writeFile(file: String, text: String): ZIO[Any, Throwable, Unit] = {
  import java.io._

  val pw = new PrintWriter(new File(file))
  ZIO.attempt(pw.write(text))
}

// 3. Using the flatMap method of ZIO effects, together with the readFileZio
// and writeFileZio functions that you wrote, implement a ZIO version of
// the function copyFile.
def copyFileZio(source: String, dest: String) = {
  readFileZio(source).flatMap(contents => writeFile(dest, contents))
}

// 4. Rewrite the following ZIO code that uses flatMap into a for comprehension.
def printLine(line: String) = ZIO.attempt(println(line))

def methodQuestion4() = {
  val readLine = ZIO.attempt(scala.io.StdIn.readLine())
  for
    _ <- printLine("What is your name?")
    name <- readLine
    _ <- printLine(s"Hello, ${name}!")
  yield ()

}

// 5. Rewrite the following ZIO code that uses flatMap into a for comprehension.

def methodQuestion5() = {
  val random = ZIO.attempt(scala.util.Random.nextInt(3) + 1)
  val readLine = ZIO.attempt(scala.io.StdIn.readLine())
  for {
    int <- random
    _ <- printLine("Guess a number from 1 to 3:")
    num <- readLine
    _ <-
      if (num == int.toString) printLine("You guessed right!")
      else printLine(s"You guessed wrong, the number was $int!")
  } yield ()
}

// 6. Implement the zipWith function in terms of the toy model of a ZIO
// effect. The function should return an effect that sequentially composes
// the specified effects, merging their results with the specified user-defined
// function.
final case class NIO[-R, +E, +A](run: R => Either[E, A])

def zipWith[R, E, A, B, C](self: NIO[R, E, A], that: NIO[R, E, B])(
    f: (A, B) => C
): NIO[R, E, C] = NIO(r =>
  val a = self.run(r)
  val b = that.run(r)
  a.fold(e => Left(e), x => b.fold(e => Left(e), y => Right(f(x, y))))
)

// 7. Implement the collectAll function in terms of the toy model of a ZIO
// effect. The function should return an effect that sequentially collects the
// results of the specified collection of effects.
def collectAll[R, E, A](
    in: Iterable[NIO[R, E, A]]
): NIO[R, E, List[A]] =
  NIO(r => collectAll(in).run(r))

// 8. Implement the foreach function in terms of the toy model of a ZIO effect.
// The function should return an effect that sequentially runs the specified
// function on every element of the specified collection.
def foreach[R, E, A, B](
    in: Iterable[A]
)(f: A => NIO[R, E, B]): NIO[R, E, List[B]] =
  NIO(r => foreach(in)(f).run(r))

// 9. Implement the orElse function in terms of the toy model of a ZIO effect.
// The function should return an effect that tries the left hand side, but if
// that effect fails, it will fallback to the effect on the right hand side.
def orElse[R, E1, E2, A](
    self: NIO[R, E1, A],
    that: NIO[R, E2, A]
): NIO[R, E2, A] =
  NIO(r => self.run(r).fold(_ => that.run(r), a => Right(a)))

// 10. Using the following code as a foundation, write a ZIO application that
// prints out the contents of whatever files are passed into the program as
// command-line arguments. You should use the function readFileZio that
// you developed in these exercises, as well as ZIO.foreach.
object Cat extends ZIOApp {
  def run(commandLineArguments: List[String]) =
    ZIO
      .foreach(commandLineArguments) { fileName =>
        for
          fileData <- readFileZio(fileName)
          _ <- printLine(fileData)
        yield ()
      }
      .exitCode
}

// 11. Using ZIO.fail and ZIO.succeed, implement the following function,
// which converts an Either into a ZIO effect:
def eitherToZIO[E, A](either: Either[E, A]): ZIO[Any, E, A] =
  either.fold(ZIO.fail, ZIO.succeed)

// 12. Using ZIO.fail and ZIO.succeed, implement the following function,
// which converts a List into a ZIO effect, by looking at the head element in
// the list and ignoring the rest of the elements.

def listToZIO[A](list: List[A]): ZIO[Any, None.type, A] =
  val option = list.headOption
  option match {
    case Some(i) => ZIO.succeed(i)
    case None    => ZIO.fail(None)
  }

// 13. Using ZIO.succeed, convert the following procedural function into a ZIO
// function:
def currentTime(): Long = java.lang.System.currentTimeMillis()

lazy val currentTimeZIO: ZIO[Any, Nothing, Long] = ZIO.succeed(currentTime())

// 14. Using ZIO.async, convert the following asynchronous, callback-based func-
// tion into a ZIO function:
def getCacheValue(
    key: String,
    onSuccess: String => Unit,
    onFailure: Throwable => Unit
): Unit =
  ???

def getCacheValueZio(key: String): ZIO[Any, Throwable, String] =
  ZIO.async { callback =>
    getCacheValue(
      key,
      value => callback(ZIO.succeed(value)),
      error => callback(ZIO.fail(error))
    )
  }

// 15. Using ZIO.async, convert the following asynchronous, callback-based func-
// tion into a ZIO function:
trait User
def saveUserRecord(
    user: User,
    onSuccess: () => Unit,
    onFailure: Throwable => Unit
): Unit =
  ???
def saveUserRecordZio(user: User): ZIO[Any, Throwable, Unit] =
  ZIO.async { callback =>
    saveUserRecord(
      user,
      () => callback(ZIO.unit),
      error => callback(ZIO.fail(error))
    )
  }

// 16. Using ZIO.fromFuture, convert the following code to ZIO:
import scala.concurrent.{ExecutionContext, Future}
trait Query
trait Result
def doQuery(query: Query)(implicit ec: ExecutionContext): Future[Result] = ???

def doQueryZio(query: Query): ZIO[Any, Throwable, Result] =
  Task.fromFuture(doQuery(query))

// 17. Using the Console, write a little program that asks the user what their
// name is, and then prints it out to them with a greeting.
import zio.{App => ZIOApp}
object HelloHuman extends ZIOApp {
  def run(args: List[String]) = {
    for
      _ <- Console.printLine("What is your name?")
      name <- Console.readLine
      _ <- Console.printLine(s"Welcome ${name}")
    yield ()
  }.exitCode
}

// 18. Using the Console and Random services in ZIO, write a little program that
// asks the user to guess a randomly chosen number between 1 and 3, and
// prints out if they were correct or not.
import zio._
object NumberGuessing extends ZIOApp {
  def run(args: List[String]) = {
    for
      num <- Random.nextIntBetween(1, 3)
      _ <- Console.printLine("Guess a number between 1 to 3")
      guess <- Console.readLine
      guessInt <- ZIO.attempt(guess.toInt)
      _ <-
        if (num == guessInt) then Console.printLine("You guess correct")
        else Console.printLine("You guess wrong")
    yield ()
  }.provideLayer(Console.live ++ Random.live).exitCode
}

// 19. Using the Console service and recursion, write a function that will repeat-
// edly read input from the console until the specified user-defined function
// evaluates to true on the input.
import java.io.IOException
def readUntil(
    acceptInput: String => Boolean
): ZIO[Console, IOException, String] =
  for {
    input <- Console.readLine
    value <-
      if (!acceptInput(input)) then readUntil(acceptInput)
      else ZIO.succeed(input)
  } yield value

// 20. Using recursion, write a function that will continue evaluating the specified
// effect, until the specified user-defined function evaluates to true on the
// output of the effect.
def doWhile[R, E, A](
    body: ZIO[R, E, A]
)(condition: A => Boolean): ZIO[R, E, A] =
  for
    a <- body
    value <-
      if (!condition(a)) then doWhile(body)(condition)
      else ZIO.succeed(a)
  yield a
