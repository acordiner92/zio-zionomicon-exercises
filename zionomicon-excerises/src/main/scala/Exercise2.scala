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
