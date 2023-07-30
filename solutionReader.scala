package trianglepuzzle

import java.io.{BufferedReader, FileNotFoundException, FileReader, IOException, Reader}
import scala.io.Source
import scala.util.Random

object solutionReader:

  def readFile: String =
    var text: String = ""
      // opens character stream
      val fileIn = FileReader("src/main/scala/trianglepuzzle/solutions.txt")

       // buffered stream
      val linesIn = BufferedReader(fileIn)

        try
         // stream is now open so we must close it in the finally block
         // if the readLine is null then the file end has been reached
         val random_line_number: Int = Random.nextInt(5)
         var oneLine: String = linesIn.readLine()
         var lines_read: Int = 0
         while oneLine != null do
             if lines_read == 9 then
               text = oneLine
             oneLine = linesIn.readLine()
             lines_read += 1

        finally
         linesIn.close()
         fileIn.close()
      end try
    text












