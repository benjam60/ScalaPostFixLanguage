package ScalaPostFixLanguage

import scala.collection.immutable.Stack
import scala.collection.immutable.Queue

object PostFixInterpreter {

	def run(programText : String) : String = {
		val tokens = programText.split("\\s+").toList
		runProgram(tokens)
	}

	def runProgram(rest : List[String]) : String =
		if (!rest.contains(addOperatorToken)) rest.mkString(" ")
	  else {
			val (cmd, rest1) = parseCommand(rest)
			val result = executeCommand(cmd)
			runProgram(result::rest1)
		}

	def parseCommand(tokens : List[String]) : (Command, List[String]) = {
		val argsToken = tokens.takeWhile(_ != addOperatorToken)
		val operatorToken = tokens.dropWhile(_ != addOperatorToken).head
		val restOfCommands = tokens.dropWhile(_ != addOperatorToken).drop(1)
		(Command(argsToken, OperatorToken(operatorToken)), restOfCommands)
	}

	def executeCommand(command: Command) : String = {
		""
	}

	val addOperatorToken = "+"

	case class Command(arguments : List[String], operatorToken: OperatorToken)

	case class OperatorToken(get : String) extends AnyVal

}
