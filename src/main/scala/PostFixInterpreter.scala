package ScalaPostFixLanguage

object PostFixInterpreter {

	sealed trait ParsedValue
	case class ParsedInt(get : Int) extends ParsedValue
	case class ParsedCommand(get : String) extends ParsedValue
	case class ParsedCommandSequence(get : String) extends ParsedValue

	sealed trait StackValue
	case class IntValue(get : Int) extends StackValue
	case class ExecutableSequence(get : String) extends StackValue

	def run(program : String) : Int = {
		val parsedProgram = parse(program)
		val stack = List[StackValue]()
		runProgram(parsedProgram, stack)
	}

	private def runProgram(parsedProgram : List[ParsedValue], stack : List[StackValue]) : Int = parsedProgram match {
		case Nil => stack.head.asInstanceOf[IntValue].get
		case ParsedInt(value):: restOfParsed => runProgram(restOfParsed, IntValue(value)::stack)
		case ParsedCommand(cmd):: restOfParsed => runProgram(restOfParsed, runCommand(cmd, stack))
		case ParsedCommandSequence(cmdSeq):: restOfParsed => runProgram(restOfParsed, ExecutableSequence(cmdSeq) :: stack)
	}

	private def runCommand(cmd : String, stack : List[StackValue]) : List[StackValue] =
		if (List("add", "swap").contains(cmd)) {
			stack match {
				case ExecutableSequence(first)::ExecutableSequence(second)::_ => executeBinaryCommand(cmd, run(first), run(second), stack)
				case IntValue(first)::ExecutableSequence(second)::_ => executeBinaryCommand(cmd, first, run(second), stack)
				case ExecutableSequence(first)::IntValue(second)::_ => executeBinaryCommand(cmd, run(first), second, stack)
				case IntValue(first)::IntValue(second)::_ => executeBinaryCommand(cmd, first, second, stack)
			}
		}
	else throw new RuntimeException("Not implemented")

	private def executeBinaryCommand(cmd : String, firstArg : Int, secondArg : Int,
	                                 stack : List[StackValue]) : List[StackValue] = cmd match {
		case "add" => IntValue(firstArg + secondArg)::stack
		case "swap" => IntValue(secondArg)::IntValue(firstArg)::stack
		case _ => throw new RuntimeException("Not implemented")
	}

	def parse(program : String): List[ParsedValue] = {
		val removeHeader = stripWrappingParens(program).replaceFirst("postfix ", "")
		val topLevelCmdsAndSeq = breakUp(removeHeader)
		topLevelCmdsAndSeq.filter(_ != "")flatMap { elt =>
			if (elt(0) == '(')
				List(ParsedCommandSequence(elt))
			else
				parseWithoutCommandSequence(elt)  }
	}

	private def parseWithoutCommandSequence(input : String) =
		input.trim.split("\\s+").map { elt =>
			if (elt(0).isDigit) ParsedInt(elt(0).asDigit) else ParsedCommand(elt) }.toList

	private def breakUp(input : String) : List[String] = {
		val maybeOpenParensIndex = input.indexOf('(')
		val NotFound = -1
		maybeOpenParensIndex match {
			case openIndex if openIndex == 0 =>
				val closingIndex = getClosingParenethesisIndex(input)
				val (executableSequence, rest) = input.splitAt(closingIndex + 1)
				List(executableSequence) ++ breakUp(rest)
			case openIndex if openIndex > 0 =>
				val (cmds, cmdseqandrest) = input.splitAt(openIndex)
				val closingIndex = getClosingParenethesisIndex(cmdseqandrest)
				val (cmdseq, rest) = cmdseqandrest.splitAt(closingIndex + 1)
				List(cmds.trim, cmdseq.trim) ++ breakUp(rest.trim)
			case NotFound => List(input)
		}
	}

	private def getClosingParenethesisIndex(input : String) : Int = {

		def traverse(input : String, currIndex : Int, numOpen : Int, numClosed : Int) : Int = {
			if (numOpen == numClosed) { currIndex }
			else {
				val nextIndex = currIndex + 1
				val nextChar = input(nextIndex)
				val restInput = input.drop(0)
				if (nextChar == '(') traverse(restInput, nextIndex, numOpen + 1, numClosed)
				else if (nextChar == ')') traverse(restInput, nextIndex, numOpen, numClosed + 1)
				else traverse(restInput, nextIndex, numOpen, numClosed)
			}
		}

		traverse(input.drop(0), currIndex = 0, numOpen = 1, numClosed = 0)

	}
	private def stripWrappingParens(input : String) : String = input.drop(1).dropRight(1)

}
